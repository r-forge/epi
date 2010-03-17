splitMatrix <- function (x, f, drop=FALSE) {
    lapply(split(seq_len(nrow(x)), f, drop = drop),
           function(ind) x[ind, , drop = FALSE])
}

fixEvent <- function(event)
{
    ### Convert outcome in clogit model to 0/1 binary coding
    
    if (any(is.na(event)))
        stop("Event contains missing values")

    if (is.logical(event)) {
        status <- is.numeric(event)
    }
    else if (is.numeric(event)) {
        status <- if (max(event) == 2) event - 1  else event
        temp <- (status == 0 | status == 1)
        if (!all(temp)) {
            warning("If outcome is numeric then it must be coded 0/1 or 1/2")
        }
    }
    else if (is.factor(event)) {
        if (nlevels(event) != 2)
            stop("If outcome is a factor then it must have 2 levels")
        status <- event == levels(event)[2]
    }
    return(as.integer(status))
}  

fitClogit <- function(X, y, strata, init, iter.max, eps, toler.chol, ...)
{
    ## Safe wrapper around the C function "clogit" that ensures all
    ## arguments have the correct type and storage mode.
    
    y <- fixEvent(y)
    if (!is.matrix(X)) {
        X <- as.matrix(X)
    }
    if (!is.real(X)) {
        X <- matrix(as.real(X), nrow(X), ncol(X))
    }
    
    ## Split into strata
    Xsplit <- splitMatrix(X, strata, drop=TRUE)
    ysplit <- split(y, strata, drop=TRUE)

    .Call("clogit", Xsplit, ysplit, as.double(init),
          as.integer(iter.max), as.double(eps), as.double(toler.chol),
          PACKAGE="Epi")
}

clogistic <- function (formula, strata, data, weights, subset, na.action,
                       init, offset, model = TRUE, x = FALSE, y = TRUE,
                       contrasts = NULL, iter.max=20, eps=1e-9,
                       toler.chol = sqrt(.Machine$double.eps)) 
{
    ## User interface, edited version of glm
    
    call <- match.call()
    if (missing(data)) 
        data <- environment(formula)
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data", "subset", "weights",
                 "na.action", "offset", "strata"), names(mf), 0L)
    mf <- mf[c(1, m)]
    mf$drop.unused.levels <- TRUE
    mf[[1L]] <- as.name("model.frame")
    mf <- eval(mf, parent.frame())

    mt <- attr(mf, "terms")
    Y <- model.response(mf, "any")
    if (length(dim(Y)) == 1L) {
        nm <- rownames(Y)
        dim(Y) <- NULL
        if (!is.null(nm)) 
            names(Y) <- nm
    }
    X <- if (!is.empty.model(mt)) 
        model.matrix(mt, mf, contrasts)
    else matrix(, NROW(Y), 0L)
    weights <- as.vector(model.weights(mf))
    if (!is.null(weights) && !is.numeric(weights)) 
        stop("'weights' must be a numeric vector")
    if (!is.null(weights) && any(weights < 0)) 
        stop("negative weights not allowed")
    offset <- as.vector(model.offset(mf))
    if (!is.null(offset)) {
        if (length(offset) != NROW(Y)) 
            stop(gettextf("number of offsets is %d should equal %d (number of observations)", 
                length(offset), NROW(Y)), domain = NA)
    }

    strata <- model.extract(mf, "strata")

    if (attr(mt, "intercept") > 0) {
        X <- X[,-1, drop=FALSE]
    }
    if (missing(init))
        init <- rep(0, ncol(X))
    fit <- fitClogit(X = X, y = Y, strata=strata, init=init,
                     toler.chol=toler.chol, eps=eps, iter.max=iter.max)
    if (fit$flag <= 0) {
        error("Information matrix is not positive definite")
    }
    else if (fit$flag == 1000) {
        warning("Iteration limit exceeded")
    }

    nvar <- length(init)
    which.sing <- if (fit$flag < nvar) {
        diag(fit$var)==0
    } else {
        rep(FALSE, nvar)
    }
    fit$coefficients[which.sing] <- NA
    
    ## Add back in parameter names
    cfnames <- colnames(X)
    names(fit$coefficients) <- cfnames
    dimnames(fit$var) <- list(cfnames, cfnames)
    
    if (model) 
        fit$model <- mf
    fit$na.action <- attr(mf, "na.action")
    if (x) 
        fit$x <- X
    if (!y) 
        fit$y <- NULL
    fit <- c(fit, list(call = call, formula = formula, terms = mt, 
        data = data, offset = offset, control = NULL, method = NULL, 
        contrasts = attr(X, "contrasts"), xlevels = .getXlevels(mt, 
            mf)))
    class(fit) <- c("clogistic")
    fit
}

coef.clogistic <- function(object,...) { object$coefficients }

vcov.clogistic <- function(object, ...) { object$var }

print.clogistic <- function (x, digits = max(options()$digits - 4, 3), ...) 
{
    ## Print method for clogistic objects, edited from print.coxph
    
    cat("\nCall: ", deparse(x$call), "\n\n", sep="\n")

    savedig <- options(digits = digits)
    on.exit(options(savedig))
    coef <- coef.clogistic(x)
    se <- sqrt(diag(vcov.clogistic(x)))
    if (is.null(coef) | is.null(se)) 
        stop("Input is not valid")

    coefmat <- cbind(coef, exp(coef), se, coef/se,
                     signif(1 - pchisq((coef/se)^2, 1), digits - 1))
    dimnames(coefmat) <- list(names(coef),
                              c("coef", "exp(coef)", "se(coef)", "z", "p"))
    cat("\n")
    prmatrix(coefmat)
    logtest <- -2 * (x$loglik[1] - x$loglik[2])
    if (is.null(x$df)) 
        df <- sum(!is.na(coef))
    else df <- round(sum(x$df), 2)
    cat("\n")
    cat("Likelihood ratio test=", format(round(logtest, 2)), 
        "  on ", df, " df,", " p=", format(1 - pchisq(logtest, 
                                                      df)), sep = "")
    cat("\n")
    invisible()
}
