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
    return(status)
}  

fitClogit <- function(X, y, strata, weights, init, offset, ...)
{
    m <- ncol(X)
    M <- 1 + m + m*m

    y <- fixEvent(y)
    
    ## Centre predictor variables about the mean
    ## FIXME: we can do this within strata
    X <- sweep(X, 2, apply(X, 2, mean))
    
    if (missing(strata)) {
        stop("strata missing")
    }
    ## Split into strata
    X <- splitMatrix(X, strata, drop=TRUE)
    y <- split(y, strata, drop=TRUE)
    
    ## Objective function for nlm uses lexical scoping
    objf <- function(beta) {
        .Call("neg_cloglik", X, y, beta, PACKAGE="Epi")
    }

    nlm.out <- nlm(objf, init, hessian=TRUE, ...)
    nlm.out$nullmodel <- objf(rep(0,m))
    return(nlm.out)
}

clogistic <- function (formula, strata, data, weights, subset, na.action,
                    init, offset, model = TRUE, x = FALSE, y = TRUE,
                    contrasts = NULL, ...) 
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
    fit <- fitClogit(X = X, y = Y, strata=strata, weights = weights,
                     offset = offset, init=init)
    ## Translate nlm results into the more familiar language of regression
    ## models
    fit <- list(coefficients=fit$estimate, var = solve(fit$hessian),
                loglik = c(-fit$nullmodel,-fit$minimum),
                score = -fit$gradient,
                iter = fit$iterations, code=fit$code)
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
    if (x$code > 2) {
        cat("Optimization in clogistic terminated with code ", x$code, "\n")
        cat("See nlm help page for details\n")
        return()
    }
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
