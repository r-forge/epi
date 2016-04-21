######################################################################
### estimation method
LCa.fit <-
function( data, A, P, D, Y,
         ref.b, ref.t,
         model = "P",
          npar = c(A = 6, B = 6, T = 6),
            VC = FALSE,
           eps = 1e-6,
         maxit = 100,
         quiet = TRUE )
{
# if dataframe supplied    
if(!missing(data))
  {
  if (length(match(c("A", "P", "D", "Y"), names(data))) != 4)
  stop("Data frame ", deparse(substitute(data)),
       " has columns:\n", names(data),
       "\nmust have variables:\n", "A (age), P (period), D (cases) and Y (person-time)")
  data <- data[,c("A","P","D","Y")]
  data <- data[complete.cases(data),]
  A <- data$A
  P <- data$P
  D <- data$D
  Y <- data$Y
  }
# if single vectors supplied
else
  {
  # check they are all there
  nm <- logical(4)
  nm[1] <- missing(A)
  nm[2] <- missing(P)
  nm[3] <- missing(D)
  nm[4] <- missing(Y)
  if (any(nm))
      stop("Variable", if (sum(nm) > 1)
          "s", paste(c(" A", " P", " D", " Y")[nm], collapse = ","),
          " missing from input")
  # and that they have the same length
  if( diff(range( lv <- c( length(A),
                           length(P),
                           length(D),
                           length(Y) ) )) != 0 )
      stop( "\nLengths of variables (", paste(paste(names(lv),
            lv, sep = ":"), collapse = ", "), ") are not the same." )
  }

# Period or Cohort interaction? - a degree of forgiveness provided 
Pm <- ( toupper(substr(model,1,1)) == "P" )
Tm <- if( Pm ) P else P-A
    
# Define knots
if( is.list(npar) )
  {  
a.kn <- if( length(npar$A)>1 ) npar$A else quantile( rep(A ,D), probs=(1:npar$A-0.5)/npar$A )
b.kn <- if( length(npar$B)>1 ) npar$B else quantile( rep(A ,D), probs=(1:npar$B-0.5)/npar$B )
t.kn <- if( length(npar$T)>1 ) npar$T else quantile( rep(Tm,D), probs=(1:npar$T-0.5)/npar$T )
  }
else
  { 
  if( is.null(names(npar)) ) names(npar) <- c("A","B","T")
  if( !identical(sort(names(npar)),c("A","B","T")) ) 
      stop( "If named, npar must have names A, B and T" )
a.kn <- quantile( rep(A ,D), probs=(1:npar["A"]-0.5)/npar["A"] )
b.kn <- quantile( rep(A ,D), probs=(1:npar["B"]-0.5)/npar["B"] )
t.kn <- quantile( rep(Tm,D), probs=(1:npar["T"]-0.5)/npar["T"] )
  }
    
# Reference points
if( missing(ref.b) ) ref.b <- median( rep(A ,D) )    
if( missing(ref.t) ) ref.t <- median( rep(Tm,D) )
    
# Coefficients for age-interaction at reference point
CB <- Ns( ref.b, knots=b.kn, intercept=TRUE )

# Main effects model
mat <- glm( D ~ -1 + Ns( A , knots=a.kn, intercept=TRUE ) + 
                     Ns( Tm, knots=t.kn, ref=ref.t),
            offset = log(Y),
            family = poisson )
    
# Terms prediction
pat <- predict( mat, type="terms" )

# iteration counter
nit <- 0    

# iterate till convergence
cond <- TRUE
while( cond )
  {  
nit <- nit+1

# iteration at age-interaction    
mb <- glm( D ~ -1 + Ns( A, knots=b.kn, intercept=TRUE ):pat[,2],
           offset = pat[,1] + log(Y),
           family = poisson )

# Get the age-interaction term only - remove period term, rescale to 1
ba <- as.vector(predict( mb, type="terms" ) / pat[,2] / ci.lin( mb, ctr.mat=CB)[,1])
ba[is.na(ba)] <- 0

# age-time model with fixed interaction    
mat <- glm( D ~ -1 + Ns( A , knots=a.kn, intercept=TRUE )
                   + Ns( Tm, knots=t.kn, ref=ref.t):ba,
            offset = log(Y),
            family = poisson )

# extract age and period terms      
pat <- predict( mat, type="terms" ) / cbind( 1, ba )
pat[is.na(pat)] <- 0

# convergence?
cond <- ( ( abs(mat$deviance-mb$deviance) > eps ) &
          ( nit < maxit ) )
if( !quiet ) cat( "Iteration", nit, mat$deviance, mb$deviance,
                                abs(mat$deviance- mb$deviance), "\n" )
  }

# Deviance and d.f --- note that the dimension of the model is one
# less than the no. of formal parameters due to the explicit
# normalization of the fit in model mb to be 1 at ref.b. Hence the +1.
dev <- mb$deviance
df  <- mat$df.null - (mb$df.null- mb$df.res +
                     mat$df.null-mat$df.res) + 1
cat( "LCa.fit convergence in ", nit,
     " iterations, deviance:", dev, "on", df, "d.f.\n")
    
# extract terms from final models after convergence
prb  <- predict( mb , type="terms", se.fit=TRUE )
prat <- predict( mat, type="terms", se.fit=TRUE )

# We only want estimates for each age/time once
a.pt <- sort(unique(A))
t.pt <- sort(unique(Tm))
ax <- ci.exp( mat, subset="A",
              ctr.mat=Ns( a.pt, knots=a.kn, intercept=TRUE ) )
kt <- ci.exp( mat, subset="Tm",
              ctr.mat=Ns(t.pt,knots=t.kn,ref=ref.t), Exp=FALSE )
bx <- ci.exp( mb,
              ctr.mat=Ns(a.pt,knots=b.kn,intercept=TRUE), Exp=FALSE )
# Label the estimates
rownames( ax ) <-    
rownames( bx ) <- a.pt    
rownames( kt ) <- t.pt

if( VC )
{    
if( !quiet ) cat("...computing Hessian by numerical differentiation...\n")
# simple scalars needed
na <- length( grep( "A" , names(coef(mat)) ) )
nt <- length( coef(mat) ) - na
nb <- length( coef(mb ) )
# fancy packages needed
# require( numDeriv )
# require( Matrix )

# We need the variance-covariance of the estimates as the 2nd
# derivative of the log-likelihood, D*log(lambda) - lambda*Y,
# or for eta=log(lambda), D*eta - exp(eta)*Y,
# assuming the sequence of parameters is ax, kt, bx
# Note that we cannot simplify because the model is non-linear in kt, bx
llik <-
function( parms )
{
ax <- Ns( A , knots=a.kn, intercept=TRUE ) %*% parms[      1:na]
kt <- Ns( Tm, knots=t.kn, ref=ref.t )      %*% parms[   na+1:nt]
bx <- Ns( A , knots=b.kn, intercept=TRUE ) %*% parms[nt+na+1:nb]
eta <- ax + bx*kt # the log-rate
sum( D*eta - exp(eta)*Y )
}

# Numerical calculation of the Hessian, and some more snappy names
ivar <- -numDeriv::hessian( llik, coef <- c( coef(mat),
                                             coef(mb ) ) )
rownames( ivar ) <-
colnames( ivar ) <- c( paste("ax",1:na,sep=""),
                       paste("kt",1:nt,sep=""),
                       paste("bx",1:nb,sep="") )

# Sometimes not quite positive definite, fix that after inversion
vcov <- Matrix::nearPD( solve( ivar ) )
vcov <- as.matrix( vcov$mat )
if( !quiet ) cat("DONE.\n")
}
    
# Finally output object
res <- list( Model = ifelse( Pm, "Period", "Cohort" ), 
                ax = ax, 
                kt = kt, 
                bx = bx, 
            mod.at = mat, 
             mod.b = mb,
              coef = if( VC ) coef else NULL, # no. 7
              vcov = if( VC ) vcov else NULL, # no. 8
              a.kn = a.kn, 
              b.kn = b.kn, 
              t.kn = t.kn,
             ref.b = ref.b,
             ref.t = ref.t,
          deviance = dev,
                df = df,
              iter = nit )
if( !VC ) res <- res[-(7:8)]    
class( res ) <- "LCa"
invisible( res ) 
}

######################################################################
### print method
print.LCa <-
function( x, ... )
cat(paste("Lee-Carter model using natural splines:\n",
    "  log(Rate) = a(Age) + b(Age)k(", x$Model, ")\nwith ",
    length(x$a.kn),", ",length(x$b.kn)-1," and ",length(x$t.kn),
    " parameters respectively (1 aliased).\n",
    "Deviance: ", round(x$deviance,3), " on ", x$df, " d.f.\n", sep="") ) 

######################################################################
### sumary method
summary.LCa <-
function( object, ... )
{
print( object )    
cat( "\nKnots used:\na(Age):\n" )
print( round(object$a.kn,2) )
cat( paste("b(Age), [b(",round(object$ref.b,2),")=1]:\n", sep="") )
print( round(object$b.kn,2) )
cat( paste("k(",object$Model,"), [k(",round(object$ref.t,2),")=0]:\n", sep="") )
print( round(object$t.kn,2) )
if( "vcov" %in% names(object) )
  {  
cf <- cbind( rbind( ci.lin(object$mod.at)[,1:2],
                    ci.lin(object$mod.b )[,1:2] ),
             sqrt( diag(object$vcov) ) )
colnames( cf )[-1] <- c("cond.se","joint.se")
  }
else
cf <- rbind( ci.lin(object$mod.at)[,1:2],
             ci.lin(object$mod.b )[,1:2] )
invisible( cf )
}

######################################################################
### plot method
plot.LCa <-
function( x,
       rnam = "Rates", ... )
{
# A small utility to exploit the structure of the effects    
plc <- 
function( x, ... ) matplot( as.numeric( rownames(x) ), x,
                              type="l", lty=1, lwd=c(3,1,1), ... )
opar <- par( mfrow=c(1,3), bty="n", las=1, mgp=c(3,1,0)/1.6 )
plc( x$ax, col="black", xlab="Age", ylab=rnam, log="y" )
rug( x$a.kn, lwd=2 )
plc( exp(x$kt), col="black", xlab="Date",
     ylab=paste(x$Model,"effect (RR)"), log="y" )
abline(h=1,v=x$ref.t)
rug( x$t.kn, lwd=2 )
plc( x$bx, col="black", xlab="Age",
     ylab=paste("Relative",x$Model,"log-effect multiplier") )
abline(h=1,v=x$ref.b)
rug( x$b.kn, lwd=2 )
on.exit( par(opar) )
}

######################################################################
### predict method
predict.LCa <-
function( object,
         newdata,
           level = 0.95,
           alpha = 1-level,
             sim = ( "vcov" %in% names(object) ),
             ... )
{
# age part of the interaction term
CB <- Ns( newdata$A, knots = object$b.kn, intercept = TRUE)
bx <- ci.lin( object$mod.b, ctr.mat=CB )[,1]

# contrast matrix for the main effect of age
CA <- Ns( newdata$A, knots = object$a.kn, intercept = TRUE)
ax <- ci.lin( object$mod.at, subset="a.kn", ctr.mat=CA )[,1] 

# define the time variable (period or cohort), contrast matrix for
# time and the time-term
Tm <- if( object$Model=="Period" ) newdata$P else newdata$P-newdata$A
CTm <- Ns( Tm, knots = object$t.kn, ref = object$ref.t )
kt <- ci.lin( object$mod.at, subset="t.kn", ctr.mat=CTm )[,1] 
    
# fitted values and ci.s from the mod.at model
pr0 <- ci.exp( object$mod.at, alpha=alpha, ctr.mat=cbind(CA,CTm*bx) )
# fitted values and ci.s from the mod.b model
pr0 <- cbind( pr0, ci.exp( object$mod.b, ctr.mat=CB*kt )*exp(ax) )
colnames( pr0 )[c(1,4)] <- c("at|b Est.","b|at Est.")
head( pr0 )    
   
# But this gives confidence intervals based on the conditional models,
# so if we want proper intervals we should simulate instead, using the
# posterior distribuion of all parameters, albeit under the slightly
# fishy assumption that the joint posterior is normal...  
if( sim )
  {
if( is.logical(sim) & sim ) sim <- 1000
# Check that there is a vcov component of the model
if( !( "vcov" %in% names(object) ) )
    warning(
    "No variance-covariance in LCa object, only conditional c.i.s available.\n",
    "Conditional s.e. s are used in calculation of prediction c.i.s\n"
    )  
else{   
# require( MASS )    
# using the parametric bootstrap based on the parameters and the
# (numerically computed) Hessian
eta <- NArray( list( pt = 1:nrow(pr0),
                     it = 1:sim ) )                      
parms <- MASS::mvrnorm( n = sim, 
                       mu = object$coef,
                    Sigma = object$vcov )
na <- ncol( CA )
nt <- ncol( CTm )
nb <- ncol( CB )
for( i in 1:sim )
   {
ax <- CA  %*% parms[i,      1:na]
kt <- CTm %*% parms[i,   na+1:nt]
bx <- CB  %*% parms[i,nt+na+1:nb]
eta[,i] = ax + bx*kt
   } 
pr.sim <- exp( t( apply( eta, 1, quantile,
                         probs=c(0.5,alpha/2,1-alpha/2), 
                         na.rm=TRUE ) ) )
colnames( pr.sim )[1] <- "Joint est."
return( pr.sim )
  }
}
else return( pr0 )    
}
