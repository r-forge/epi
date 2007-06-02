### Name: ci.cum
### Title: Compute cumulative sum of estimates.
### Aliases: ci.cum
### Keywords: models regression

### ** Examples

# Packages required for this example
library( splines )
library( survival )
data( lung )
par( mfrow=c(1,2) )

# Plot the Kaplan-meier-estimator
#
plot( survfit( Surv( time, status==2 ), data=lung ) )

# Cut the follow-up every 10 days
#
dx <- W.Lexis( exit=time, fail=(status==2), breaks=seq(0,1100,10), data=lung )
str( dx )

# Fit a Poisson model with a natural spline for the effect of time.
# Note that "Time" is the left endpoint of the interval in the dataframe dx.
#
MM <- ns( dx$Time, knots=c(50,100,200,400,700), intercept=TRUE )
mp <- glm( Fail ~ MM - 1 + offset(log(Exit-Entry)),
                  family=poisson, data=dx, eps=10^-8, maxit=25 )

# Contrast matrix to extract effects, i.e. matrix to multiply with the
# coefficients to produce the log-rates: unique rows of MM, in time order.
#
T.pt <- sort( unique( dx$Time ) )
T.wh <- match( T.pt, dx$Time )
Lambda <- ci.cum( mp, ctr.mat=MM[T.wh,], intl=diff(c(0,T.pt)) )

# Put the estimated survival function on top of the KM-estimator
#
matlines( c(0,T.pt[-1]), exp(-Lambda[,1:3]), lwd=c(3,1,1), lty=1, col="Red" )

# Extract and plot the fitted intensity function
#
lambda <- ci.lin( mp, ctr.mat=MM[T.wh,], Exp=TRUE )
matplot( T.pt, lambda[,5:7]*10^3, type="l", lwd=c(3,1,1), col="black", lty=1,
         log="y", ylim=c(0.2,20) )



