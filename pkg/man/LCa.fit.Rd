\name{LCa.fit}
\alias{LCa.fit}
\alias{print.LCa}
\alias{summary.LCa}
\alias{plot.LCa}
\alias{predict.LCa}
\title{
  Fit the Lee-Carter model for rates to arbitrarily shaped observations
  of rates in a Lexis diagram. 
}
\description{
  The Lee-Carter model is originally defined as a model for rates
  observed in A-sets (age by period) of a Lexis diagram, using one
  parameter per age(x) and period(t), as log(rate(x,t)) = a(x) +
  b(x)k(t).  This function use natural splines for a(), b() and k(),
  placing knots for each effect such that the number of events is the
  same between knots. It also has the option of using date of birth
  (cohort) as time (t).
  }
\usage{
LCa.fit( data, A, P, D, Y,
         ref.b, ref.t,
         model = "P",
          npar = c(A = 6, B = 6, T = 6),
            VC = FALSE,
           eps = 1e-06,
         maxit = 100,
         quiet = TRUE )
\method{print}{LCa}( x, ... )
\method{summary}{LCa}( object, ... )
\method{plot}{LCa}( x, rnam = "Rates", ... )
\method{predict}{LCa}( object, newdata,
                          level = 0.95,
                          alpha = 1-level,
                            sim = ( "vcov" \%in\% names(object) ),
                            ... )
  }
\arguments{
  \item{data}{
  A data frame. Must have columns \code{A}(age), \code{P}(period, that is
  calendar time), \code{D}(no. of events) and \code{Y}(person-time,
  exposure). Alternatively these four quantities can be given as
  separate vectors:
  }
  \item{A}{
  Vector of ages (midpoint of observation).
  }
  \item{P}{
  Vector of period (midpoint of observation).
  }
  \item{D}{
  Vector of no. of events.
  }
  \item{Y}{
  Vector of person-time. Demographers would say "exposure", bewildering epidemiologists.
  }
  \item{ref.b}{
  Reference age for the age-interaction term \code{b(x)} where \code{b(ref.b)=1}.
  }
  \item{ref.t}{
  Reference period for the time-interaction term \code{k(t)} where \code{k(ref.t)=0}.
  }
  \item{model}{
  Character, either \code{P} in which case time is calendar time or
  \code{C} in which case time is date of birth (cohort), \code{C=P-A}.
  }
  \item{npar}{
  Possibly named vector or list, with either number of knots or actual
  knots for each term. If unnamed, components are taken to be in the
  order (a,t,b). If a vector, the three integers indicate the number of
  knots for each term; these will be placed so that there is an equal
  number of events (\code{D}) between each, and half as many below the
  first and above the last knot. If \code{npar} is a list of scalars the
  behavior is the same. If \code{npar} is a list of vectors, there are
  taken as the knots for the natural splines.
  }
  \item{VC}{
  Logical. Should the variance-covariance matrix of the parameters be
  computed by numerical differentiation? See details.
  }
  \item{eps}{
  Convergence criterion for the deviance. See details.
  }
  \item{maxit}{
  Maximal number of iterations.
  }
  \item{quiet}{
  Shall wee talk about iteration progression etc.?
  }
  \item{object}{An \code{LCa} object, see under "Value".}
  \item{x}{An \code{LCa} object, see under "Value".}
  \item{rnam}{Annotation of the rate-axis in the plot.}
  \item{newdata}{Prediction data frame, must have columns \code{A} and
  \code{P}. Any \code{Y} column is ignored, predictions are given in
  units of the \code{Y} supplied for the call that generated the
  \code{LCa} object.}
  \item{level}{Confidence level.}
  \item{alpha}{One minus the confidence level.}
  \item{sim}{Logical or numeric. If \code{TRUE}, prediction c.i.s will be
  based on 1000 simulations from the posterior parameters. If numeric,
  it will be based on that number of simulations.}
  \item{...}{Additional parameters passed on to the method.}
}
\details{
  The Lee-Carter model is non-linear in age and time so does not fit
  in the classical glm-Poisson framework. But for fixed \code{b(x)} it
  is a glm, and also for fixed \code{a(x)}, \code{k(t)}. The function
  alternately fits the two versions until the same fit is produced (same
  deviance).

  The standard errors of the parameters from the model fits are however
  wrong; they are conditional on some of therms having a fixed value. And
  the symbolic calculation of the Hessian is a nightmare, so this is done
  numerically using the \code{hessian} function from the \code{numDeriv}
  package. But only conditional on \code{VC=TRUE}, because it is a bit
  time-consuming (minutes, not hours, though).
  The coefficients and the variance-covariance matrix of these are used
  in \code{predict.LCa} for a parametric bootstrap to get confidence
  intervals for the predictions if \code{sim} is \code{TRUE} --- which
  it is by default if they are part of the object.
  }
\value{
  \code{LCa.fit} returns an object of class \code{LCa} (smooth
  effects \code{L}ee-\code{Ca}rter model); it is a list with the
  following components:
  \item{Model}{Character "Period" or "Cohort", indicating the varible
  intercting with age.}
  \item{ax}{3-column matrix of age-effects, c.i. from the
  age-time model. Rownames are the actually occurring ages in the dataset.}
  \item{kt}{3-column matrix of time-effects, c.i. from the
  age-time model. Rownames are the actually occurring times in the dataset.}
  \item{bx}{3-column matrix of age-interaction effects, c.i. from the
  age model. Rownames are the actually occurring ages in the dataset.}
  \item{mod.at}{\code{glm} object with the final age-time model. Gives
  the same fit as the \code{mod.b} model.}
  \item{mod.b}{\code{glm} object with the final age model. Gives
  the same fit as the \code{mod.at} model.}
  \item{coef}{All coefficients from both models, in the order \code{ax},
  \code{kt}, \code{bx}. Only present if \code{LCa.fit} were called with
  \code{VC=TRUE}.} 
  \item{vcov}{Variance-covariance matrix of coefficients from both
  models, in the order \code{ax}, \code{kt}, \code{bx}. Only present if
  \code{LCa.fit} were called with \code{VC=TRUE}.} 
  \item{a.kn}{Knots used for the main age affect.}
  \item{b.kn}{Knots used for the age-interaction.}
  \item{t.kn}{Knots used for the time-effect.}
  \item{ref.b}{Reference value for the age-interaction}
  \item{ref.t}{Reference value for the time-interaction}
  \item{deviance}{Deviance of the model}
  \item{df}{Degrees of freedom}
  \item{iter}{Number of iteration to reach convergence.}

  \code{plot.LCa} plots the three estimated effects in separate panels,
  using a log-scale for the baseline rates (\code{ax}) and the time-RR
  (\code{kt}).
  
  \code{summary.LCa} returns (invisibly) a matrix with the parameters
  from the models and a column of the conditional se.s and of the se.s
  derived from the numerically computed Hessian (if \code{LCa.fit} were
  called with \code{VC=TRUE}.)

  \code{predict.LCa} returns a matrix with one row per row in
  \code{newdata}. If \code{LCa.fit} were called with \code{VC=TRUE}
  there will be 3 columns, namely prediction (1st column) and c.i.s
  based on the s.e. from the joint Hessian using the median and
  \code{alpha}/2 quantiles from the \code{sim} simulations.  If
  \code{LCa.fit} were called with \code{VC=FALSE} (the default) there
  will be 6 columns, namely estimates and c.i.s from age-time model, and
  from the age-interaction model (both using conditional variances).
  }
%% \references{
%% ~put references to the literature/web site here ~
%% }
\author{
Bendix Carstensen, \url{http://BendixCarstensen.com}
}
\seealso{\code{\link{apc.fit}}}
\examples{
library( Epi )
# Load the male lung cancer data by Lexis triangles and
# rename age and period as required by LCa.fit
data( lungDK )
head( lungDK )
lM <- lungDK[,c("Ax","Px","D","Y")]
names( lM )[1:2] <- c("A","P")
head( lM )

# We want to see rates per 1000 PY
lM$Y <- lM$Y / 10^3

# Fit the Lee-Carter model with age-period interaction (default)
system.time( LCa.lM <- LCa.fit( lM, ref.b=70, ref.t=1970,
                                quiet=FALSE, VC=TRUE ) )

LCa.lM
summary( LCa.lM )

# Inspect what we got
names( LCa.lM )

# show the estimated effects
plot( LCa.lM )

# Prediction data frame for ages 60 to 80 for three time points: 
nd <- data.frame( A=rep(60:80,3), P=rep(c(1960,1970,1980),each=21) )
# separate by NA to facilitate plotting of curves
nd <- rbind( nd[1:21,], NA, nd[21+1:21,], NA, nd[21*2+1:21,] )

predict.LCa( LCa.lM, newdata=nd )
pp <- predict.LCa( LCa.lM, newdata=nd, sim=10000 )

# Inspect the difference between the curves from the conditional fits
# and the quantiles from the parametric bootstrap (simulation):
par( mfrow=c(1,1) )
matplot( nd$A, pp, type="l", lwd=c(4,2,2)/2, lty=1,
         col=rep(c("red","forestgreen","black"),each=3), log="y",
         ylab="Male lung cancer incidence per 1000 PY, 1970 & 1980", xlab="Age" )
}
\keyword{models}
\keyword{regression}
