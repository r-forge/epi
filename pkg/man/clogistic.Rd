\name{clogistic}
\alias{clogistic}
\title{Conditional logistic regression}
\description{
  Estimates a logistic regression model by maximizing the conditional
  likelihood.  The conditional likelihood calculations are exact and
  scale efficiently to strata with large numbers of cases.
}
\usage{
clogistic(formula, strata, data, subset, na.action, init,
model = TRUE, x = FALSE, y = TRUE, contrasts = NULL,
iter.max=20, eps=1e-6, toler.chol = sqrt(.Machine$double.eps)) 
}
\arguments{
  \item{formula}{Model formula}
  \item{strata}{Factor describing membership of strata for conditioning}
  \item{data}{data frame containing the variables in the formula and
    strata arguments}
  \item{subset}{subset of records to use}
  \item{na.action}{missing value handling}
  \item{init}{initial values}
  \item{model}{
    a logical value indicating whether \emph{model frame}
    should be included as a component of the returned value}
  \item{x,y}{
    logical values indicating whether the response vector and model
    matrix used in the fitting process should be returned as components
    of the returned value.
  }
  \item{contrasts}{
    an optional list. See the \code{contrasts.arg} of
    \code{model.matrix.default}
  }
  \item{iter.max}{maximum number of iterations}
  \item{eps}{
    Convergence tolerence.  Iteration continues until the relative
    change in the conditional log likelihood is less than \code{eps}.
    Must be positive.
  }
  \item{toler.chol}{
    Tolerance used for detection of a singularity during a Cholesky
    decomposition of the variance martrix. This is used to detect
    redundant predictor variables. Must be less than \code{eps}.
  }
}
\value{
  An object of class \code{"clogistic"}.
}
\examples{
  data(bdendo)
  clogistic(d ~ cest + dur, strata=set, data=bdendo)
}
\author{Martyn Plummer}
\seealso{\code{\link{glm}}}
\keyword{models}
