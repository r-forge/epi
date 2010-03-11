\name{clogistic}
\alias{clogistic}
\title{Conditional logistic regression}
\description{
  Estimates a logistic regression model by maximizing the conditional
  likelihood.  The conditional likelihood calculations are exact and
  scale efficiently to strata with large numbers of cases.
}
\usage{
clogistic(formula, strata, data, weights, subset, na.action, init, offset, model = TRUE, x = FALSE, y = TRUE, contrasts = NULL, ...)
}
\arguments{
  \item{formula}{Model formula}
  \item{strata}{Factor describing membership of strata for conditioning}
  \item{data}{data frame containing the variables in the formula and
    strata arguments}
  \item{weights}{NOT IMPLEMENTED}
  \item{subset}{subset of records to use}
  \item{na.action}{missing value handling}
  \item{init}{initial values}
  \item{offset}{NOT IMPLEMENTED}
  \item{model}{a logical value indicating whether \emph{model frame}
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
  \item{\dots}{NOT IMPLEMENTED}
}
\value{
  An object of class \code{"clogistic"}.
}
\author{Martyn Plummer}
\seealso{\code{\link{glm}}}
\keyword{models}
