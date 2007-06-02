\name{start.Lexis}
\alias{entry}
\alias{exit}
\alias{status}
\alias{deltat.Lexis}
\title{Time series methods for Lexis objects}
\description{
  Extract the entry time, exit time, status, or duration of follow-up from a
  \code{Lexis} object.
}
\usage{
entry(x, time.scale = NULL)
exit(x, time.scale = NULL)
status(x, at=c("exit","entry"))
\method{deltat}{Lexis}(x, ...)
}
\arguments{
  \item{x}{an object of class \code{Lexis}}
  \item{time.scale}{a string or integer indicating the time scale. If
    omitted, the first time scale is used as a default}
  \item{at}{string indicating the time point at which status is to be measured}
  \item{\dots}{optional arguments to the \code{deltat} method}
}
\value{
  The \code{entry} and \code{exit} functions return a vector containing
  the entry times and exit times, respectively, on the requested time
  scale. The \code{status} function returns the status either at entry
  or (by default) at exit, and the \code{deltat} method returns the length
  of the follow-up interval.
}
\author{Martyn Plummer}
\seealso{\code{\link{Lexis}}}
\keyword{ts}
