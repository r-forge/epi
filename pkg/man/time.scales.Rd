\name{timeScales}
\alias{timeScales}
\alias{time.scales.Lexis}
\title{The time scales of a Lexis object}
\description{
  Function to get the names of the time scales of a \code{Lexis} object.
}
\usage{
timeScales(x)
\method{time.scales}{Lexis}(x)
}
\arguments{
  \item{x}{an object of class \code{Lexis}.}
}
\value{
  A character vector containing the names of the variables in \code{x}
  that represent the time scales. Extracted from the \code{time.scales}
  attribute of the object.
}
\details{
  The two functions are identical.
}
\author{Martyn Plummer}
\seealso{\code{\link{Lexis}}, \code{\link{splitLexis}}}
\keyword{attribute}
