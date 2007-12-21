\name{cutLexis2}
\alias{cutLexis2}
\alias{countLexis}
\title{
  Cut follow-up at a specified date for each person.
}
\description{
  
  Follow-up intervals in a Lexis object are divided into two
  sub-intervals: one before and one after an intermediate event.  The
  intermediate event may denote a change of state, in which case the
  entry and exit status variables in the split Lexis object are
  modified.

}
\usage{
  cutLexis2(data, cut, timescale = 1, new.state, progressive=FALSE,
           precursor.states=NULL)
  countLexis(data, cut, timescale = 1)
}
\arguments{
  \item{data}{A \code{Lexis} object.}
  \item{cut}{A numeric vector with the times of the intermediate event.
    If a time is missing (\code{NA}) then the event is assumed to occur
    at time \code{Inf}}
  \item{timescale}{The timescale that \code{cut} refers to.}
  \item{new.state}{an optional vector, to be used when the cut point
    denotes a change of state. This may be a single value, which is
    applied to all rows of \code{data}, or a vector with a separate
    value for each row}
  \item{progressive}{a logical flag that determines the changes to exit
    status. See Details below}
  \item{precursor.states}{an optional vector of states to be considered
    as "less severe" than \code{new.state}. See Details below}
}
\value{
  A \code{Lexis} object, for which each follow-up interval
  containing the cut point is split into two rows: one before
  and one after the cut point.
}
\note{
  The \code{cutLexis} function superficially resembles the
  \code{splitLexis} function. However, the \code{splitLexis} function
  splits on a vector of common cut-points for all rows of the Lexis
  object, whereas the \code{cutLexis} function splits on a single time
  point, which may be distinct for each row, and additionally modifies
  the status variables.
}
\details{
  The \code{cutLexis} function allows a number of different ways
  of modifying the status variable.

  If the \code{new.state} argument is omitted, then the subject is
  assumed to remain in the entry state. In this case, if an interval
  is split, the entry status is carried forward to the cut point.

  If a \code{new.state} argument is supplied then, by default, the
  status variable is only modified at the time of the cut
  point. However, it is often useful to modify the status variable after
  the cutpoint when an important event occurs. There are three distinct
  ways of doing this.

  If the \code{progressive=TRUE} argument is given, then a "progressive"
  model is assumed, in which the status can either remain the same or
  increase during follow-up, but never decrease. In this case, if
  \code{new.state=X}, then any exit status with a value less than
  \code{X} is replaced with \code{X}. This argument may only be used if
  the status variable is numeric or an ordered factor. The Lexis object
  must already be progressive, so that there are no rows for which the
  exit status is less than the entry status.

  As an alternative to the \code{progressive} argument, an explicit
  vector of precursor states, that are considered less severe than the
  new state, may be given. If \code{new.state=X} and
  \code{precursor.states=c(Y,Z)} then any exit status of \code{Y} or
  \code{Z} in the second interval is replaced with \code{X} and all
  other values for the exit status are retained.

  The \code{countLexis} function is a variant of \code{cutLexis} when
  the cutpoint marks a recurrent event, and the status variable is used
  to count the number of events that have occurred.  Splitting with
  \code{countLexis} augments the status variable by 1. If the entry
  status is \code{X} and the exit status is \code{Y} before splitting,
  then after splitting the entry status is \code{X}, \code{X+1} for
  the first and second intervals, respectively, and the exit status is
  \code{X+1}, \code{Y+1} respectively.

}
\author{
  Bendix Carstensen, Steno Diabetes Center, \email{bxc@steno.dk}
}
\seealso{
  \code{\link{splitLexis}}
}
\examples{
# A small artificial example
xx <- Lexis( entry=list(age=c(17,24,33,29),per=c(1920,1933,1930,1929)),
             duration=c(23,57,12,15), exit.status=c(1,2,1,2) )
xx
cut <- c(33,47,29,50)
cutLexis2(xx, cut, new.state=3, precursor=1)
cutLexis2(xx, cut, new.state=3, precursor=2)
cutLexis2(xx, cut, new.state=3, precursor=1:2)
# The same as the last example
cutLexis2(xx, cut, new.state=3)

# The same example with a factor status variable
yy <- Lexis(entry = list(age=c(17,24,33,29),per=c(1920,1933,1930,1929)),
            duration = c(23,57,12,15),
            entry.status = factor(rep("alpha",4),
            levels=c("alpha","beta","gamma")),
            exit.status = factor(c("alpha","beta","alpha","beta"),
            levels=c("alpha","beta","gamma")))

cutLexis2(yy,c(33,47,29,50),precursor="alpha",new.state="gamma")
cutLexis2(yy,c(33,47,29,50),precursor=c("alpha","beta"),new.state="aleph")
}
\keyword{survival}
