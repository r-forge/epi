\name{cutLexis}
\alias{cutLexis}
\title{
  Cuts follow-up at a specified date for each person.
  }
\description{
  The follow-up of a person is divided into the period before and after
  an intermediate event, which may denote a transition to a new state.
  }
\usage{
  cutLexis( data,
             cut,
       timescale = timeScales(data)[1],
       new.state,
           count = FALSE,
          na.cut = Inf)
  }
\arguments{
  \item{data}{A \code{\link{Lexis}} object.}
  \item{cut}{Numerical vector with the times of intermediate event.
    NAs are allowed, in which cases the original record is retained.}
  \item{timescale}{The timescale that \code{cut} refers to.}
  \item{new.state}{What value should the new state have? If omitted, the
    previous (entry) state is carried forward.}
  \item{count}{logical flag. If \code{new.state} is omitted, then
    setting \code{count=TRUE} uses a counting notation, and the state
    variable is augmented by 1 at the cut time.}
  \item{na.cut}{What value should be substituted for the NAs in
    \code{cut}. The default is \code{Inf} which results in the orginal
    records being kept.}
  }
\value{
  A \code{\link{Lexis}} object, where the follow-up of each person is cut in
  two at the time as given in \code{cut}.
  }
\details{
  When a follow-up interval is split into two, then the exit state of the
  first interval and the entry state of the second interval are set to
  \code{new.state}.  If \code{new.state} is omitted, and \code{count=FALSE},
  then the entry state is used instead.

  Counting notation can be used when the state is a numeric variable that
  counts a number of events (such as disease occurrence) and the cut times
  indicate when those events occur.  With counting notation, the exit status
  of the second interval is also increased by 1.
  }
\author{
  Bendix Carstensen, Steno Diabetes Center, \email{bxc@steno.dk}
  }
\seealso{
  \code{\link{splitLexis}}
  }
\examples{
data( nickel )
nic <- Lexis( data=nickel,
             entry=list(age=agein),
              exit=list(age=ageout,cal=ageout+dob,tfh=ageout-age1st),
# Lung cancer deaths and other deaths are coded 1 and 2
       exit.status=( (icd > 0) + (icd \%in\% c(162,163)) ) )
str( nic )
tab.Lexis( nic )
nicc <- cutLexis( nic, 50, "age" )
tab.Lexis( nicc )
  }
\keyword{survival}
