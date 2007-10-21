\name{cutLexis}
\alias{cutLexis}
\title{
  Cuts follow-up at a specified date for each person.
  }
\description{
  The follow-up of a person is divided into the period before and after
  an intermediate event, typically disease recurrenece or something like
  that. This event is assigned a new state.
  }
\usage{
  cutLexis( data,
             cut,
       timescale = timeScales(data)[1],
       new.state = max( as.integer( c(data$lex.Cst,data$lex.Xst) )+1 ),
          na.cut = Inf,
            cens = ifelse( is.character(new.state), "0", 0 ) )
  }
\arguments{
  \item{data}{A \code{\link{Lexis}} object.}
  \item{cut}{Numerical vector with the times of intermediate event.
    NAs are allowed, in which cases the original record is retained.}
  \item{timescale}{The timescale that \code{cut} refers to.}
  \item{new.state}{What value should the new state have?}
  \item{na.cut}{What value should be substituted for the NAs in
    \code{cut}. The default is \code{Inf} which results in the orginal
    records being kept.}
  \item{cens}{Censoring values for the states. Persons in any of these states
              will have their entry and exit status (\code{lex.Cst},
              \code{lex.Xst}, updated if the follow up is after \code{cut}.}
  }
\value{
  A \code{\link{Lexis}} object, where the follow-up of each person is cut in
  two at the time as given in \code{cut}.
  }
\details{
  The \code{cens} parameter is used to decide wheter an exitsing status
  (\code{lex.Cst} or \code{lex.Xst}) should be updated to
  \code{new.state} or not, if the corresponding entry or exit time is after
  \code{cut}.
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
