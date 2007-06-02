\name{Lexis}
\alias{Lexis}
\title{Create a Lexis object}
\description{
  Create an object of class \code{Lexis} to represent follow-up on
  multiple time scales.
}  
\usage{
Lexis(entry, exit, duration, entry.status = 0, exit.status = 0, id, data,
      merge=TRUE)
}
\arguments{
  \item{entry}{a named list of entry times. Each element of the list is
    a numeric variable representing the entry time on the named time
    scale. All time scales must have the same units (e.g. years)}
  \item{exit}{a named list of exit times}
  \item{duration}{a numeric vector giving the duration of follow-up}
  \item{entry.status}{a vector giving the status at entry}
  \item{exit.status}{a vector giving status at exit. Any change in
    status during follow-up is assumed to take place exactly at the exit time}
  \item{id}{a vector giving a unique identity value for each row of the
    Lexis object}
  \item{data}{an optional data frame, list, or environment containing
    the variables. If  not found in \code{data}, the variables are
    taken from the environment from which \code{Lexis} was called}
  \item{merge}{a logical flag. If \code{TRUE} then the \code{data} 
    argument will be coerced to a data frame and then merged with
    the resulting \code{Lexis} object}
}
\details{
  The analysis of long-term population-based follow-up studies typically
  requires multiple time scales to be taken into account, such as
  age, calender time, or time since an event. A \code{Lexis} object is
  a data frame with additional attributes that allows these multiple time
  dimensions of follow-up to be managed.

  Lexis objects are named after the German gemographer Wilhelm
  Lexis (1837-1914), who is credited with the invention of the
  "Lexis diagram" for representing population dynamics simultaneously
  by age, period and cohort.
  
  The \code{Lexis} function creates a minimal \code{Lexis} object with
  only those variables required to define the follow-up history in each
  row. Additional variables can be merged into the \code{Lexis} object
  using the \code{merge} method for \code{Lexis} objects.
}
\note{
  Only two of the three arguments \code{entry}, \code{exit} and
  \code{duration} need to be given.  If the third parameter is missing,
  it is imputed. If duration is imputed, it must be the same on
  all time scales.
}
\value{
  An object of class \code{Lexis}. This is represented as a data frame
  with a column for each time scale, and additional columns with the
  following names:
  \item{lex.id}{Identification of the inidvidual}
  \item{lex.deltat}{Duration of follow-up}
  \item{lex.status1}{Entry status}
  \item{lex.status2}{Exit status}
  If \code{merge=TRUE} then the \code{Lexis} object will also contain
  all variables from the \code{data} argument.
}
\author{Martyn Plummer}
\examples{
# A small bogus cohort
xcoh <- structure( list( id = c("A", "B", "C"),
                      birth = c("14/07/1952", "01/04/1954", "10/06/1987"),
                      entry = c("04/08/1965", "08/09/1972", "23/12/1991"),
                       exit = c("27/06/1997", "23/05/1995", "24/07/1998"),
                       fail = c(1, 0, 1) ),
                     .Names = c("id", "birth", "entry", "exit", "fail"),
                  row.names = c("1", "2", "3"),
                      class = "data.frame" )

# Convert the character dates into numerical variables (fractional years)
xcoh$bt <- cal.yr( xcoh$birth, format="\%d/\%m/\%Y" )
xcoh$en <- cal.yr( xcoh$entry, format="\%d/\%m/\%Y" )
xcoh$ex <- cal.yr( xcoh$exit , format="\%d/\%m/\%Y" )

# See how it looks
xcoh

# Define as Lexis object with timescales calendar time and age
Lcoh <- Lexis( entry = list( per=en ),
                exit = list( per=ex, age=ex-bt ),
         exit.status = fail,
                data = xcoh )
Lcoh
}
\seealso{\code{\link{plot.Lexis}}, \code{\link{splitLexis}},
  \code{\link{merge.Lexis}}, \code{\link{entry}},
  \code{\link{timeScales}},
  \code{\link{timeBand}}}
\keyword{survival}
