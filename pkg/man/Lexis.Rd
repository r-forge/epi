\name{Lexis}
\alias{Lexis}
\alias{print.Lexis}
\title{Create a Lexis object of follow-up}
\description{
  Create an object of class \code{Lexis} to represent follow-up in
  multiple states on multiple time scales.
}
\usage{
Lexis( entry,
        exit,
    duration,
entry.status = 0,
 exit.status = 0,
          id,
        data,
       merge = TRUE,
      states,
       notes = TRUE,
         tol = .Machine$double.eps^0.5,
keep.dropped = FALSE)
\method{print}{Lexis}(x, ...,
                      td = 2,
                      nd = 3,
                    rnam = FALSE,
                     org = FALSE)
}
\arguments{
  \item{entry}{a named list of entry times. Each element of the list is
    a numeric variable representing the entry time on the named time
    scale. The name of the elements of the list will appear as names of
    variables designated as timescales in the resulting object. All time
    scales must have the same units (e.g. years). 
    The names of the timescales must be different from any column name in
    \code{data}.}
  \item{exit}{a named list of exit times.}
  \item{duration}{a numeric vector giving the duration of follow-up.}
  \item{entry.status}{a vector or a factor giving the status at
    entry}
  \item{exit.status}{a vector or factor giving status at exit. Any
    change in status during follow-up is assumed to take place exactly
    at the exit time.}
  \item{id}{a vector giving a unique identity value for each person
    represented in the Lexis object. Defaults to \code{1:nrow(data)}}
  \item{data}{an optional data frame, list, or environment containing
    the variables. If  not found in \code{data}, the variables are
    taken from the environment from which \code{Lexis} was called.}
  \item{merge}{a logical flag. If \code{TRUE} then the \code{data}
    argument will be coerced to a data frame and then merged with
    the resulting \code{Lexis} object.}
  \item{states}{A vector of labels for the states. If given, the state
    variables \code{lex.Cst} and \code{lex.Xst} are returned as factors with
    identical levels attributes equal to \code{states}.}
  \item{notes}{Logical. Should notes on entry states and time be given.}
  \item{tol}{Numerical tolerance for follow-up time. Rows with duration
    less than this value are automatically dropped.}
  \item{keep.dropped}{Logical. Should dropped rows from \code{data} be
    saved as an attribute with the object for inspection?}
  \item{x}{A \code{Lexis} object.}
  \item{td}{Number of digits after the decimal separator used for
    timescales and \code{lex.dur} when printing}
  \item{nd}{Number of digits after the decimal separator used for other
    numerical variables in the \code{Lexis} object.}
  \item{rnam}{Logical, should row names be printed?}
  \item{org}{Logical, should columns be printed in the original order?}
  \item{ ... }{Other parameters passed on to \code{print.data.frame}.}
}
\details{
  The analysis of long-term population-based follow-up studies typically
  requires multiple time scales to be taken into account, such as
  age, calendar time, or time since an event. A \code{Lexis} object is
  a data frame with additional attributes that allows these multiple time
  dimensions of follow-up to be managed.

  Separate variables for current end exit state allows representation of
  multistate data.

  Lexis objects are named after the German demographer Wilhelm
  Lexis (1837-1914), who is credited with the invention of the
  "Lexis diagram" for representing population dynamics simultaneously
  by several timescales in the book
  "Einleitung in die Theorie der Bevolkerungsstatistik" from 1875. 

  The \code{Lexis} function can create a minimal \code{Lexis} object
  with only those variables required to define the follow-up history in
  each row. Additional variables can be merged into the \code{Lexis}
  object using the \code{merge} method for \code{Lexis} objects. The
  latter is the default.

  The \code{print} method prints the time-scale variables and other
  numerical variables rounded, possibly differently. Reorders columns so
  the Lexis-specific variables comes first. Returns (invisibly) a character
  vector with the (re)ordering of the columns in the object, even if
  \code{org = TRUE} is set.
  
  There are also \code{merge}, \code{subset}, \code{transform} and many
  other methods for \code{Lexis} objects. They work as the corresponding
  methods for data-frames but ensures that the result is a \code{Lexis}
  object.
  }
\note{
  Only two of the three arguments \code{entry}, \code{exit} and
  \code{duration} need to be given.  If the third parameter is missing,
  it is imputed.

  \code{entry}, \code{exit} must be numeric, using  \code{\link{Date}}
  variables will cause some of the utilities to crash. Transformation by
  \code{\link{cal.yr}} is recommended.

  If only either \code{exit} or \code{duration} are supplied it is
  assumed that \code{entry} is 0. This is only meaningful (and therefore
  checked) if there is only one timescale.

  If any of \code{entry.status} or \code{exit.status} are of mode character,
  they will both be converted to factors.

  If \code{entry.status} is not given, then its class is automatically
  set to that of \code{exit.status}. If \code{exit.status} is a
  character or factor, the value of \code{entry.status} is set to the
  first level. This may be highly undesirable, and therefore noted. For
  example, if \code{exit.status} is character the first level will be
  the first in the alphabetical ordering; slightly unfortunate if values
  are \code{c("Well","Diseased")}. If \code{exit.status} is logical, the
  value of \code{entry.status} set to \code{FALSE}. If
  \code{exit.status} is numeric, the value of \code{entry.status} set to
  0.

  If \code{entry.status} or \code{exit.status} are factors or character,
  the corresponding state variables in the returned \code{Lexis} object,
  \code{lex.Cst} and \code{lex.Xst} will be (unordered) factors with
  identical set of levels, namely the union of the levels of
  \code{entry.status} and \code{exit.status}.
  }

\value{
  An object of class \code{Lexis}. This is represented as a data frame
  with a column for each time scale (with names equal to the union of
  the names of \code{entry} and \code{exit}), and additional columns with the
  following names:
  \item{lex.id}{Identification of the persons.}
  \item{lex.dur}{Duration of follow-up.}
  \item{lex.Cst}{Entry status (\code{C}urrent \code{st}ate),
                 i.e. the state in which the follow up takes place.}
  \item{lex.Xst}{Exit status (e\code{X}it \code{st}ate),
                 i.e. that state taken up after \code{dur} in \code{lex.Cst}.}
  If \code{merge=TRUE} (the default) then the \code{Lexis} object will
  also contain all variables from the \code{data} argument.
  }

\author{Martyn Plummer with contributions from Bendix Carstensen}

\examples{
# A small bogus cohort
xcoh <- structure(list( id = c("A", "B", "C"),
                     birth = c("14/07/1952", "01/04/1954", "10/06/1987"),
                     entry = c("04/08/1965", "08/09/1972", "23/12/1991"),
                      exit = c("27/06/1997", "23/05/1995", "24/07/1998"),
                      fail = c(1, 0, 1) ),
                    .Names = c("id", "birth", "entry", "exit", "fail"),
                 row.names = c("1", "2", "3"),
                     class = "data.frame")

# Convert the character dates into numerical variables (fractional years)
xcoh <- cal.yr(xcoh, format="\%d/\%m/\%Y", wh=2:4)
# xcoh <- cal.yr(xcoh, format="%d/%m/%Y", wh=2:4)

# See how it looks
xcoh
str( xcoh )

# Define a Lexis object with timescales calendar time and age
Lcoh <- Lexis(entry = list(per = entry ),
               exit = list(per = exit,
                           age = exit - birth),
        exit.status = fail,
               data = xcoh)

# Using character states may have undesired effects:
xcoh$Fail <- c("Dead","Well","Dead")
xcoh
L1 <- Lexis(entry = list(per = entry),
             exit = list(per = exit,
                         age = exit - birth),
      exit.status = Fail,
             data = xcoh)
L1
# people start being dead!

# ...unless you order the levels sensibly
xcoh$Fail <- factor(xcoh$Fail, levels = c("Well", "Dead"))
L2 <- Lexis(entry = list(per = entry),
             exit = list(per = exit,
                         age = exit - birth),
      exit.status = Fail,
             data = xcoh)
L2
# behaviour of print method:
L2[,1:6]
L2[,6:1]
print(L2[,6:1], org=TRUE)
(print(L2[,-3]))
}
\seealso{
   \code{\link{plot.Lexis}},
   \code{\link{splitLexis}},
   \code{\link{cutLexis}},
   \code{\link{mcutLexis}},
   \code{\link{rcutLexis}},
   \code{\link{addCov.Lexis}},
%   \code{\link{glm.Lexis}},
%   \code{\link{gam.Lexis}},
%   \code{\link{coxph.Lexis}},
   \code{\link{merge.Lexis}},
   \code{\link{subset.Lexis}},
   \code{\link{cbind.Lexis}},
   \code{\link{rbind.Lexis}},
   \code{\link{transform.Lexis}},
   \code{\link{summary.Lexis}},
   \code{\link{unLexis}},
   \code{\link{timeScales}},
   \code{\link{timeBand}},
   \code{\link{entry}},
   \code{\link{exit}},
   \code{\link{transient}},
   \code{\link{absorbing}},
   \code{\link{dur}}
   }
\keyword{survival}
\keyword{manip}
