\name{Relevel}
\alias{Relevel}
\alias{Relevel.factor}
\title{Reorder and combine levels of a factor}
\description{
  The levels of a factor are re-ordered so that the levels specified by
  \code{ref} appear first and remaining levels are moved down. This is
  useful for \code{contr.treatment} contrasts which take the first level
  as the reference. Factor levels may also be combined; two possibilities for
  specifying this are supported.
}
\usage{
\method{Relevel}{factor}( x, ref, first = TRUE, collapse="+", xlevels=TRUE, \dots )
}
\arguments{
  \item{x}{An unordered factor}
  \item{ref}{Vector, list, matrix or table. If \code{ref} is a vector
    (integer or character), it is assumed it contains the names or
    numbers of levels to be the first ones; nom mentoned levels are
    kept. If \code{ref} is a list,
    factor levels mentioned in each list element are combined. If the
    list is named the names are used as new factor levels, otherwise
    new level names are constructed. If \code{ref} is a
    matrix or table, the two first columns are assumed to have unique
    levels of \code{x} and groupings of this, respectively.}
  \item{first}{Should the levels mentioned in \code{ref} (if it is a
    list) come before those not?} 
  \item{collapse}{String used when constructing names for combined factor levels.}
  \item{xlevels}{Logical. Should all levels in the 2nd column of
    \code{ref} be maintained, or (if \code{FALSE}) only the acutally
    ocurring. See details.}
  \item{\dots}{Arguments passed on to other methods.}
}
\value{
  An unordered factor, where levels of \code{x} have been reordered
  and/or collapsed.
}
\details{
  The facility where \code{ref} is a two-column matrix mimicks the
  SAS-facility where a dataset can be used to construct a format --- SAS
  format is the grouping tool for variable values.
}
\author{Bendix Carstensen, \url{BendixCarstensen.com}.}
\seealso{\code{\link{Relevel.Lexis}}}
\examples{
# Grouping using a list (hardcoding)
#
ff <- factor( sample( letters[1:5], 100, replace=TRUE ) )
table( ff, Relevel( ff, list( AB=1:2, "Dee"=4, c(3,5) ) ) )
table( ff, Relevel( ff, list( 5:4, Z=c("c","a") ), coll="-und-",first=FALSE ) )

# Grouping using a two-column matrix as input
#
ff <- factor( sample( letters[1:10][-c(4,7)], 100, replace=TRUE ),
              levels = letters[1:10] )
table(ff)
gg <- cbind( letters[sample(1:10)],
             LETTERS[sample(1:4,10,repl=TRUE)] )
gg
# check the two tabels are alive at the same places as they shoudl be:
print( as.table( cbind( table( gg[,1], gg[,2] ), NA, NA,
                        table( ff, Relevel(ff, gg) ) ) ), zero="." )
}
\keyword{manip}
