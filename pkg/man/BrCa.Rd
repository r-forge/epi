\name{BrCa}
\alias{BrCa}
\docType{data}
\title{
Survival, recurrence and metastases in women with primary breast cancer.
}
\description{
This is slightly groomed version of the dataset provided as example data
by Crowther \& Lambert in their paper on multistate models.
}
\usage{data("BrCa")}
\format{
  A data frame with 2982 observations on the following 17 variables.
  \describe{
    \item{\code{pid}}{Person id}
    \item{\code{year}}{Calendar year of diagnosis}
    \item{\code{age}}{Age at diagnosis}
    \item{\code{meno}}{Menopausal status: \code{pre} / \code{post}}
    \item{\code{size}}{Tumour size: \code{<=20 mm} / \code{>20-50 mm} / \code{>50 mm}}
    \item{\code{grade}}{Grade of tumour: \code{2} / \code{3} }
    \item{\code{nodes}}{Number of positive lymph nodes}
    \item{\code{pr}}{Progesterone receptor value}
    \item{\code{pr.tr}}{Transformed progesterone receptor value}
    \item{\code{er}}{Estrogen receptor value}
    \item{\code{hormon}}{Hormone replacement therapy: \code{no} / \code{yes}}
    \item{\code{chemo}}{Chemotherapy treatment: \code{no} / \code{yes}}
    \item{\code{tor}}{Time from diagnosis to relapse (years)}
    \item{\code{tom}}{Time from diagnosis to metastases (years)}
    \item{\code{tod}}{Time from diagnosis to death (years)}
    \item{\code{tox}}{Time from diagnosis to exit from study (years)}
    \item{\code{xst}}{Vital status at exit: \code{Alive} / \code{Dead}}
  }
}
\details{
As opposed to the original Stata data set the time variables have be
re-computed so that only dates of actually occurring events are
non-missing, and hence the times of censoring are in \code{tox}.
}
\source{
The original data were read from
\url{http://fmwww.bc.edu/repec/bocode/m/multistate_example.dta}
in January 2018.
}
\references{
Michael Crowther \& Paul Lambert:
"Parametric multistate survival models: Flexible modelling
allowing transition-specific distributions with application
to estimating clinically useful measures of effect differences" form
Statistics in Medicine, 2017;36: 4719-4742.
}
\examples{
data(BrCa)
str(BrCa)
head(BrCa)
}
\keyword{datasets}
