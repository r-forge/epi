\name{gen.exp}
\alias{gen.exp}
\title{
Generate covariates for drug-exposure follow-up from drug purchase records.
}
\description{
From records of drug purchase and possibly known treatment intensity,
the time since first drug use and cumulative dose at prespecified times
is computed. Optionally, lagged exposures are computed too,
i.e. cumulative exposure a prespecified time ago.
}
\usage{
gen.exp( purchase, id = "id", dop = "dop", amt = "amt", dpt = "dpt",
               fu, doe = "doe", dox = "dox",
           breaks,
          use.dpt = ( dpt \%in\% names(purchase) ),
             lags = NULL,
         push.max = Inf,
         pred.win = Inf,
          lag.dec = 1 )
}
\arguments{
  \item{purchase}{Data frame with columns \code{id}-person id,
    \code{dop} - \code{d}ate \code{o}f \code{p}urchase, \code{amt} -
    \code{am}oun\code{t} purchased, and optionally \code{dpt} -
    (\code{d}ose \code{p}er \code{t}ime) ("defined daily dose", DDD,
    that is), how much is assume  to be ingested per unit time. The
    units used for \code{dpt} is assumed to be units of \code{amt} per
    units of \code{dop}.} 
  \item{id}{Character. Name of the id variable in the data frame.}
  \item{dop}{Character. Name of the \code{d}ate \code{o}f \code{p}urchase variable in the data frame.}
  \item{amt}{Character. Name of the \code{am}oun\code{t} purchased variable in the data frame.}
  \item{dpt}{Character. Name of the \code{d}ose-\code{p}er-\code{t}ime variable in the data frame.}
  \item{fu}{Data frame with \code{f}ollow-\code{u}p period for each person, the person
    id variable must have the same name as in the \code{purchase} data frame.}
  \item{doe}{Character. Name of the \code{d}ate \code{o}f \code{e}ntry variable.}
  \item{dox}{Character. Name of the \code{d}ate \code{o}f e\code{x}it variable.}
  \item{use.dpt}{Logical: should we use information on dose per time.}
  \item{breaks}{Numerical vector of dates at which the time since
    first exposure, cumulative dose etc. are computed.}
  \item{lags}{Numerical vector of lag-times used in computing lagged cumulative
    doses.}
  \item{push.max}{Numerical. How much can purchases maximally be pushed forward in
    time. See details.}
  \item{pred.win}{The length of the window used for constructing the
    average dose per time used to compute the duration of the last purchase}
  \item{lag.dec}{How many decimals to use in the construction of
    names for the lagged exposure variables}
}

\details{
  Each purchase record is converted into a time-interval of exposure.

  If \code{use.dpt} is \code{TRUE} then the dose per time information is
  used to compute the exposure interval associated with each purchase.
  Exposure intervals are stacked, that is each interval is put after any
  previous. This means that the start of exposure to a given purchase
  can be pushed into the future. The parameter \code{push.max} indicates
  the maximally tolerated push. If this is reached by a person, the
  assumption is that some of the purchased drug is not counted in the
  exposure calculations.

  The \code{dpt} can either be a constant, basically translating the
  purchased amount into exposure time the same way for all persons, or
  it can be a vector with different treatment intensities for each
  purchase. In any case the cumulative dose is computed taking this
  into account.

  If \code{use.dpt} is \code{FALSE} then the exposure from one purchase
  is assumed to stretch over the time to the next purchase, so we are
  effectively assuming different rates of dose per time between any two
  adjacent purchases.

  Formally this approach conditions on the
  future, because the rate of consumption (the accumulation of
  cumulative exposure) is computed based on knowledge of when next
  purchase is made.

  Moreover, with this approach, periods of
  non-exposure does not exist, except after the last purchase where the
  future consumption rate is taken to be the average over the period of
  use, and hence defines a date of cesstion.
  
  The intention of this function is to generate covariates for a
  particular drug for the entire follow-up of each person. The reason
  that the follow-up prior to drug purchase and post-exposure is
  included is that the covariates must be defined for these periods too,
  in order to be useful for analysis of disease outcomes.

  The functionality is described in terms of calendar time as underlying
  time scale, because this will normally be the time scale for drug
  purchases and for entry and exit for persons. In principle the
  variables termed as dates might equally well refer to say the age
  scale, but this would then have to be true \emph{both} for the
  purchase data and the follow-up data.  
}

\value{A data frame with one record per person and follow-up interval between
  \code{breaks}. There is no guarantee that variables are
  in the order given here:
\describe{
    \item{\code{id}}{person id.}
    \item{\code{dof}}{date of follow up, i.e. start of interval. Apart
  from possibly the first interval for each person, this will assume
  values in the set of the values in \code{breaks}. All other variables
  refer to this date.}
    \item{\code{dur}}{the length (\code{dur}ation) of interval.}
    \item{\code{tfi}}{\code{t}ime \code{f}rom first \code{i}nitiation of drug.}
    \item{\code{off}}{Logical, indicating whether the person is \code{off} drug
  in the interval. So it is \code{FALSE} for exposed intervals.}
    \item{\code{doff}}{\code{d}ate of last transition to \code{off}
  drug. Note that tis defined also at dates after drug exposure has been
  resumed.}
    \item{\code{tfc}}{\code{t}ime \code{f}rom latest \code{c}essation of drug.}
    \item{\code{ctim}}{\code{c}umulative \code{tim}e on the drug.}
    \item{\code{cdos}}{\code{c}umulative \code{dos}e.}
    \item{\code{ldos}}{suffixed with one value per element in
  \code{lags}, the latter giving the cumulative doses \code{lags} before
  \code{dof}.}
}
}

\author{Bendix Carstensen, \email{b@bxc.dk}.  The development of
this function was supported partly through a grant from the EFSD
(European Foundation for the Study of Diabetes)}

\seealso{\code{\link{Lexis}}, \code{\link{splitLexis}}}
\examples{
# Example data for drug purchases in 3 persons - note dates (dop) is in
# years, amount purchased (amt) in no.pills and dose per time (dpt)
# consequently given in pills/year:
n <- c( 10, 18, 8 )
hole <- rep(0,n[2])
hole[10] <- 2 # to create a hole of 2 years in purchase dates
dop <- c( 1995.278+cumsum(sample(1:4/10,n[1],replace=TRUE)),
          1992.351+cumsum(sample(1:4/10,n[2],replace=TRUE)+hole),
          1997.320+cumsum(sample(1:4/10,n[3],replace=TRUE)) )
amt <- sample( 1:3*50 , sum(n), replace=TRUE ) # size of packages (no. pills)
dpt <- sample( 4:1*365, sum(n), replace=TRUE ) # pills per year 
dfr <- data.frame( id = rep(1:3,n),
                  dop,
                  amt = amt,
                  dpt = dpt )
head( dfr, 3 )

# a simple dataframe for follow-up periods for these 3 persons
fu <- data.frame( id = 1:3,
                 doe = c(1995,1992,1996)+1:3/4,
                 dox = c(2001,2003,2002)+1:3/5 )
fu

# Note that the following use of gen.exp relies on
# dfr having variable names id dop amt dpt
#  fu having variable names id doe dox

# 1: use the dosage information
dposx <- gen.exp( dfr,
                   fu = fu,
              use.dpt = TRUE,
               breaks = seq(1990,2015,0.1),
                 lags = 2:4/4 )
format( dposx, digits=5 )

# 2: ignore the dosage information
xposx <- gen.exp( dfr,
                   fu = fu,
              use.dpt = FALSE,
               breaks = seq(1990,2015,0.5),
                 lags = 2:3/5 )
format( xposx, digits=5 )

# it is possible to have disjoint follow-up period for the same person:
fu <- fu[c(1,2,2,3),]
fu$dox[2] <- 1996.2
fu$doe[3] <- 1998.3
fu

# not that drug purchase information for the period not at risk *is* used
dposx <- gen.exp( dfr,
                   fu = fu,
              use.dpt = TRUE,
               breaks = seq(1990,2015,0.1),
                 lags = 2:4/4 )
format( dposx, digits=5 )
}
\keyword{data manipulation}
