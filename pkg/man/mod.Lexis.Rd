\name{mod.Lexis}
\alias{glm.Lexis}
\alias{gam.Lexis}
\alias{coxph.Lexis}
\title{Fit intensity models to follow-up data in Lexis objects  
}
\description{
Modeling intensities based on Lexis objects, exploiting the structure of the
Lexis objects where the events and risk time have predefined
representations. This allows a simpler syntax than the
traditional explicit modeling using \code{\link{glm}}, \code{\link{gam}}
and \code{\link{coxph}}. But it is only a set of wrappers fro
\code{glm}, \code{gam} and \code{coxph}.
}
\usage{
  glm.Lexis( Lx,         # Lexis object	
           resp,         # 'to' states	
        formula,         # ~ model	
           xpos,         # 'from' states
           link = "log", # link function
          scale = 1,     # scaling of PY
             ... )
  gam.Lexis( Lx,         # Lexis object	
           resp,         # 'to' states	
        formula,         # ~ model	
           xpos,         # 'from' states
           link = "log", # link function
          scale = 1,     # scaling of PY
             ... )
coxph.Lexis( Lx, # Lexis object	
           resp, # 'to' states	
        formula, # timescale ~ model	
           xpos, # 'from' states
             ... )
}
\arguments{
  \item{Lx}{A \code{\link{Lexis}} object representing cohort follow-up.
}
  \item{resp}{Character vector of states \bold{to} which a transition is
    considered an event. May also be an integer vector in which case the
    reference will be to the position of levels of \code{lex.Xst}. 
}
  \item{formula}{Model formula describing the model for the
    intensity. For \code{glm} and \code{gam}, the formula should be
    one-sided; for \code{coxph} the formula should be two-sided and have
    the name of the time-scale used for baseline as the l.h.s.
}
\item{xpos}{Character vector of states \bold{from} which transitions
    are considered. May also be an integer vector in which case the
    reference will be to the position of levels of \code{lex.Cst}. If
    missing (that is not supplied), the entire \code{Lexis} object is
    used in the modeling.
}
\item{link}{Link function used, allowed values are \code{log} (the
    default), \code{identity} and \code{sqrt}, see the family
    \code{\link{poisreg}}. 
}
\item{scale}{Scalar. \code{lex.dur} is divided by this number before
  analysis, so that you can get resultion rates on a scale of your wish. 
}
  \item{\dots}{Arguments passed on to the methods.
}
}
\details{
  The \code{glm} and \code{gam} models are fitted using the family
  \code{\link{poisreg}} which is a bit faster than the traditional
  \code{poisson} family. The response variable for this family is a
  two-column vector of events and person-time respectively, so the
  predictions, for example using \code{\link{ci.pred}} does not require
  \code{lex.dur} (and would ignore this) as variable in the
  \code{newdata}. \code{ci.pred} returns the estimated rates in the same
  units of the \code{lex.dur} in the \code{Lexis} object, scaled by
  \code{scale}.

  Strictly speaking, it is a bit counterintuitive to have the time-scale
  on the l.h.s. of the formula for the \code{coxph} since the time scale
  is also a predictor of the occurrence rate. On the other hand, calling
  \code{coxph} directly would also entail having the name of the time
  scale in the \code{Surv} object on the l.h.s. of the formula. So the
  inconsistency is just carried over from \code{coxph}.
}
\value{\code{glm} returns a \code{\link{glm}} object, \code{gam} returns
  a \code{\link[mgcv]{gam}} object and \code{coxph} returns a
  \code{\link[survival]{coxph}} object. The returned objects all have an
  extra attribute, \code{Lexis}; a list with names \code{Exposure} and
  \code{Events}; character vectors of state names from which transitions
  are modeled and that are considered events, respectively. The
  \code{coxph} object also has a \code{Timescale} element in the list.
}
\author{
Bendix Carstensen, \url{http://BendixCarstensen.com}.
}
\seealso{
\code{\link{Lexis}}, \code{\link{cutLexis}}
}
\examples{
library( Epi )
library( survival )
data( DMlate )

# Lexis object of total follow-up
mL <- Lexis( entry = list(age=dodm-dobth,per=dodm),
              exit = list(per=dox),
       exit.status = factor(!is.na(dodth),labels=c("Alive","Dead")),
              data = DMlate )

# Cut follow-up at start of insulin use
cL <- cutLexis( mL, cut = mL$doins,
              timescale = "per",
              new.state = "Ins",
       precursor.states = "Alive" )

# Split follow-up on age-axis
system.time( sL <- splitLexis( cL, breaks=0:100, time.scale="age") )
summary( sL )

# glm models for rates based on the time-split dataset by insulin and sex

# proportional hazards model with insulin as time-dependent variable
mt <- glm.Lexis( sL, resp="Dead",
                     ~ sex + lex.Cst + Ns(age,knots=c(15,3:8*10)) )

# mortality only among persons not on insulin
m0 <- glm.Lexis( sL, resp="Dead",
                     ~ sex + Ns(age,knots=c(15,3:8*10)),
                     xpos="Alive" )

# mortality only among insulin users
mi <- glm.Lexis( sL, resp="Dead",
                     ~ sex + Ns(age,knots=c(15,3:8*10)),
                     xpos="Ins" )

# rates of insulin initiation
ii <- glm.Lexis( sL, resp="Ins",
                     ~ sex + Ns(age,knots=c(15,3:8*10)),
                     xpos="Alive" )

# transition from Ins to Alive - none is in the dataset
ia <- glm.Lexis( sL, resp="Alive",
                     ~ sex + Ns(age,knots=c(15,3:8*10)),
                     xpos="Ins" )

# prediction of mortality rates from "Alive" with and without PH assumption
nA <- data.frame( age=40:70, sex="M", lex.Cst="Alive" )
nI <- data.frame( age=40:70, sex="M", lex.Cst="Ins" )
matshade( nA$age, cbind( ci.pred(m0,nA),
                         ci.pred(mt,nA),
                         ci.pred(mt,nI) )*1000, plot=TRUE,
          lwd=3, lty=1, log="y", col=c("black","blue","red"),
          xlab="Age", ylab="Mortality per 1000 PY" )

# Identical baseline haz for Alive->Ins and Ins->Dead (a bit goofy in this context)
cm <- glm.Lexis( sL, resp=c("Ins","Dead"),
                     ~ lex.Cst + Ns(age,knots=c(15,3:8*10)),
                     xpos=c("Alive","Ins") )

# Only ALive and Ins contain PY so 4th argument is actually superfluous: 
cm <- glm.Lexis( sL, c("Ins","Dead"),
                     ~ lex.Cst + Ns(age,knots=c(15,3:8*10)) )

# Using the class of sL:
cm.glm <- glm.Lexis( sL, c("Ins","Dead"), ~ lex.Cst + sex * Ns(age,knots=c(15,3:8*10)) )
 
# Simple multiplicative and additive models
mm.glm <- glm.Lexis( sL, "Dead", ~ sex, "Alive", scale=1000 )
ma.glm <- glm.Lexis( sL, "Dead", ~ sex, "Alive", scale=1000, link="identity" )
round( cbind( ci.exp(mm.glm), ci.exp(ma.glm,Exp=FALSE) ), 3 )

# Different parametrizations by different link functions
mm.glm <- glm.Lexis( sL, "Dead", ~ sex - 1 , "Alive", scale=1000 )
ma.glm <- glm.Lexis( sL, "Dead", ~ sex - 1 , "Alive", scale=1000, link="identity" )
round( cbind( ci.exp(mm.glm), ci.exp(ma.glm,Exp=FALSE) ), 2 )

# proportional hazards gam model
mt.gam <- gam.Lexis( sL, "Dead", ~ sex + lex.Cst + s(age), scale=1000 )

# Fit a Cox model with age as baseline time scale and insulin as time-dependent
lcox <- coxph.Lexis( sL, "Dead", age ~ sex + lex.Cst )
summary( lcox )
ci.exp( lcox )

# Pretty much the same results for regression paramters as the glm:
ci.exp( mt, subset="ex" )
ci.exp( mt.gam, subset="ex" )
}
\keyword{models}
