### * <HEADER>
###
attach(NULL, name = "CheckExEnv")
assign("nameEx", 
       local({
	   s <- "__{must remake R-ex/*.R}__"
           function(new) {
               if(!missing(new)) s <<- new else s
           }
       }),
       pos = "CheckExEnv")
## Add some hooks to label plot pages for base and grid graphics
assign("base_plot_hook",
       function() {
           pp <- par(c("mfg","mfcol","oma","mar"))
           if(all(pp$mfg[1:2] == c(1, pp$mfcol[2]))) {
               outer <- (oma4 <- pp$oma[4]) > 0; mar4 <- pp$mar[4]
               mtext(sprintf("help(\"%s\")", nameEx()), side = 4,
                     line = if(outer)max(1, oma4 - 1) else min(1, mar4 - 1),
              outer = outer, adj = 1, cex = .8, col = "orchid", las=3)
           }
       },
       pos = "CheckExEnv")
assign("grid_plot_hook",
       function() {
           pushViewport(viewport(width=unit(1, "npc") - unit(1, "lines"),
                                 x=0, just="left"))
           grid.text(sprintf("help(\"%s\")", nameEx()),
                     x=unit(1, "npc") + unit(0.5, "lines"),
                     y=unit(0.8, "npc"), rot=90,
                     gp=gpar(col="orchid"))
       },
       pos = "CheckExEnv")
setHook("plot.new",     get("base_plot_hook", pos = "CheckExEnv"))
setHook("persp",        get("base_plot_hook", pos = "CheckExEnv"))
setHook("grid.newpage", get("grid_plot_hook", pos = "CheckExEnv"))
assign("cleanEx",
       function(env = .GlobalEnv) {
	   rm(list = ls(envir = env, all.names = TRUE), envir = env)
           RNGkind("default", "default")
	   set.seed(1)
   	   options(warn = 1)
	   .CheckExEnv <- as.environment("CheckExEnv")
	   delayedAssign("T", stop("T used instead of TRUE"),
		  assign.env = .CheckExEnv)
	   delayedAssign("F", stop("F used instead of FALSE"),
		  assign.env = .CheckExEnv)
	   sch <- search()
	   newitems <- sch[! sch %in% .oldSearch]
	   for(item in rev(newitems))
               eval(substitute(detach(item), list(item=item)))
	   missitems <- .oldSearch[! .oldSearch %in% sch]
	   if(length(missitems))
	       warning("items ", paste(missitems, collapse=", "),
		       " have been removed from the search path")
       },
       pos = "CheckExEnv")
assign("ptime", proc.time(), pos = "CheckExEnv")
grDevices::postscript("Epi-Ex.ps")
assign("par.postscript", graphics::par(no.readonly = TRUE), pos = "CheckExEnv")
options(contrasts = c(unordered = "contr.treatment", ordered = "contr.poly"), pager="console")
options(warn = 1)    
library('Epi')

assign(".oldSearch", search(), pos = 'CheckExEnv')
assign(".oldNS", loadedNamespaces(), pos = 'CheckExEnv')
cleanEx(); nameEx("Icens");
### * Icens

flush(stderr()); flush(stdout())

### Name: Icens
### Title: Fits a regression model to interval censored data.
### Aliases: Icens print.Icens
### Keywords: models regression survival

### ** Examples

data( hivDK )
# Convert the dates to fractional years so that rates are
# expressed in cases per year
for( i in 2:4 ) hivDK[,i] <- cal.yr( hivDK[,i] )

m.RR <- Icens( entry, well, ill,
               model="MRR", formula=~pyr+us, breaks=seq(1980,1990,5),
               data=hivDK)
# Currently the MRR model returns a list with 2 glm objects.
round( ci.lin( m.RR$rates ), 4 )
round( ci.lin( m.RR$cov, Exp=TRUE ), 4 )
# There is actually a print method:
print( m.RR )

m.ER <- Icens( entry, well, ill,
               model="AER", formula=~pyr+us, breaks=seq(1980,1990,5),
               data=hivDK)
# There is actually a print method:
print( m.ER )
  


cleanEx(); nameEx("Lexis.diagram");
### * Lexis.diagram

flush(stderr()); flush(stdout())

### Name: Lexis.diagram
### Title: Plot a Lexis diagram
### Aliases: Lexis.diagram
### Keywords: hplot dplot

### ** Examples

Lexis.diagram( entry.age = c(3,30,45),
               risk.time = c(25,5,14),
              birth.date = c(1970,1931,1925.7),
                    fail = c(TRUE,TRUE,FALSE) )
LL <- Lexis.diagram( entry.age = sample( 0:50, 17, replace=TRUE ),
                     risk.time = sample( 5:40, 17, r=TRUE),
                    birth.date = sample( 1910:1980, 17, r=TRUE ),
                          fail = sample( 0:1, 17, r=TRUE ), 
                      cex.fail = 1.1,
                      lwd.life = 2 )
# Identify the persons' entry and exits
text( LL$exit.date, LL$exit.age, paste(1:nrow(LL)), col="red", font=2, adj=c(0,1) )
text( LL$entry.date, LL$entry.age, paste(1:nrow(LL)), col="blue", font=2, adj=c(1,0) )
data( nickel )
attach( nickel )
LL <- Lexis.diagram( age=c(10,100), date=c(1900,1990), 
                     entry.age=age1st, exit.age=ageout, birth.date=dob, 
                     fail=(icd %in% c(162,163)), lwd.life=1,
                     cex.fail=0.8, col.fail=c("green","red") )
abline( v=1934, col="blue" )
nickel[1:10,]
LL[1:10,]



cleanEx(); nameEx("Lexis.lines");
### * Lexis.lines

flush(stderr()); flush(stdout())

### Name: Lexis.lines
### Title: Draw life lines in a Lexis diagram.
### Aliases: Lexis.lines
### Keywords: hplot dplot

### ** Examples

Lexis.diagram( entry.age = c(3,30,45),
               risk.time = c(25,5,14),
              birth.date = c(1970,1931,1925.7),
                    fail = c(TRUE,TRUE,FALSE) )
Lexis.lines( entry.age = sample( 0:50, 100, replace=TRUE ),
             risk.time = sample( 5:40, 100, r=TRUE),
            birth.date = sample( 1910:1980, 100, r=TRUE ),
                  fail = sample(0:1,100,r=TRUE),
              cex.fail = 0.5,
              lwd.life = 1 )



cleanEx(); nameEx("Life.lines");
### * Life.lines

flush(stderr()); flush(stdout())

### Name: Life.lines
### Title: Compute dates/ages for life lines in a Lexis diagram
### Aliases: Life.lines
### Keywords: manip dplot

### ** Examples

( Life.lines( entry.age = c(3,30,45),
              risk.time = c(25,5,14),
             birth.date = c(1970,1931,1925.7) ) )

# Draw a Lexis diagram
Lexis.diagram()

# Compute entry and exit age and date.
( LL <-  Life.lines( entry.age = c(3,30,45),
                     risk.time = c(25,5,14),
                    birth.date = c(1970,1931,1925.7) ) )
segments( LL[,1], LL[,2], LL[,3], LL[,4] ) # Plot the life lines.

# Compute entry and exit age and date, supplying a date variable
bd <- ( c(1970,1931,1925.7) - 1970 ) * 365.25
class( bd ) <- "Date"
( Life.lines( entry.age = c(3,30,45),
              risk.time = c(25,5,14),
             birth.date = bd ) )



cleanEx(); nameEx("ROC");
### * ROC

flush(stderr()); flush(stdout())

### Name: ROC
### Title: Function to compute and draw ROC-curves.
### Aliases: ROC
### Keywords: manip htest

### ** Examples

x <- rnorm( 100 )
z <- rnorm( 100 )
w <- rnorm( 100 )
tigol <- function( x ) 1 - ( 1 + exp( x ) )^(-1)
y <- rbinom( 100, 1, tigol( 0.3 + 3*x + 5*z + 7*w ) )
ROC( form = y ~ x + z, plot="ROC" )



cleanEx(); nameEx("Relevel");
### * Relevel

flush(stderr()); flush(stdout())

### Name: Relevel
### Title: Reorder and combine levels of a factor
### Aliases: Relevel
### Keywords: manip

### ** Examples

ff <- factor( sample( letters[1:5], 100, replace=TRUE ) )
table( ff, Relevel( ff, list( AB=1:2, "Dee"=4, c(3,5) ) ) )
table( ff, rr=Relevel( ff, list( 5:4, Z=c("c","a") ), coll="-und-", first=FALSE ) )



cleanEx(); nameEx("S.typh");
### * S.typh

flush(stderr()); flush(stdout())

### Name: S.typh
### Title: Salmonella Typhimurium outbreak 1996 in Denmark.
### Aliases: S.typh
### Keywords: datasets

### ** Examples

data(S.typh)



cleanEx(); nameEx("W.Lexis");
### * W.Lexis

flush(stderr()); flush(stdout())

### Name: W.Lexis
### Title: Split follow-up time in cohort studies.
### Aliases: W.Lexis
### Keywords: manip

### ** Examples

# A small bogus cohort
#
xcoh <- structure( list( id = c("A", "B", "C"),
                      birth = c("14/07/1952", "01/04/1954", "10/06/1987"),
                      entry = c("04/08/1965", "08/09/1972", "23/12/1991"),
                       exit = c("27/06/1997", "23/05/1995", "24/07/1998"),
                       fail = c(1, 0, 1) ),
                     .Names = c("id", "birth", "entry", "exit", "fail"),
                  row.names = c("1", "2", "3"),
                      class = "data.frame" )

# Convert the character dates into numerical variables (fractional years)
#
xcoh$bt <- cal.yr( xcoh$birth, format="%d/%m/%Y" )
xcoh$en <- cal.yr( xcoh$entry, format="%d/%m/%Y" )
xcoh$ex <- cal.yr( xcoh$exit , format="%d/%m/%Y" )

# See how it looks
#
xcoh 

# Split time along one time-axis
#
W.Lexis( entry = en,
          exit = ex,
          fail = fail,
         scale = 1,
        origin = bt,
        breaks = seq( 5, 40, 5 ),
       include = list( bt, en, ex, id ),
          data = xcoh )

# Split time along two time-axes
#
( x2 <- 
W.Lexis( entry = en,
          exit = ex,
          fail = fail,
         scale = 1,
        origin = list( per=0,                 age=bt          ),
        breaks = list( per=seq(1900,2000,10), age=seq(0,80,5) ),
       include = list( bt, en, ex, id ),
          data = xcoh ) )

# Tabulate the cases and the person-years
#
tapply( x2$Fail, list( x2$age, x2$per ), sum )
tapply( x2$Exit - x2$Entry, list( x2$age, x2$per ), sum )



cleanEx(); nameEx("apc.fit");
### * apc.fit

flush(stderr()); flush(stdout())

### Name: apc.fit
### Title: Fit an Age-Period-Cohort model to tabular data.
### Aliases: apc.fit
### Keywords: models regression

### ** Examples

library( Epi )
data(lungDK)

# Taylor a dataframe that meets the requirements
exd <- lungDK[,c("Ax","Px","D","Y")]
names(exd)[1:2] <- c("A","P")

# Two different ways of parametrizing the APC-model, ML
ex.H <- apc.fit( exd, npar=7, model="ns", dr.extr="Holford",  parm="ACP", scale=10^5 )
ex.W <- apc.fit( exd, npar=7, model="ns", dr.extr="weighted", parm="ACP", scale=10^5 )

# Sequential fit, first AC, then P given AC.
ex.S <- apc.fit( exd, npar=7, model="ns", parm="AC-P", scale=10^5 )

# Show the estimated drifts
ex.H[["Drift"]]
ex.W[["Drift"]]
ex.S[["Drift"]]

# Plot the effects
fp <- apc.plot( ex.H )
apc.lines( ex.W, frame.par=fp, col="red" )
apc.lines( ex.S, frame.par=fp, col="blue" )



cleanEx(); nameEx("apc.frame");
### * apc.frame

flush(stderr()); flush(stdout())

### Name: apc.frame
### Title: Produce an empty frame for display of parameter-estimates from
###   Age-Period-Cohort-models.
### Aliases: apc.frame
### Keywords: hplot

### ** Examples

par( mar=c(4,4,1,4) )
res <-
apc.frame( a.lab=seq(30,90,20), cp.lab=seq(1880,2000,30), r.lab=c(1,2,5,10,20,50),
           a.tic=seq(30,90,10), cp.tic=seq(1880,2000,10), r.tic=c(1:10,1:5*10),
           gap=27 )
res
# What are the axes actually?
par(c("usr","xlog","ylog"))
# How to plot in the age-part: a point at (50,10)
points( 50, 10, pch=16, cex=2, col="blue" )
# How to plot in the cohort-period-part: a point at (1960,0.3)
points( 1960-res[1], 0.3*res[2], pch=16, cex=2, col="red" )



graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx(); nameEx("apc.plot");
### * apc.plot

flush(stderr()); flush(stdout())

### Name: apc.plot
### Title: Plot the estimates from a fitted Age-Period-Cohort model
### Aliases: apc.plot
### Keywords: hplot

### ** Examples

data( lungDK )
attach( lungDK )
apc1 <- apc.fit( A=Ax, P=Px, D=D, Y=Y/10^5 )
fp <- apc.plot( apc1 )
apc.lines( apc1, frame.par=fp, drift=1.01, col="red" )
for( i in 1:11 )
  apc.lines( apc1, frame.par=fp, drift=1+(i-6)/100, col=rainbow(12)[i] )



cleanEx(); nameEx("bdendo");
### * bdendo

flush(stderr()); flush(stdout())

### Name: bdendo
### Title: A case-control study of endometrial cancer
### Aliases: bdendo
### Keywords: datasets

### ** Examples

data(bdendo)



cleanEx(); nameEx("bdendo11");
### * bdendo11

flush(stderr()); flush(stdout())

### Name: bdendo11
### Title: A 1:1 subset of the endometrial cancer case-control study
### Aliases: bdendo11
### Keywords: datasets

### ** Examples

data(bdendo11)



cleanEx(); nameEx("births");
### * births

flush(stderr()); flush(stdout())

### Name: births
### Title: Births in a London Hospital
### Aliases: births
### Keywords: datasets

### ** Examples

data(births)



cleanEx(); nameEx("blcaIT");
### * blcaIT

flush(stderr()); flush(stdout())

### Name: blcaIT
### Title: Bladder cancer mortality in Italian males
### Aliases: blcaIT
### Keywords: datasets

### ** Examples

data(blcaIT)



cleanEx(); nameEx("brv");
### * brv

flush(stderr()); flush(stdout())

### Name: brv
### Title: Bereavement in an elderly cohort
### Aliases: brv
### Keywords: datasets

### ** Examples

data(brv)



cleanEx(); nameEx("cal.yr");
### * cal.yr

flush(stderr()); flush(stdout())

### Name: cal.yr
### Title: Functions to convert character, factor and various date objects
###   into a number, and vice versa.
### Aliases: cal.yr as.Date.cal.yr as.Date.numeric
### Keywords: manip chron

### ** Examples

 # Charcter vector of dates:
 birth <- c("14/07/1852","01/04/1954","10/06/1987","16/05/1990",
            "01/01/1996","01/01/1997","01/01/1998","01/01/1999")
 # Proper conversion to class "Date":
 birth.dat <- as.Date( birth, format="%d/%m/%Y" )
 # Converson of character to class "cal.yr"
 bt.yr <- cal.yr( birth, format="%d/%m/%Y" )
 # Back to class "Date":
 bt.dat <- as.Date( bt.yr )
 # Numerical calculation of days since 1.1.1970:
 days <- Days <- (bt.yr-1970)*365.25
 # Blunt assignment of class:
 class( Days ) <- "Date"
 # Then data.frame() to get readable output of results:
 data.frame( birth, birth.dat, bt.yr, bt.dat, days, Days, round(Days) )



cleanEx(); nameEx("ccwc");
### * ccwc

flush(stderr()); flush(stdout())

### Name: ccwc
### Title: Generate a nested case-control study
### Aliases: ccwc
### Keywords: datagen

### ** Examples

#
# For the diet and heart dataset, create a nested case-control study
# using the age scale and matching on job
#
data(diet)
dietcc <- ccwc(doe, dox, chd, origin=dob, controls=2, data=diet,
               include=energy, match=job)



cleanEx(); nameEx("ci.cum");
### * ci.cum

flush(stderr()); flush(stdout())

### Name: ci.cum
### Title: Compute cumulative sum of estimates.
### Aliases: ci.cum
### Keywords: models regression

### ** Examples

# Packages required for this example
library( splines )
library( survival )
data( lung )
par( mfrow=c(1,2) )

# Plot the Kaplan-meier-estimator
#
plot( survfit( Surv( time, status==2 ), data=lung ) )

# Cut the follow-up every 10 days
#
dx <- W.Lexis( exit=time, fail=(status==2), breaks=seq(0,1100,10), data=lung )
str( dx )

# Fit a Poisson model with a natural spline for the effect of time.
# Note that "Time" is the left endpoint of the interval in the dataframe dx.
#
MM <- ns( dx$Time, knots=c(50,100,200,400,700), intercept=TRUE )
mp <- glm( Fail ~ MM - 1 + offset(log(Exit-Entry)),
                  family=poisson, data=dx, eps=10^-8, maxit=25 )

# Contrast matrix to extract effects, i.e. matrix to multiply with the
# coefficients to produce the log-rates: unique rows of MM, in time order.
#
T.pt <- sort( unique( dx$Time ) )
T.wh <- match( T.pt, dx$Time )
Lambda <- ci.cum( mp, ctr.mat=MM[T.wh,], intl=diff(c(0,T.pt)) )

# Put the estimated survival function on top of the KM-estimator
#
matlines( c(0,T.pt[-1]), exp(-Lambda[,1:3]), lwd=c(3,1,1), lty=1, col="Red" )

# Extract and plot the fitted intensity function
#
lambda <- ci.lin( mp, ctr.mat=MM[T.wh,], Exp=TRUE )
matplot( T.pt, lambda[,5:7]*10^3, type="l", lwd=c(3,1,1), col="black", lty=1,
         log="y", ylim=c(0.2,20) )



graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx(); nameEx("ci.lin");
### * ci.lin

flush(stderr()); flush(stdout())

### Name: ci.lin
### Title: Compute linear functions of parameters with s.e.
### Aliases: ci.lin
### Keywords: models regression

### ** Examples

# Bogus data:
f <- factor( sample( letters[1:5], 200, replace=TRUE ) )
g <- factor( sample( letters[1:3], 200, replace=TRUE ) )
x <- rnorm( 200 )
y <- 7 + as.integer( f ) * 3 + 2 * x + 1.7 * rnorm( 200 )

# Fit a simple model:
mm <- lm( y ~ x + f + g )
ci.lin( mm ) 
ci.lin( mm, subset=3:6, diff=TRUE, fnam=FALSE )
ci.lin( mm, subset=3:6, diff=TRUE, fnam=TRUE )
ci.lin( mm, subset="f", diff=TRUE, fnam="f levels:" )
print( ci.lin( mm, subset="g", diff=TRUE, fnam="gee!:", vcov=TRUE ) )

# Use character defined subset to get ALL contrasts:
ci.lin( mm, subset="f", diff=TRUE )



cleanEx(); nameEx("ci.pd");
### * ci.pd

flush(stderr()); flush(stdout())

### Name: ci.pd
### Title: Compute confidence limits for a difference of two independent
###   proportions.
### Aliases: ci.pd
### Keywords: distribution htest

### ** Examples

( a <- matrix( sample( 10:40, 4 ), 2, 2 ) )
ci.pd( a )
twoby2( t(a) )
prop.test( t(a) )
( A <- array( sample( 10:40, 20 ), dim=c(2,2,5) ) )
ci.pd( A )
ci.pd( A, detail.labs=TRUE, digits=3 )



cleanEx(); nameEx("diet");
### * diet

flush(stderr()); flush(stdout())

### Name: diet
### Title: Diet and heart data
### Aliases: diet
### Keywords: datasets

### ** Examples

data(diet)
# Illustrate the follow-up in a Lexis diagram
Lexis.diagram( age=c(30,75), date=c(1965,1990),
               entry.date=cal.yr(doe), exit.date=cal.yr(dox), birth.date=cal.yr(dob), 
               fail=(fail>0), pch.fail=c(NA,16), col.fail=c(NA,"red"), cex.fail=1.0,
               data=diet )




cleanEx(); nameEx("effx");
### * effx

flush(stderr()); flush(stdout())

### Name: effx
### Title: Function to calculate effects
### Aliases: effx
### Keywords: models regression

### ** Examples

library(Epi)
data(births)
births$hyp <- factor(births$hyp,labels=c("normal","hyper"))
births$sex <- factor(births$sex,labels=c("M","F"))

# bweight is the birth weight of the baby in gms, and is a metric
# response (the default) 

# effect of hypertension on birth weight
effx(bweight,exposure=hyp,data=births) 
# effect of hypertension on birth weight stratified by sex
effx(bweight,exposure=hyp,strata=sex,data=births) 
# effect of hypertension on birth weight controlled for sex
effx(bweight,exposure=hyp,control=sex,data=births) 
# effect of gestation time on birth weight
effx(bweight,exposure=gestwks,data=births) 
# effect of gestation time on birth weight stratified by sex
effx(bweight,exposure=gestwks,strata=sex,data=births) 
# effect of gestation time on birth weight controlled for sex
effx(bweight,exposure=gestwks,control=sex,data=births) 

# lowbw is a binary response coded 1 for low birth weight and 0 otherwise
# effect of hypertension on low birth weight
effx(lowbw,type="binary",exposure=hyp,data=births)
# etc.



cleanEx(); nameEx("effx.match");
### * effx.match

flush(stderr()); flush(stdout())

### Name: effx.match
### Title: Function to calculate effects for individually matched
###   case-control studies
### Aliases: effx.match
### Keywords: models regression

### ** Examples

library(Epi)
library(survival)
data(bdendo)

# d is the case-control variable, set is the matching variable.
# The variable est is a factor and refers to estrogen use (yes,no)
# The variable age is numeric and refers to estrogen use (yes,no)
# effect of est on the odds of being a case
effx.match(d,exposure=est,match=set,data=bdendo)
# effect of age on the odds of being a case
effx.match(d,exposure=age,match=set,data=bdendo)



cleanEx(); nameEx("ewrates");
### * ewrates

flush(stderr()); flush(stdout())

### Name: ewrates
### Title: Rates of lung and nasal cancer mortality, and total mortality.
### Aliases: ewrates
### Keywords: datasets

### ** Examples

data(ewrates)
str(ewrates)



cleanEx(); nameEx("ex1");
### * ex1

flush(stderr()); flush(stdout())

### Name: ex1
### Title: Split follow-up time along a timescale
### Aliases: ex1
### Keywords: manip datagen

### ** Examples

one <- round( runif( 15, 0, 10 ), 1 )
two <- round( runif( 15, 0, 10 ), 1 )
doe <- pmin( one, two )
dox <- pmax( one, two )
# Goofy data rows to test possibly odd behaviour
doe[1:3] <- dox[1:3] <- 8
dox[2] <- 6
dox[3] <- 7.5
# Some failure indicators
fail <- sample( 0:1, 15, replace=TRUE, prob=c(0.7,0.3) )
# Split follow-up:
ex1( doe, dox, fail, breaks=0:10 )



cleanEx(); nameEx("expand.data");
### * expand.data

flush(stderr()); flush(stdout())

### Name: expand.data
### Title: Function to expand data for regression analysis of interval
###   censored data.
### Aliases: expand.data
### Keywords: models regression survival

### ** Examples

  


cleanEx(); nameEx("fcut");
### * fcut

flush(stderr()); flush(stdout())

### Name: fcut
### Title: Cuts follow-up time at multiple failure times.
### Aliases: fcut
### Keywords: manip datagen

### ** Examples

one <- round( runif( 15, 0, 10 ), 1 )
two <- round( runif( 15, 0, 10 ), 1 )
doe <- pmin( one, two )
dox <- pmax( one, two )
# Goofy data rows to test possibly odd behaviour
doe[1:3] <- dox[1:3] <- 8
dox[2] <- 6
dox[3] <- 7.5
# Some failure indicators
fail <- sample( 0:1, 15, replace=TRUE, prob=c(0.7,0.3) )
# Failure times in a list
dof <- sample( c(one,two), 15 )
l.dof <- list( f1=sample( c(one,two), 15 ),
               f2=sample( c(one,two), 15 ),
               f3=sample( c(one,two),15 ) )
# The same, but with events prior to entry removed
lx.dof <- lapply( l.dof, FUN=function(x){ x[x<doe] <- NA ; x } )
# So what have we got
data.frame( doe, dox, fail, l.dof, lx.dof )
# Cut follow-up at event times
fcut( doe, dox, lx.dof, fail, data=data.frame( doe, dox, lx.dof ) )



cleanEx(); nameEx("fcut1");
### * fcut1

flush(stderr()); flush(stdout())

### Name: fcut1
### Title: Cut follow-up time at a failure time.
### Aliases: fcut1
### Keywords: manip datagen

### ** Examples

one <- round( runif( 15, 0, 10 ), 1 )
two <- round( runif( 15, 0, 10 ), 1 )
doe <- pmin( one, two )
dox <- pmax( one, two )
# Goofy data rows to test possibly odd behaviour
doe[1:3] <- dox[1:3] <- 8
dox[2] <- 6
dox[3] <- 7.5
# Some failure indicators and failure times
fail <- sample( 0:1, 15, replace=TRUE, prob=c(0.7,0.3) )
dof <- sample( c(one,two), 15 )
# So what have we got
data.frame( doe, dox, fail, dof )
# Cut follow-up at dof
fcut1( doe, dox, fail, dof )



cleanEx(); nameEx("fit.add");
### * fit.add

flush(stderr()); flush(stdout())

### Name: fit.add
### Title: Fit an addive excess risk model to interval censored data.
### Aliases: fit.add
### Keywords: models regression survival

### ** Examples

  data( HIV.dk ) 
  


cleanEx(); nameEx("fit.baseline");
### * fit.baseline

flush(stderr()); flush(stdout())

### Name: fit.baseline
### Title: Fit a piecewise contsnt intesity model for interval censored
###   data.
### Aliases: fit.baseline
### Keywords: models regression survival

### ** Examples

  


cleanEx(); nameEx("fit.mult");
### * fit.mult

flush(stderr()); flush(stdout())

### Name: fit.mult
### Title: Fits a multiplicative relative risk model to interval censored
###   data.
### Aliases: fit.mult
### Keywords: models regression survival

### ** Examples

  data( HIV.dk ) 
  


cleanEx(); nameEx("gmortDK");
### * gmortDK

flush(stderr()); flush(stdout())

### Name: gmortDK
### Title: Population mortality rates for Denmark in 5-years age groups.
### Aliases: gmortDK
### Keywords: datasets

### ** Examples

data(gmortDK)



cleanEx(); nameEx("hivDK");
### * hivDK

flush(stderr()); flush(stdout())

### Name: hivDK
### Title: hivDK: seroconversion in a cohort of Danish men
### Aliases: hivDK
### Keywords: datasets

### ** Examples

  data(hivDK)
  str(hivDK) 
  


cleanEx(); nameEx("icut");
### * icut

flush(stderr()); flush(stdout())

### Name: icut
### Title: Function to cut the follow-up in cohort at a point in time.
### Aliases: icut
### Keywords: manip datagen

### ** Examples

one <- round( runif( 15, 0, 15 ), 1 )
two <- round( runif( 15, 0, 15 ), 1 )
doe <- pmin( one, two )
dox <- pmax( one, two )
# Goofy data rows to test possibly odd behaviour
doe[1:3] <- dox[1:3] <- 8
dox[2] <- 6
dox[3] <- 7.5
# Some failure indicators
fail <- sample( 0:1, 15, replace=TRUE, prob=c(0.7,0.3) )
# So what have we got
data.frame( doe, dox, fail )
# Cut follow-up at 5
icut( doe, dox, fail, cut=5 )



cleanEx(); nameEx("isec");
### * isec

flush(stderr()); flush(stdout())

### Name: isec
### Title: Determine the intersection between follow-up intervals and a
###   fixed interval.
### Aliases: isec
### Keywords: manip datagen

### ** Examples

one <- round( runif( 15, 0, 15 ), 1 )
two <- round( runif( 15, 0, 15 ), 1 )
doe <- pmin( one, two )
dox <- pmax( one, two )
# Goofy data rows to test possibly odd behaviour
doe[1:3] <- dox[1:3] <- 8
dox[2] <- 6
dox[3] <- 7.5
# Some failure indicators
fail <- sample( 0:1, 15, replace=TRUE, prob=c(0.7,0.3) )
# So what have we got?
data.frame( doe, dox, fail )
# Find intersection with interval (4,8)
isec( doe, dox, fail, int=c(4,8) )
# See how it compares to original data
merge( data.frame( Expand=1:15, doe, dox, fail ),
       data.frame( isec( doe, dox, fail, int=c(4,8) ) ), all=TRUE )
  


cleanEx(); nameEx("lep");
### * lep

flush(stderr()); flush(stdout())

### Name: lep
### Title: An unmatched case-control study of leprosy incidence
### Aliases: lep
### Keywords: datasets

### ** Examples

data(lep)



cleanEx(); nameEx("lungDK");
### * lungDK

flush(stderr()); flush(stdout())

### Name: lungDK
### Title: Male lung cancer incidence in Denmark
### Aliases: lungDK
### Keywords: datasets

### ** Examples

data( lungDK )
# Draw a Lexis diagram and show the number of cases in it.
attach( lungDK )
Lexis.diagram( age=c(40,90), date=c(1943,1993), coh.grid=TRUE )
text( Px, Ax, paste( D ), cex=0.7 )



cleanEx(); nameEx("mh");
### * mh

flush(stderr()); flush(stdout())

### Name: mh
### Title: Mantel-Haenszel analyses of cohort and case-control studies
### Aliases: mh
### Keywords: htest

### ** Examples

# If d and y are 3-way tables of cases and person-years 
# observation formed by tabulation by two confounders 
# (named "C1" and "C2") an exposure of interest ("E"), 
# the following command will calculate an overall 
# Mantel-Haenszel comparison of the first two exposure 
# groups.
#
# Generate some bogus data
dnam <- list( E=c("low","medium","high"), C1=letters[1:2], C2=LETTERS[1:4] )
d <- array( sample( 2:80, 24),
            dimnames=dnam, dim=sapply( dnam, length ) )
y <- array( abs( rnorm( 24, 227, 50 ) ),
            dimnames=dnam, dim=sapply( dnam, length ) )
mh(d, y, compare="E")
#
# Or, if exposure levels named "low" and "high" are to be 
# compared and these are not the first two levels of E :
#
mh(d, y, compare="E", levels=c("low", "high"))
#
# If we wish to carry out an analysis which controls for C1, 
# but examines the results at each level of C2:
#
mh(d, y, compare="E", by="C2")
#
# It is also possible to look at rate ratios for every 
# combination of C1 and C2 :
#
mh(d, y, compare="E", by=c("C1", "C2"))
#
# If dimensions and levels of the table are unnamed, they must 
# be referred to by number.
#



cleanEx(); nameEx("mortDK");
### * mortDK

flush(stderr()); flush(stdout())

### Name: mortDK
### Title: Population mortality rates for Denmark in 1-year age-classes.
### Aliases: mortDK
### Keywords: datasets

### ** Examples

data(mortDK)



cleanEx(); nameEx("ncut");
### * ncut

flush(stderr()); flush(stdout())

### Name: ncut
### Title: Function to group a variable in intervals.
### Aliases: ncut
### Keywords: manip

### ** Examples

br <- c(-2,0,1,2.5)
x <- c( rnorm( 10 ), br, -3, 3 )
cbind( x, l=ncut( x, breaks=br, type="l" ),
          m=ncut( x, breaks=br, type="m" ),
          r=ncut( x, breaks=br, type="r" ) )[order(x),]
x <- rnorm( 200 )
plot( x, ncut( x, breaks=br, type="l" ), pch=16, col="blue", ylim=range(x) )
abline( 0, 1 )
abline( v=br )
points( x, ncut( x, breaks=br, type="r" ), pch=16, col="red" )
points( x, ncut( x, breaks=br, type="m" ), pch=16, col="green" )



cleanEx(); nameEx("nice");
### * nice

flush(stderr()); flush(stdout())

### Name: nice
### Title: Nice breakpoints
### Aliases: nice
### Keywords: manip

### ** Examples

nice( exp( rnorm( 100 ) ), log=TRUE )



cleanEx(); nameEx("nickel");
### * nickel

flush(stderr()); flush(stdout())

### Name: nickel
### Title: A Cohort of Nickel Smelters in South Wales
### Aliases: nickel
### Keywords: datasets

### ** Examples

data(nickel)
str(nickel)



cleanEx(); nameEx("pctab");
### * pctab

flush(stderr()); flush(stdout())

### Name: pctab
### Title: Create percentages in a table
### Aliases: pctab
### Keywords: manip methods array

### ** Examples

Aye <- sample( c("Yes","Si","Oui"), 177, replace=TRUE )
Bee <- sample( c("Hum","Buzz"), 177, replace=TRUE )
Sea <- sample( c("White","Black","Red","Dead"), 177, replace=TRUE )
A <- table( Aye, Bee, Sea )
A
ftable( pctab( A ) )
ftable( pctab( addmargins( A, 1 ), 3 ) )
round( ftable( pctab( addmargins( A, 1 ), 3 ), row.vars=3 ), 1)



cleanEx(); nameEx("plotEst");
### * plotEst

flush(stderr()); flush(stdout())

### Name: plotEst
### Title: Plot estimates with confidence limits
### Aliases: plotEst pointsEst linesEst
### Keywords: hplot models

### ** Examples

# Bogus data and a linear model
f <- factor( sample( letters[1:5], 100, replace=TRUE ) )
x <- rnorm( 100 )
y <- 5 + 2 * as.integer( f ) + 0.8 * x + rnorm(100) * 2
m1 <- lm( y ~ f )

# Produce some confidence intervals for contrast to first level
( cf <- summary( m1 )$coef[2:5,1:2] %*% rbind( c(1,1,1), 1.96*(c(0,-1,1) ) ) )

# Plots with increasing amount of bells and whistles
par( mfcol=c(3,2), mar=c(3,3,2,1) )
plotEst( cf )
plotEst( cf, grid=TRUE )
plotEst( cf, grid=TRUE, cex=2, lwd=3 )
plotEst( cf, grid=TRUE, cex=2, col.points="red", col.lines="green" )
plotEst( cf, grid=TRUE, cex=2, col.points="red", col.lines="green",
          xlog=TRUE, xtic=c(1:8), xlim=c(0.8,6) )
rownames( cf )[1] <- "Contrast to fa:\n\n fb"
plotEst( cf, grid=TRUE, cex=2, col.points=rainbow(4), col.lines=rainbow(4), vref=1 )
  


graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx(); nameEx("rateplot");
### * rateplot

flush(stderr()); flush(stdout())

### Name: rateplot
### Title: Functions to plot rates from a table classified by age and
###   calendar time (period)
### Aliases: rateplot Aplot Pplot Cplot
### Keywords: hplot

### ** Examples

data( blcaIT )
attach(blcaIT)

# Table of rates:
bl.rate <- tapply( D, list(age,period), sum ) /
           tapply( Y, list(age,period), sum )
bl.rate

# The four classical plots:
par( mfrow=c(2,2) )
rateplot( bl.rate*10^6 )

# The labels on the vertical axis could be nicer:
rateplot( bl.rate*10^6, at=10^(-1:3), labels=c(0.1,1,10,100,1000) ) 

# More bells an whistles
par( mfrow=c(1,3), mar=c(3,3,1,1), oma=c(0,3,0,0), mgp=c(3,1,0)/1.6 )
rateplot( bl.rate*10^6, ylab="", ann=TRUE, which=c("AC","PA","CA"),
                      at=10^(-1:3), labels=c(0.1,1,10,100,1000),
                      col=topo.colors(11), cex.ann=1.2 ) 



graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx(); nameEx("stattable");
### * stattable

flush(stderr()); flush(stdout())

### Name: stat.table
### Title: Tables of summary statistics
### Aliases: stat.table print.stat.table
### Keywords: iteration category

### ** Examples

data(warpbreaks)
# A one-way table
stat.table(tension,list(count(),mean(breaks)),data=warpbreaks)
# The same table with informative labels
stat.table(index=list("Tension level"=tension),list(N=count(),
           "mean number of breaks"=mean(breaks)),data=warpbreaks)

# A two-way table
stat.table(index=list(tension,wool),mean(breaks),data=warpbreaks)  
# The same table with margins over tension, but not wool
stat.table(index=list(tension,wool),mean(breaks),data=warpbreaks,
           margins=c(TRUE, FALSE))

# A table of column percentages
stat.table(list(tension,wool), percent(tension), data=warpbreaks)
# Cell percentages, with margins
stat.table(list(tension,wool),percent(tension,wool), margin=TRUE,
           data=warpbreaks)

# A table with multiple statistics
# Note how each statistic has its own default precision
a <- stat.table(index=list(wool,tension),
                contents=list(count(),mean(breaks),percent (wool)),
                data=warpbreaks)
print(a)
# Print the percentages rounded to the nearest integer
print(a, digits=c(percent=0))

# An Epidemiological example with follow-up time
data(nickel)
str(nickel)

# Make a grouped version of the exposure variable
nickel$egr <- cut( nickel$exposure, breaks=c(0, 0.5, 5, 10, 100), right=FALSE )
stat.table( egr, list( count(), percent(egr), mean( age1st ) ), data=nickel )

# Split the follow-up time by current age
nickel.ex <-
W.Lexis( entry=agein, exit=ageout, fail=icd %in% c(162,163),
         origin=0, breaks=seq(0,100,20),
         include=list( id, exposure, egr, age1st, icd ), data=nickel )
str( nickel.ex )

# Table of rates
stat.table( Time, list( n=count(), N=count(id), D=sum(Fail),
                        "Rate/1000"=ratio(Fail,Exit-Entry,1000) ),
            margin=1, data=nickel.ex )
# Two-way table of rates and no. persons contributing
stat.table( list(age=Time, Exposure=egr),
            list( N=count(id), D=sum(Fail), Y=sum((Exit-Entry)/1000),
                  Rate=ratio(Fail,Exit-Entry,1000) ),
            margin=TRUE, data=nickel.ex )



cleanEx(); nameEx("tabplot");
### * tabplot

flush(stderr()); flush(stdout())

### Name: tabplot
### Title: Graphical display of a 2-way contingency table
### Aliases: tabplot
### Keywords: hplot

### ** Examples

b <- sample( letters[1:4], 300, replace=TRUE, prob=c(3,1,2,4)/10 )
a <- rnorm( 300 ) - as.integer( factor( b ) ) / 8
tb <- table( cut( a, -3:2 ), b )
tabplot( tb )
tabplot( tb, rowlabs="right", col=heat.colors )

# Very similar plots
ptb <- sweep( tb, 2, apply( tb, 2, sum ), "/" )
par( mfrow=c(2,2) )
barplot( ptb, space=0 )
tabplot( tb, equal=TRUE, lwd=1 )
tabplot( tb, equal=TRUE, lwd=1, rowlabs="l" )
tabplot( tb, equal=FALSE, lwd=1, rowlabs="l" )



graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx(); nameEx("thoro");
### * thoro

flush(stderr()); flush(stdout())

### Name: thoro
### Title: Thorotrast Study
### Aliases: thoro
### Keywords: datasets

### ** Examples

data(thoro)
str(thoro)



cleanEx(); nameEx("twoby2");
### * twoby2

flush(stderr()); flush(stdout())

### Name: twoby2
### Title: Analysis of a two by two table
### Aliases: twoby2
### Keywords: univar htest

### ** Examples

Treat <- sample(c("A","B"), 50, rep=TRUE )
Resp <- c("Yes","No")[1+rbinom(50,1,0.3+0.2*(Treat=="A"))]
twoby2( Treat, Resp )                 
twoby2( table( Treat, Resp )[,2:1] ) # Comparison the other way round



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
