gen.exp <-
function( purchase, id="id", dop="dop", amnt="amnt", dpt="dpt",
                fu, doe="doe", dox="dox",
          push.max = Inf,
            breaks,
              lags = NULL,
           lag.dec = 1 )
{
# Make sure that fu has the right names
names( fu )[match(c(id,doe,dox),names(fu))] <- c("id","doe","dox")

# Let all vectors be local
# First sort them all
purchase <- purchase[order(purchase$id,purchase$dop),]
# Make local copies
id   <- factor( purchase[,id] )
dop  <- purchase[,dop]
amnt <- purchase[,amnt]
# The dpt
if( is.numeric(dpt) )
  {
  if( length(dpt)!=length(id) )
      dpt <- rep( dpt[1], nrow(purchase) )
  }
else
  {
  if( dpt %in% names(purchase) )
      dpt  <- purchase[,dpt]
  else
      dpt  <- rep( 1, nrow(purchase) )
  }

# Compute length of exposure periods
drug.dur  <- amnt/dpt
# Put the exposed period head to foot
new.start <- ave( dop     , id, FUN=min ) +
             ave( drug.dur, id, FUN = function(x){
                                              c(0,cumsum(x[-length(x)]))} )
# Move them out so that no start of a period is earlier than the dop
exp.start <- new.start + ave( dop-new.start, id,
                              FUN=function(x) cummax(pmax(x,0)) )
# Compute the pushes
push.one <- exp.start - dop
# Revise them to the maximally acceptable
push.adj <- pmin( push.one, push.max )
# Revise the starting dates of exposure
exp.start <- exp.start - push.one + push.adj
# Revise the durations to be at most equal to differences between starting dates
drug.dur  <- pmin( drug.dur,
                   ave( exp.start, id,
                        FUN = function(x) c(diff(x),Inf) ) )
# Compute the end of the intervals
exp.end   <- exp.start + drug.dur

# Amalgamate records of adjoining intervals on the same amnt
# and put in a simplified dataframe
eps.time <- 0.0001
eps.dpt  <- 0.0001
nr       <- nrow( purchase )
remove <- ( abs( exp.start[-1]-exp.end[-nr] ) < eps.time ) &
          ( abs(       dpt[-1]-    dpt[-nr] ) < eps.dpt  ) &
                      ( id[-1] ==   id[-nr] )
dfR <- data.frame( cbind(id,dpt,exp.start)[c(TRUE,!remove),],
                           exp.end=exp.end[c(!remove,TRUE)] )

# Follow-up intervals before first drug exposure
first.dfr <- data.frame( exp.end = zz <- with( dfR, tapply(exp.start,id,min) ),
                              id = factor( names( zz ) ),
                             dpt = 0 )
first.dfr <- merge( first.dfr, fu, all.x=TRUE )
names(first.dfr)[match("doe",names(first.dfr))] <- "exp.start"
first.dfr <- subset( first.dfr, exp.end > exp.start )
# Follow-up intervals after last drug exposure
last.dfr <- data.frame( exp.start = zz <- with( dfR, tapply(exp.end,id,max) ),
                               id = factor( names( zz ) ),
                              dpt = 0 )
last.dfr <- merge( last.dfr, fu, all.x=TRUE )
names(last.dfr)[match("dox",names(last.dfr))] <- "exp.end"
last.dfr <- subset( last.dfr, exp.end > exp.start )
# Intervals in the middle not covered by the drug exposures
gap <- with( dfR, exp.start[-1]-exp.end[-nrow(dfR)] > 0 )
holes.dfr <- with( dfR,
             data.frame( exp.end   = exp.start[c(FALSE,gap)],
                         exp.start = exp.end[c(gap,FALSE)],
                                id = id[c(FALSE,gap)],
                                di = id[c(gap,FALSE)],
                               dpt = 0 ) )
holes.dfr <- subset( holes.dfr, id==di )
cols <- c( "id", "dpt", "exp.start", "exp.end" )
dfR <- rbind( first.dfr[,cols],
               last.dfr[,cols],
              holes.dfr[,cols],
                    dfR[,cols] )
dfR <- dfR[order(dfR$id,dfR$exp.start),]
## So far, we have a datframe, dfR, with one record per epoch of particular
## exposure status for a given person
drug.L <-  Lexis( data = dfR,
                    id = id,
                 entry = list( dof=exp.start ),
                  exit = list( dof=exp.end ) )
# Then we split the records at breaks
drug.S <- splitLexis( drug.L,
                      breaks=breaks )
# Time since first start of the drug at the start of each interval
drug.S$evxp <- with( drug.S, ave( dpt>0,
                                  id,
                                  FUN=function(x) cumsum(x)>0 ) )
drug.S$tfi  <- with( drug.S, ave( lex.dur * evxp,
                                  id,
                                  FUN = function(x) c(0,cumsum(x)[-length(x)] ) ) )
# Time since last cessation of the drug at the start of each interval
drug.S$tfc  <- with( drug.S, ave( lex.dur * (dpt==0),
                                  id,
                                  FUN=function(x) ave( x,
                                                       factor(cumsum(x==0)),
                                                       FUN=cumsum ) ) )
drug.S$tfc  <- pmin( drug.S$tfc, drug.S$tfi )
# Cumulative time on a drug at the start of each interval
drug.S$cdur <- with( drug.S, ave( lex.dur * (dpt>0),
                                  id,
                                  FUN = function(x) c(0,cumsum(x)[-length(x)] ) ) )
# Cumulative dose-exposure on a drug at the start of each interval
drug.S$cdos <- with( drug.S, ave( lex.dur * dpt,
                                  id,
                                  FUN = function(x)
                                  c(0,cumsum(x)[-length(x)] ) ) )
# Cumulative dose-exposure at the lagged values
if( !is.null(lags) )
{
lnam <- paste( "lag", formatC(lags,format="f",digits=lag.dec), sep="." )
for( i in 1:length(lags) )
{
drug.Sl <- splitLexis( drug.L,
                       breaks=breaks-lags[i] )
drug.Sl <- drug.Sl[order(drug.Sl$id,drug.Sl$dof),]
drug.Sl[,lnam] <- with( drug.Sl, ave( dpt*lex.dur,
                                      factor(id),
                                      FUN = function(x) c(0,cumsum(x)[-length(x)] ) ) )
drug.Sl$dof <- drug.Sl$dof+lags[i]
drug.S  <- merge( drug.S, drug.Sl[,c("id","dof",lnam[i])], all.x=TRUE )
}
drug.S[,lnam][is.na(drug.S[,lnam])] <- 0
}
# Recover the exit date from the fu data frame - used only to compute
# FU in the last interval.
drug.S <- merge( drug.S, fu[,c("id","dox")] )
# Restrict to the times where we actually computed cumulative doses
drug.S <- drug.S[drug.S$dof %in% breaks,]
drug.S <- drug.S[order(drug.S$id,drug.S$dof),]
# Then the lengths of each of the follow-up intervals
drug.S$Y <- with( drug.S, ave( dof, id, FUN=function(x) diff(c(x,NA) ) ) )
drug.S$Y[is.na(drug.S$Y)] <- (drug.S$dox - drug.S$dof )[is.na(drug.S$Y)]
#Invisibl(
drug.S[order(drug.S$id,drug.S$dof),c("id","dof","Y","tfi","tfc",
                                     "cdur","cdos",lnam)]
}
