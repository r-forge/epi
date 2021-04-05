### R code from vignette source 'crisk.rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: crisk.rnw:25-28
###################################################
options( width=90,
         SweaveHooks=list( fig=function()
         par(mar=c(3,3,1,1),mgp=c(3,1,0)/1.6,las=1,bty="n") ) )


###################################################
### code chunk number 2: crisk.rnw:84-101
###################################################
library(Epi)
library(popEpi)
data(DMlate)
Ldm <- Lexis(entry = list( per = dodm,
                           age = dodm-dobth, 
                           tfd = 0 ),
              exit = list( per = dox ),
       exit.status = factor( !is.na(dodth), labels = c("DM","Dead") ),
              data = DMlate )
summary(Ldm, t = T)
Mdm <- mcutLexis( Ldm,
                   wh = c('dooad','doins'),
           new.states = c('OAD','Ins'),
            precursor = 'DM',
           seq.states = FALSE,
                 ties = TRUE )
summary( Mdm )


###################################################
### code chunk number 3: crisk.rnw:106-108
###################################################
Sdm <- splitMulti(factorize(subset(Mdm, lex.Cst == "DM")),
                  tfd = seq(0, 20, 1/12))


###################################################
### code chunk number 4: boxes5
###################################################
boxes(Mdm, boxpos = list(x = c(15, 50, 15, 85, 85),
                         y = c(85, 50, 15, 85, 15)), 
          scale.R = 100, 
          show.BE = TRUE)


###################################################
### code chunk number 5: boxes4
###################################################
boxes(Relevel(Sdm, c(1, 4, 2, 3)), 
      boxpos  = list(x = c(15, 85, 80, 15),
                     y = c(85, 85, 20, 15)),
      scale.R = 100, 
      show.BE = TRUE )


###################################################
### code chunk number 6: crisk.rnw:140-143
###################################################
mD <- gam.Lexis(Sdm, ~ s(tfd, k = 5), to = 'Dead')
mO <- gam.Lexis(Sdm, ~ s(tfd, k = 5), to = 'OAD' )
mI <- gam.Lexis(Sdm, ~ s(tfd, k = 5), to = 'Ins' )


###################################################
### code chunk number 7: crisk.rnw:156-161
###################################################
int <- 1 / 100
nd <- data.frame(tfd = seq(0, 10, int)) # not the same as the split, 
                                        # and totally unrelated to it
rownames(nd) <- nd$tfd
str(nd)


###################################################
### code chunk number 8: rates
###################################################
matshade(nd$tfd, cbind(ci.pred(mD, nd),
                       ci.pred(mI, nd),
                       ci.pred(mO, nd))*1000, 
         ylim = c(0.02,500), yaxt = "n",
         ylab = "Rates per 1000 PY", 
         xlab = "Time since DM diagnosis (years)",
         col = c("black","red","blue"), log = "y", lwd = 3, plot = TRUE)
axis(side = 2, at = ll<-outer(c(1,2,5),-2:3,function(x,y) x*10^y),
               labels = formatC(ll,digits = 4), las = 1)
axis(side = 2, at = ll<-outer(c(1.5,2:9),-2:3,function(x,y) x*10^y),
               labels = NA, tcl = -0.3)
text(0, 0.5*0.6^c(1,2,0), 
     c("Dead","Ins","OAD"),
     col = c("black","red","blue"), adj = 0)


###################################################
### code chunk number 9: crisk.rnw:195-212
###################################################
# function that calculates the midpoints between sucessive values
mp <- function(x) x[-1] - diff(x) / 2
# rates at midpoints
lD <- mp(ci.pred(mD, nd)[,1])
lI <- mp(ci.pred(mI, nd)[,1])
lO <- mp(ci.pred(mO, nd)[,1])
# cumulative rates and survival fuction at right border of the intervals
LD <- cumsum(lD) * int
LI <- cumsum(lI) * int
LO <- cumsum(lO) * int
Sv <- exp( -LD - LI - LO )
# but when integrating to get the cumulative risks we use the average
# of the survival function at the two endpoints (adding 1 as the first)
Sv <- c(1, Sv) 
rD <- c(0, cumsum(lD * mp(Sv)) * int)
rI <- c(0, cumsum(lI * mp(Sv)) * int)
rO <- c(0, cumsum(lO * mp(Sv)) * int)


###################################################
### code chunk number 10: crisk.rnw:217-221
###################################################
summary(rD + rI + rO + Sv)
oo <- options(digits = 20)
cbind(summary(Sv + rD + rI + rO))
options(oo)


###################################################
### code chunk number 11: stack
###################################################
zz <- mat2pol(cbind(rD,rI,rO,Sv), x = nd$tfd, 
              xlim = c(0,10), xaxs = "i", yaxs = "i", las = 1,
              xlab = "Time since DM diagnosis (years)", 
              ylab = "Probability",
               col =  c("black","red","blue","forestgreen"))
text(9, mp(zz["9", ]), c("Dead", "Ins", "OAD"," DM"), col = "white")
box(col = "white", lwd = 3)


###################################################
### code chunk number 12: crisk.rnw:281-283
###################################################
head(cbind(ci.pred(mI,nd),     ci.exp(mI,nd)            ))
head(cbind(ci.pred(mI,nd), exp(ci.lin(mI,nd)[,c(1,5:6)])))


###################################################
### code chunk number 13: crisk.rnw:288-290
###################################################
str(ci.lin(mI, nd, sample = 4))
head(cbind(ci.pred(mI,nd), exp(ci.lin(mI, nd, sample = 4))))


###################################################
### code chunk number 14: crisk.rnw:322-324 (eval = FALSE)
###################################################
## setwd("/home/bendix/stat/R/examples")
## source('ci.Crisk.R', echo=TRUE, max=10000)


###################################################
### code chunk number 15: crisk.rnw:331-339
###################################################
system.time(
res <- ci.Crisk(list(OAD = mO, 
                     Ins = mI, 
                    Dead = mD),
                            nd = data.frame(tfd = 0:1000 / 100),
                            nB = 1000,
                          perm = 4:1))
str(res)


###################################################
### code chunk number 16: crisk.rnw:371-379
###################################################
system.time(
rsm <- ci.Crisk(list(OAD = mO, 
                     Ins = mI, 
                    Dead = mD),
                            nd = data.frame(tfd = 0:1000 / 100),
                            nB = 2000,
                       sim.res = 'rates'))
str(rsm) 


###################################################
### code chunk number 17: crisk.rnw:386-394
###################################################
system.time(
csm <- ci.Crisk(list(OAD = mO, 
                     Ins = mI, 
                    Dead = mD),
                            nd = data.frame(tfd = 0:1000 / 100),
                            nB = 2000,
                       sim.res = 'crisk'))
str(csm) 


###################################################
### code chunk number 18: crisk.rnw:413-415
###################################################
Brates <- aperm(apply(rsm, 1:2, Epi:::mnqt), c(2,3,1))
str(Brates)


###################################################
### code chunk number 19: rates-ci
###################################################
matshade(nd$tfd, cbind(ci.pred(mD, nd),
                       ci.pred(mI, nd),
                       ci.pred(mO, nd))*1000, 
         ylim = c(0.1,500), yaxt = "n",
         ylab = "Rates per 1000 PY", 
         xlab = "Time since DM diagnosis (years)",
         col = c("black","red","blue"), log = "y", lwd = 3, plot = TRUE)
matlines(nd$tfd, 
         cbind(Brates[,"Dead",],
               Brates[,"Ins" ,],
               Brates[,"OAD" ,])*1000,
         col = c("white","black","black"), lty = 3, lwd=c(3,1,1))
axis(side = 2, at = ll<-outer(c(1,2,5),-2:3,function(x,y) x*10^y),
               labels = formatC(ll,digits = 4), las = 1)
axis(side = 2, at = ll<-outer(c(1.5,2:9),-2:3,function(x,y) x*10^y),
               labels = NA, tcl = -0.3)
text(0, 0.5*0.6^c(1,2,0), 
     c("Dead","Ins","OAD"),
     col = c("black","red","blue"), adj = 0)


###################################################
### code chunk number 20: crates
###################################################
matshade(res$time,
         cbind(res$Crisk[,"Dead",],
               res$Crisk[,"Ins" ,],
               res$Crisk[,"OAD" ,]), plot = TRUE,
         xlim = c(0,10), xaxs = "i", yaxs = "i", las = 1,
         xlab = "Time since DM diagnosis (years)", 
         ylab = "Cumulative probability",
          col = c("black","red","blue"))
text(8, 0.3 + c(1,0,2)/25, 
     c("Dead","Ins","OAD"),
     col = c("black","red","blue"), adj = 0)


###################################################
### code chunk number 21: crisk.rnw:475-477
###################################################
str(res$Crisk)
str(res$Srisk)


###################################################
### code chunk number 22: stack-ci
###################################################
zz <- mat2pol(res$Crisk[,c("Dead","Ins","OAD","Surv"),1],
              x = res$time,
           xlim = c(0,10), xaxs = "i", yaxs = "i", las = 1,
           xlab = "Time since DM diagnosis (years)", 
           ylab = "Probability",
            col =  c("black","red","blue","forestgreen") )
text( 9, mp(zz["9",]), c("Dead","Ins","OAD","DM"), col = "white" )
matshade(res$time,
         cbind(res$Srisk[,1,],
               res$Srisk[,2,],
               res$Srisk[,3,]),
         col = 'transparent', col.shade = "white", alpha = 0.3)


###################################################
### code chunk number 23: crisk.rnw:510-511
###################################################
str(res$Stime)


###################################################
### code chunk number 24: crisk.rnw:514-517
###################################################
s510 <- res$Stime[1:2*500,,]
dimnames(s510)[[1]] <- c(" 5 yr","10 yr")
round(ftable(s510, row.vars=1:2), 2)


###################################################
### code chunk number 25: crisk.rnw:535-538
###################################################
data(DMlate)
set.seed(7465)
wh <- sample(1:3, nrow(DMlate), r=T, prob = c(4, 2, 6))


###################################################
### code chunk number 26: crisk.rnw:541-542
###################################################
wh[is.na(DMlate$dodth)] <- 0


###################################################
### code chunk number 27: crisk.rnw:547-549
###################################################
DMlate$codth <- factor(wh, labels=c("Alive","CVD","Can","Oth"))
with(DMlate, table(codth, isDead = !is.na(dodth)))


###################################################
### code chunk number 28: crisk.rnw:559-566
###################################################
dmL <- Lexis(entry = list(per = dodm,
                          age = dodm - dobth,
                          tfD = 0),
              exit = list(per = dox),
       exit.status = codth,
              data = DMlate )
summary(dmL, t = T)


###################################################
### code chunk number 29: boxes
###################################################
boxes(dmL, boxpos = TRUE)


###################################################
### code chunk number 30: crisk.rnw:579-581
###################################################
sL <- splitMulti(dmL, age = 0:120)#seq(0, 120, 1/2))
summary(sL)


###################################################
### code chunk number 31: crisk.rnw:583-586
###################################################
mCVD <- gam.Lexis(sL, ~ s(tfD, by=sex), to = "CVD")
mCan <- gam.Lexis(sL, ~ s(tfD, by=sex), to = "Can")
mOth <- gam.Lexis(sL, ~ s(tfD, by=sex), to = "Oth")


###################################################
### code chunk number 32: crisk.rnw:588-591 (eval = FALSE)
###################################################
## # mCVD <- glm.Lexis(sL, ~ Ns(tfD, kn=1:6*2):sex, to = "CVD")
## # mCa  <- glm.Lexis(sL, ~ Ns(tfD, kn=1:6*2):sex, to = "Ca")
## # mOth <- glm.Lexis(sL, ~ Ns(tfD, kn=1:6*2):sex, to = "Oth")


###################################################
### code chunk number 33: crisk.rnw:616-617
###################################################
nm <- data.frame(tfD = seq(0, 15, 0.1), sex = "M")


###################################################
### code chunk number 34: crisk.rnw:621-626
###################################################
cR <- ci.Crisk(list(CVD = mCVD,
                    Can = mCan,
                  Other = mOth),
               nd = nm)
str(cR)


###################################################
### code chunk number 35: cR
###################################################
clr <- c("black","orange","limegreen")
matshade(cR$time, cbind(cR$Crisk[, "CVD"  ,],
                        cR$Crisk[, "Can"  ,],
                        cR$Crisk[, "Other",]),
         col = clr, lty = 1, lwd = 2,
         plot = TRUE, ylim = c(0,1/3), yaxs = "i")
text(0, 1/3 - 1:3/30, c("CVD","Can","Oth"), 
     col = clr, adj = 0)


###################################################
### code chunk number 36: Sr1
###################################################
matshade(cR$time, cbind(cR$Srisk[,1,],
                        cR$Srisk[,2,],
                        cR$Srisk[,3,]),
         col = clr, lty = 1, lwd = 2,
         plot = TRUE, ylim = c(0,1), xaxs = "i", yaxs = "i")
text(14, mp(c(0, cR$Srisk["14", , 1], 1)), 
     rev(c(dimnames(cR$Crisk)[[2]])))


###################################################
### code chunk number 37: Sr2
###################################################
zz <- mat2pol(cR$Crisk[,c("Other","Can","CVD","Surv"),"50%"], 
              x = cR$time, 
           xlim = c(0,15), xaxs = "i", yaxs = "i", las = 1,
           xlab = "Time since DM diagnosis (years)", 
           ylab = "Probability",
            col =  c("gray","red","blue","limegreen") )
matshade(cR$time, cbind(cR$Srisk[,1,],
                        cR$Srisk[,2,],
                        cR$Srisk[,3,]),
         col = "transparent", col.shade = "white", alpha = 0.4)
text(14, mp(c(0, cR$Srisk["14", , 1], 1)), 
     rev(c(dimnames(cR$Crisk)[[2]])), col = "white")


###################################################
### code chunk number 38: crisk.rnw:705-706
###################################################
ftable(round(cR$Stime[paste(1:5 * 3),,], 1), row.vars=1)


