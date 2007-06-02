## Program to calculate effects for matched case-control studies
## Michael Hills May 8 2007

effx.match<-function(response,
exposure,
match,
strata=NULL,
control=NULL,
base=1,
digits=3,
alpha=0.05,
data=NULL)
{

require(Epi)

  ## attaches the dataframe specified in data=
  
  if(!is.null(data)) {
     attach(data,2)
     on.exit(detach(pos=2))
  }

## sorts out list of control variables

  if(!is.null(control)) {
    control.arg<-substitute(control)
    if(length(control.arg)>1) {
        control.names <- sapply(control.arg, deparse)[-1]
        control <- data[, control.names, drop=FALSE]
    }
    else {
          control <- data[,deparse(control.arg),drop=FALSE]
    }
  }

  ## stores the variable names for response, etc.

  rname <-deparse(substitute(response))
  ename<-deparse(substitute(exposure))
  sname<-deparse(substitute(strata))

  ## performs a few checks

  if(!is.numeric(response))stop("Response must be numeric, not a factor")
  if(rname==ename)stop("Same variable specified as response and exposure")
  if(rname==sname)stop("Same variable specified as response and strata")
  if(sname==ename)stop("Same variable specified as strata and exposure")
  
  if(missing(response))stop("Must specify the response","\n")
  if(missing(exposure))stop("Must specify the exposure","\n")
  if(!is.null(strata)&!is.factor(strata))stop("Stratifying
    variable must be a factor")

  tmp<-(response==0 | response==1)
  if(all(tmp,na.rm=TRUE)==FALSE)
  stop("Binary response must be coded 0,1 or NA")

  if(class(exposure)[1]=="ordered") {
      exposure<-factor(exposure, ordered=F)
  }
  
  ## prints out some information about variables


  cat("---------------------------------------------------------------------------","\n")
  cat("response      : ", rname, "\n")
  cat("exposure      : ", ename, "\n")
  if(!is.null(control))cat("control vars  : ",names(control),"\n")
  if(!is.null(strata)) {
    cat("stratified by : ",sname,"\n")
  }
  cat("\n")
  if(is.factor(exposure)) {
    cat(ename,"is a factor with levels: ")
    cat(paste(levels(exposure),collapse=" / "),"\n")
    cat( "baseline is ", levels( exposure )[base] ,"\n")   
    exposure <- Relevel( exposure, base )
  }
  else {
    cat(ename,"is numeric","\n")
  }
  if(!is.null(strata)) {
    cat(sname,"is a factor with levels: ")
    cat(paste(levels(strata),collapse="/"),"\n")
  }

  cat("effects are measured as odds ratios","\n")
  cat("---------------------------------------------------------------------------","\n")
  cat("\n")

  ## gets number of levels for exposure if a factor

  if(is.factor(exposure)) {
    nlevE<-length(levels(exposure))
  }
  else {
    nlevE<-1
  }

  ## labels the output
  
  if(is.factor(exposure)) {
    cat("effect of",ename,"on",rname,"\n")
  }
  else {
    cat("effect of an increase of 1 unit in",ename,"on",rname,"\n")
  }
  if(!is.null(control)) {
    cat("controlled for",names(control),"\n\n")
  }
  if(!is.null(strata)) {
    cat("stratified by",sname,"\n\n")
  }


  ## no stratifying variable

  if(is.null(strata)) {
          if(is.null(control)) {            
            m<-clogit(response~exposure+strata(match))
            cat("number of observations ",m$n,"\n\n")
          }
          else  {
              m<-clogit(response~.+exposure+strata(match),
                     subset=!is.na(exposure),data=control)
              cat("number of observations ",m$n,"\n\n")
              mm<-clogit(response~.+strata(match),
                      subset=!is.na(exposure),data=control)
          }
          res<-ci.lin(m,subset=c("exposure"),Exp=TRUE,alpha=alpha)[,c(5,6,7)]
          res<-signif(res,digits)
          if(nlevE<3) {
              names(res)[1]<-c("Effect")
          }
          else {
              colnames(res)[1]<-c("Effect")
              if(is.factor(exposure)) {
                ln <- levels(exposure)
                rownames(res)[1:nlevE-1]<-paste(ln[2:nlevE],"vs",ln[1])
              }
          }
          print(res)
          if(is.null(control)) {
              chisq<-round(summary(m)$logtest[1],2)
              df<-round(summary(m)$logtest[2])
              p<-summary(m)$logtest[3]
              cat("\n")
              cat("Test for no effects of exposure:  ","\n")
              cat("chisq=",chisq, " df=",df, " p-value=",p,"\n")
              invisible(list(res,paste("Test for no effects of exposure on",
                 df,"df:","p=",format.pval(p,digits=3))))

          }
          else  {
              aov <- anova(mm,m,test="Chisq")
              cat("\nTest for no effects of exposure on",
              aov[2,3],"df:",
              "p=",format.pval(aov[2,5],digits=3),"\n")
              invisible(list(res,paste("Test for no effects of exposure on",
                 aov[2,3],"df:","p=",format.pval(aov[2,5],digits=3))))
          }
  }      
   ## stratifying variable

  if(!is.null(strata)) {
      sn <- levels(strata)
      nlevS<-length(levels(strata))
          if(is.null(control)) {
            m<-clogit(response~strata/exposure+strata(match))
            cat("number of observations ",m$n,"\n\n")
            mm<-clogit(response~strata+exposure+strata(match))
          }
          else {
            m <-clogit(response~strata/exposure + . +strata(match),
            data=control)
            cat("number of observations ",m$n,"\n\n")
            mm <-clogit(response~strata+exposure + . +strata(match),
            data=control)
          }
          res<-ci.lin(m,Exp=TRUE,alpha=alpha,subset="strata")[c(-1:-(nlevS-1)),c(5,6,7)]
          res<-signif(res,digits)
          colnames(res)[1]<-c("Effect")
          if(is.factor(exposure)) {
            ln<-levels(exposure)
            newrownames<-NULL
            for(i in c(1:(nlevE-1))) {
              newrownames<-c(newrownames,
                             paste("strata",sn[1:nlevS],"level",ln[i+1],"vs",ln[1]))
            }
          }
          else {
             newrownames<-paste("strata",sn[1:nlevS])
          }
          rownames(res)<-newrownames
          aov<-anova(mm,m,test="Chisq")
          print( res )
          cat("\nTest for effect modification on",
          aov[2,3],"df:","p=",format.pval(aov[2,5],digits=3),"\n")
          invisible(list(res,paste("Test for effect modification on",
          aov[2,3],"df:","p=",format.pval(aov[2,5],digits=3))))

  }
}
