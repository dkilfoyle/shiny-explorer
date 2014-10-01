# creates a new data frame from an existing one containing repeated
# measures as columns, e.g. a data frame named exp1.df:
# subject	age	repeat1	repeat2
# 001		34	1.45	1.67
# 002		38	1.20	1.54
# ...
# called like:
# make.rm(1:2,3:4,exp1.df)
# would multiply the "constant" variables by the number of
# repeats and reformat the repeats to a single column.
# subject	age	repdat	contrasts
# 001		34	1.45	T1
# 002		38	1.20	T1
# ...
# 001		34	1.67	T2
# 002		38	1.54	T2
# ...
# this allows a "univariate" repeated measures analysis of the data.

make.rm<-function(constant,repeated,data,contrasts) {
  if(!missing(constant) && is.vector(constant)) {
    if(!missing(repeated) && is.vector(repeated)) {
      if(!missing(data)) {
        dd<-dim(data)
        replen<-length(repeated)
        if(missing(contrasts))
          contrasts<-
          ordered(sapply(paste("T",1:length(repeated),sep=""),rep,dd[1]))
        else
          contrasts<-matrix(sapply(contrasts,rep,dd[1]),ncol=dim(contrasts)[2])
        if(length(constant) == 1) cons.col<-rep(data[,constant],replen)
        else cons.col<-lapply(data[,constant],rep,replen)
        new.df<-data.frame(cons.col,
                           repdat=as.vector(data.matrix(data[,repeated])),
                           contrasts)
        return(new.df)
      }
    }
  }
  cat("Usage: make.rm(constant, repeated, data [, contrasts])\n")
  cat("\tWhere 'constant' is a vector of indices of non-repeated data and\n")
  cat("\t'repeated' is a vector of indices of the repeated measures data.\n")
}

panel.cor.scale <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r = (cor(x, y,use="pairwise"))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex * abs(r))
  
  test = cor.test(x,y)
  Signif = symnum(test$p.value, corr=F, na=F,
                  cutpoints = c(0,0.001, 0.01, 0.05, 0.1, 1),
                  symbols = c("***","**","*","."," "))
  text(0.8,0.8,Signif, cex=cex, col=2)
}

panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r = (cor(x, y,use="pairwise"))
  txt <- format(c(round(r,digits), 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex )
}

panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
}

dkPairs <- function (x, colourizefactor, levelscol, smooth=TRUE, scale=TRUE, digits=2, ...) #combines a splom, histograms, and correlations
{
  if (missing(colourizefactor)==FALSE)
  {
    if (missing(levelscol))
    {
      levelscol = palette()[2:length(palette())]
    }
    cols = levelscol[unclass(colourizefactor)]
  }
  else
    cols = c("Black")
  pairs(x, pch=21, bg=cols, ..., lower.panel=panel.smooth, upper.panel=panel.cor.scale, diag.panel=panel.hist)
}

pairs2 = function()
{
  if (smooth)
  {
    if (scale)
    {
      if (missing(colourizefactor))
        pairs(x, ..., diag.panel=panel.hist, upper.panel=panel.cor.scale, lower.panel=panel.smooth)
      else
        pairs(x, pch=21, bg=cols, ..., diag.panel=panel.hist, upper.panel=panel.cor.scale, lower.panel=panel.smooth)
    }
    else
    {
      if (missing(colourizefactor))
        pairs(x, ..., diag.panel=panel.hist,upper.panel=panel.cor,lower.panel=panel.smooth)
      else
        pairs(x, pch=21, bg=cols, ...,diag.panel=panel.hist,upper.panel=panel.cor,lower.panel=panel.smooth)     
    } 
  }
  else      #smooth is not true
  { 
    if (scale) 
    {
      if (missing(colourizefactor))
        pairs(x,...,diag.panel=panel.hist,upper.panel=panel.cor.scale)
      else
        pairs(x, pch=21, bg=levelscol[unclass(colourizefactor)], ...,diag.panel=panel.hist,upper.panel=panel.cor.scale)
    }
    else  
    {
      if (missing(colourizefactor))
        pairs(x,...,diag.panel=panel.hist,upper.panel=panel.cor)
      else
        pairs(x, pch=21, bg=levelscol[unclass(colourizefactor)], ...,diag.panel=panel.hist,upper.panel=panel.cor)
    }
  } #end of else (smooth)
}   #end of function

dkPlotMeans <- function(response, factor1, factor2, error.bars = c("se", "sd", "conf.int", "none"),
                        level=0.95, xlab=deparse(substitute(factor1)), ylab=paste("mean of", deparse(substitute(response))), 
                        legend.lab=deparse(substitute(factor2)), main="Plot of Means",
                        pch=1:n.levs.2, lty=1:n.levs.2, col=palette()){
  if (!is.numeric(response)) stop("Argument response must be numeric.")
  xlab # force evaluation
  ylab                                                      
  legend.lab
  error.bars <- match.arg(error.bars)
  if (missing(factor2)){
    if (!is.factor(factor1)) stop("Argument factor1 must be a factor.")
    valid <- complete.cases(factor1, response)
    factor1 <- factor1[valid]
    response <- response[valid]
    means <- tapply(response, factor1, mean)
    sds <- tapply(response, factor1, sd)
    ns <- tapply(response, factor1, length)
    if (error.bars == "se") sds <- sds/sqrt(ns)
    if (error.bars == "conf.int") sds <- qt((1 - level)/2, df=ns - 1, lower.tail=FALSE) * sds/sqrt(ns)
    yrange <-  if (error.bars != "none") c( min(means - sds), max(means + sds)) else range(means)
    levs <- levels(factor1)
    n.levs <- length(levs)
    plot(c(1, n.levs), yrange, type="n", xlab=xlab, ylab=ylab, axes=FALSE, main=main)
    points(1:n.levs, means, type="b", pch=16, cex=2)
    box()
    axis(2)
    axis(1, at=1:n.levs, labels=levs)
    if (error.bars != "none") arrows(1:n.levs, means - sds, 1:n.levs, means + sds, 
                                     angle=90, lty=2, code=3, length=0.125)
  }
  else {
    if (!(is.factor(factor1) | is.factor(factor2))) stop("Arguments factor1 and factor2 must be factors.")
    valid <- complete.cases(factor1, factor2, response)
    factor1 <- factor1[valid]
    factor2 <- factor2[valid]
    response <- response[valid]
    means <- tapply(response, list(factor1, factor2), mean)
    sds <- tapply(response, list(factor1, factor2), sd)
    ns <- tapply(response, list(factor1, factor2), length)
    if (error.bars == "se") sds <- sds/sqrt(ns)
    if (error.bars == "conf.int") sds <- qt((1 - level)/2, df=ns - 1, lower.tail=FALSE) * sds/sqrt(ns)
    yrange <-  if (error.bars != "none") c( min(means - sds,na.rm=T), max(means + sds,na.rm=T)) else range(means,na.rm=T)
    levs.1 <- levels(factor1)
    levs.2 <- levels(factor2)
    n.levs.1 <- length(levs.1)
    n.levs.2 <- length(levs.2)
    if (n.levs.2 > length(col)) stop(sprintf("Number of groups for factor2, %d, exceeds number of distinct colours, %d."), n.levs.2, length(col))
    plot(c(1, n.levs.1 + 1), yrange, type="n", xlab=xlab, ylab=ylab, axes=FALSE, main=main)
    box()
    axis(2)
    axis(1, at=1:n.levs.1, labels=levs.1)
    for (i in 1:n.levs.2){
      points(1:n.levs.1, means[, i], type="b", pch=pch[i], cex=2, col=col[i], lty=lty[i])
      if (error.bars != "snone") arrows(1:n.levs.1, means[, i] - sds[, i], 
                                        1:n.levs.1, means[, i] + sds[, i], angle=90, code=3, col=col[i], lty=lty[i], length=0.125)
    }
    x.posn <- n.levs.1 + 0.25
    y.posn <- sum(c(0.1, 0.9) * par("usr")[c(3,4)])
    text(x.posn, y.posn, legend.lab, adj=c(0, -.5))
    legend(x.posn, y.posn, levs.2, pch=pch, col=col, lty=lty)
  }
  invisible(NULL)
}

dkPlotMeans2 <- function(response, factor1, factor2, level=0.95,
                         xlab=deparse(substitute(factor1)), ylab=paste("mean of", deparse(substitute(response))), 
                         legend.lab=deparse(substitute(factor2)), main="Plot of Means", col=palette()) {
  if (!is.numeric(response)) stop("Argument response must be numeric.")
  xlab # force evaluation
  ylab                                                      
  legend.lab
  if (missing(factor2)){
    if (!is.factor(factor1)) stop("Argument factor1 must be a factor.")
    valid <- complete.cases(factor1, response)
    factor1 <- factor1[valid]
    response <- response[valid]
    means <- tapply(response, factor1, mean)
    sds <- tapply(response, factor1, sd)
    ns <- tapply(response, factor1, length)
    sds <- qt((1 - level)/2, df=ns - 1, lower.tail=FALSE) * sds/sqrt(ns)
    require(gplots, quietly=T, warn.conflicts=F)
    barplot2(means, beside=T, main=main, xlab=xlab, ylab=ylab, legend.text=F, plot.ci=T, plot.grid=T, ci.u=means+sds, ci.l=means-sds)
  }
  else {
    if (!(is.factor(factor1) | is.factor(factor2))) stop("Arguments factor1 and factor2 must be factors.")
    valid <- complete.cases(factor1, factor2, response)
    factor1 <- factor1[valid]
    factor2 <- factor2[valid]
    response <- response[valid]
    means <- tapply(response, list(factor1, factor2), mean)
    sds <- tapply(response, list(factor1, factor2), sd)
    ns <- tapply(response, list(factor1, factor2), length)
    sds <- qt((1 - level)/2, df=ns - 1, lower.tail=FALSE) * sds/sqrt(ns)
    require(gplots,quietly=T,warn.conflicts=F)
    barplot2(means,beside=T,main=main,xlab=xlab,ylab=ylab,legend.text=T, plot.ci=T,plot.grid=T, ci.u=means+sds,ci.l=means-sds)
  }
  invisible(NULL)
}

dkPlotSurvival = function(Data, EventTime, EventType, EventFactor=NULL, AnalysisTime=NULL, bAddCensusLine=FALSE, sex=NULL, age=NULL, ...)
{
  require(survival)
  mySurv = Surv(EventTime, EventType)
  if (is.null(EventFactor))
  {
    mySurvFit = survfit(mySurv)
    plot(mySurvFit, col=MYCOLS, ...)
    if (!is.null(AnalysisTime))
    {
      AnalysisResult = summary(mySurvFit, times=c(AnalysisTime))$surv
      abline(v=AnalysisTime,lty="dotted")
      text(x=AnalysisTime, y=AnalysisResult[1]+0.15, labels=c(sprintf("%3.2f", AnalysisResult[1])))
    }      
  }
  else
  {
    mySurvFit = survfit(mySurv~EventFactor)
    myCoxph = coxph(mySurv~EventFactor)
    plot(mySurvFit, col=MYCOLS, legend.text=levels(EventFactor), ...)
    #plot(survfit(myCoxph)) #mySurvFit, col=MYCOLS, legend.text=levels(EventFactor), ...)
    if (!is.null(AnalysisTime))
    {
      AnalysisResult = summary(mySurvFit, times=c(AnalysisTime))$surv
      abline(v=AnalysisTime,lty="dotted")
      if (length(levels(EventFactor))==2)
        text(x=AnalysisTime, y=AnalysisResult[1]+0.15, labels=c(sprintf("%3.2f/%3.2f, p=%3.3f", AnalysisResult[1], AnalysisResult[2], myCoxph$wald.test)))
    } 
  }
}

dkPlotExpectedSurvival = function( myDFN, myEventTime, myGender, myAge)
{
  myData=eval(parse(text=myDFN))
  CensusYear<-rep(as.date("1/1/1980"),nrow(myData))
  # NB: Age must be years in days, Gender must be M=1, F =2
  if (is.factor(myData[,myGender]))
  {
    sex12 = rep(1,nrow(myData))
    sex12[myData[myGender]=="F"]=2
    sex12[myData[myGender]=="M"]=1
    sex12[myData[myGender]=="f"]=2
    sex12[myData[myGender]=="m"]=1
    myGender="sex12"
  }
  cmd1= sprintf("%s$sex12 = sex12", myDFN)
  eval(parse(text=cmd1))
  cmd2= sprintf("%s$CensusYear = CensusYear", myDFN)
  eval(parse(text=cmd2))
  cmd = sprintf("efit <- survexp(%s ~ ratetable(sex=%s,year=CensusYear,age=%s*365), data=%s)", myEventTime, myGender, myAge, myDFN)
  eval(parse(text=cmd))
  eval(parse(text="lines(efit)"))
}

DKPlotTukey = function (x, main=NULL, ...) 
{
  for (i in seq_along(x)) {
    xi <- x[[i]][, -4, drop = FALSE]
    yvals <- nrow(xi):1
    plot(c(xi[, "lwr"], xi[, "upr"]), rep.int(yvals, 2), 
         type = "n", axes = FALSE, xlab = "", ylab = "", ...)
    axis(1, ...)
    axis(2, at = nrow(xi):1, labels = dimnames(xi)[[1]], 
         srt = 0, ...)
    abline(h = yvals, lty = 1, lwd = 0, col = "lightgray")
    abline(v = 0, lty = 2, lwd = 0, col="Blue", ...)
    segments(xi[, "lwr"], yvals, xi[, "upr"], yvals, col="Red", ...)
    segments(as.vector(xi), rep.int(yvals - 0.1, 3), as.vector(xi), 
             rep.int(yvals + 0.1, 3), col="Red", ...)
    if (is.null(main)) main = paste(format(100 * attr(x, "conf.level"), 2), "% family-wise confidence level\n", sep = "")
    if (main=="") main=NULL
    title(main = main, xlab = paste("Differences in mean levels of", names(x)[i]))
    box()
  }
}

rescale<-function(x,newrange) {
  if(nargs() > 1 && is.numeric(x) && is.numeric(newrange)) {
    # if newrange has max first, reverse it
    if(newrange[1] > newrange[2]) {
      newmin<-newrange[2]
      newrange[2]<-newrange[1]
      newrange[1]<-newmin
    }
    xrange<-range(x)
    if(xrange[1] == xrange[2]) stop("can't rescale a constant vector!")
    mfac<-(newrange[2]-newrange[1])/(xrange[2]-xrange[1])
    invisible(newrange[1]+(x-xrange[1])*mfac)
  }
  else {
    cat("Usage: rescale(x,newrange)\n")
    cat("\twhere x is a numeric object and newrange is the min and max of the new range\n")
  }
}

dkPlotNormality = function(x,...) {
  def.par <- par(no.readonly = TRUE); on.exit(par(def.par))
  layout(matrix(c(1,1,2,3), 2, 2, byrow=T), widths=c(2,1), heights=c(1,2))
  
  xx = na.omit(x)
  xxhist = hist(xx, plot=F)
  xxarea = sum(diff(xxhist$breaks) * xxhist$counts)
  xxdens = density(xx)
  xxdens$y = xxdens$y * xxarea # match scale of hist to density ?? invalid but looks ok
  
  #xxspline = spline(xxhist$counts)
  #hist(xx, probability=T,col="light blue", xlab="", ylab="", main="", axes=T) , ylim=c(0,maxdens))
  #hist(xx, col="light blue", xlab="", ylab="", main="", axes=T, ylim=c(0,max(xxspline$y)))
  #lines(rescale(xxspline$x, range(xxhist$mids)), xxspline$y, col="red", lwd=2)
  hist(xx, col="light blue", xlab="", ylab="", main="", axes=T, ylim=c(0,max(xxdens$y,xxhist$counts))) 
  lines(xxdens, col="red", lwd=2)
  
  qqnorm(xx)#, main=paste("kurtosis =", round(kurtosis(x), digits=2)))
  qqline(xx, col="red")
  
  boxplot(xx, notch=T)
  rug(xx,side=2)
}

dkROCPlotSetup = function(x,y, cutoff)
{
  roc.range.min <<- min(x, y) - 0.1 * diff(range(x, y))
  roc.range.max <<- max(x, y) + 0.1 * diff(range(x, y))
  .sens <<- c(0, 1)
  .spec <<- c(0, 1)
  roc.dx <<- density(x)
  roc.dy <<- density(y)
}

dkROCPlotRefresh = function(x,y,cutoff)
{
  sens <- mean(y > cutoff)
  spec <- mean(x > cutoff)
  .sens <<- c(.sens, sens)
  .spec <<- c(.spec, spec)
  par(mar = c(5, 4, 0, 1) + 0.1)
  layout(matrix(c(1, 2), ncol = 1), heights = c(2, 1))
  op <- par(pty = "s")
  plot(.spec, .sens, xlab = "1-Specificity", ylab = "Sensitivity", xlim = c(0, 1), ylim = c(0, 1))
  par(pty = "m")
  tmp <- chull(c(1, .spec), c(0, .sens))
  lines(c(NA, .spec)[tmp], c(NA, .sens)[tmp])
  points(spec, sens, col = "red", pch = 16)
  specdiff <- diff(c(NA, .spec)[tmp])
  specdiff <- specdiff[!is.na(specdiff)]
  sensmean <- (c(c(NA, .sens)[tmp][-1], NA) + c(NA, .sens)[tmp])/2
  sensmean <- sensmean[!is.na(sensmean)]
  auc <- sum(specdiff * sensmean)
  text(1, 0.1, paste("Area Under Curve =", round(auc, 3)), cex = 1.7, adj = 1)
  d <- (1 - .sens)^2 + (.spec)^2
  dd <- which.min(d)
  lines(c(0, .spec[dd]), c(1, .sens[dd]), col = "purple")
  plot(roc.dx$x, roc.dx$y, type = "l", col = "red", xlim = c(roc.range.min, roc.range.max),
       xlab = paste("Sensitivity = ", round(sens, 3), ", Specificity = ", round(1 - spec, 3), sep = ""), 
       ylab = "Densities", ylim = c(0, max(roc.dx$y, roc.dy$y)))
  if (any(x <= cutoff)) 
    rug(x[x <= cutoff], col = "red", ticksize = 0.3)
  if (any(x > cutoff)) 
    rug(x[x > cutoff], col = "red", ticksize = 0.3, side = 3)
  lines(roc.dy$x, roc.dy$y, col = "blue")
  if (any(y <= cutoff)) 
    rug(y[y <= cutoff], col = "blue", ticksize = 0.3)
  if (any(y > cutoff)) 
    rug(y[y > cutoff], col = "blue", ticksize = 0.3, side = 3)
  abline(v = cutoff, col = "green")
}

dkStripChart = function(x, myFactor, ...)
{
  if (missing(myFactor))
  {
    stripchart(x, method="jitter",vertical=T, xlab=deparse(substitute(x)), ylab=deparse(substitute(y)), ...)
    return()
  }
  d=as.data.frame(cbind(x,myFactor))
  dd = unstack(d,x~myFactor)
  newcol = dim(dd)[2]+1
  dd[newcol] = tapply(x, myFactor, mean)
  names(dd)[newcol]="Means"
  stripchart(dd, vertical=T, xlab=deparse(substitute(x)), ylab=deparse(substitute(y)), ...)
  m <- tapply(x, myFactor, mean)
  segments(1:nlevels(myFactor)-0.25, m, 1:nlevels(myFactor)+0.25, m)
  segments(nlevels(myFactor)+1-0.25, mean(x), nlevels(myFactor)+1+0.25, mean(x))
  for (i in 1:nlevels(myFactor))
  {
    abline(v=i, lty = 3)
  }
}

logit.plot <- function(model, title="Success of logistic model")
{
  # sort the fitted values
  sf <- sort(fitted(model), index=T)
  plot(sf$x, ylim=c(0,1), type="l", col="blue", lwd=3,xlab="sorted sample number", ylab="probability")
  text(0, min(fitted(model))-.03,"fitted probability",col="blue",pos=4)
  title(title)
  abline(h=mean(fitted(model)), lty=2)
  text(0, mean(fitted(model))+.02, "mean probability", pos=4)
  # name of the response field
  field.name <- attr(attr(terms(formula(model)), "factors"),"dimnames")[[1]][1]
  # extract the T/F vector
  # depends on whether glm was called
  # with a data argument
  eval(parse(text=paste("tmp <- ",ifelse(class(model$data) == "data.frame", "model$data$", ""),field.name, sep="")))
  abline(v=length(tmp)/2,lty=2)
  text(length(tmp)/2,.03,"midpoint",pos=4)
  # show T/F
  points(1:length(tmp), tmp[sf$ix],  pch="|",cex=1,col=ifelse(tmp[sf$ix], "green4", "red"))
  text(0,.03,"FALSE Samples",col="red",pos=4)
  text(0,.97,"TRUE Samples",col="green4",pos=4)
  # print model and fit
  text(length(tmp),0.30,paste("Model:", formula(model)[2], formula(model)[1],formula(model)[3],sep=" "), pos=2,font=4)
  text(length(tmp),0.25,paste("AIC:", round(model$aic,0), sep=" "),pos=2,font=4)
  text(length(tmp),0.20,paste("Null deviance:", round(model$null.deviance,0), sep=" "),pos=2,font=4)
}

logit.plot.quad <- function(model, threshold=0.5, title="Model success", ylab="Probability")
{
  sf<-sort(fitted(model), index=T)
  # leave extra space at bottom
  par(mar=c(6,4,4,2)+.1); par(xaxs="i", yaxs="r")
  plot(sf$x, ylim=c(0,1), type="l", col="blue", lwd=3, xlab="",ylab=ylab)
  abline(h=c(0,1), lty=1)
  # show threshold and crossover point
  abline(h=threshold,lty=2); text(0,threshold+.02,paste("threshold =", threshold), pos=4)
  crossover <- sum(fitted(model) < threshold)
  abline(v=crossover,lty=2)
  text(crossover,.05,"crossover",pos=4)
  #text(crossover, threshold-.03,"fitted probability of change",col="blue",pos=4)
  # name of the response field
  field.name <- attr(attr(terms(formula(model)), "factors"),"dimnames")[[1]][1]
  # extract the T/F from it
  eval(parse(text=paste("tmp <- ",ifelse(class(model$data) == "data.frame", "model$data$", ""),field.name, sep="")))
  tmp=as.numeric(tmp)-1
  # show T/F as vertical bars at the index
  # colours differ with T/F predictions
  points(1:length(tmp),tmp[sf$ix],pch="|",cex=1,col=ifelse((tmp[sf$ix] == as.numeric(sf$x>threshold)),"green4","red"))
  # compute proportions
  tn <- sum((!tmp[sf$ix]) & as.numeric(sf$x < threshold))
  fn <- sum((!tmp[sf$ix]) & as.numeric(sf$x >= threshold))
  tp <- sum(tmp[sf$ix] & as.numeric(sf$x >= threshold))
  fp <- sum(tmp[sf$ix] & as.numeric(sf$x < threshold))
  right <- length(sf$x)*.65
  text(0,.1,paste("True negatives:",tn), col="green4",pos=4)
  text(right,.1,paste("False positives:", fn), col="red",pos=4)
  text(right,.9,paste("True positives:", tp), col="green4",pos=4)
  text(0,.9,paste("False negatives:", fp), col="red",pos=4)
  title(main=title)
  title(sub=paste("Sensitivity:", round(tp/(tp+fp),4),"; Specificity:", round(tn/(tn+fn),4)), line=4)
}

logit.roc <- function(model, steps=20)
{
  # get the response field
  # from the model object
  field.name <- attr(attr(terms(formula(model)), "factors"),"dimnames")[[1]][1]
  # and extract the T/F from it
  eval(parse(text=paste("tmp <- ",  ifelse(class(model$data) == "data.frame", "model$data$", ""),field.name, sep="")))
  tmp=as.numeric(tmp)-1
  r <- data.frame(pts = seq(0, 1-(1/steps), by=1/steps),sens = 0, spec=0);
  for (i in 0:steps)
  {
    thresh <- i/steps;
    r$sens[i] <- sum((fitted(model) >= thresh) & tmp)/sum(tmp);
    r$spec[i] <- sum((fitted(model) < thresh) & !tmp)/sum(!tmp)
  }
  return(r)
}

logit.roc.area <- function(r)
{
  area <- 0;
  for (i in 1:(length(r$pts)-1))
    area <- area + ((1 - r$sens[i+1]) - (1 - r$sens[i])) * ((r$spec[i+1] + r$spec[i])/2);
  return(area)
}

logit.roc.plot <- function(r, title="ROC curve")
{
  old.par <- par(no.readonly = TRUE); on.exit(par(old.par))
  par(xaxs="i", yaxs="i")
  plot(1 - r$sens, r$spec, xlim=c(0, 1), ylim=c(0,1), type="l",xlab="(1 - sensitivity): false positive rate",ylab="specificity: true positive rate",col="blue", lwd=2);
  points(1 - r$sens, r$spec, pch=20, cex=2, col="blue");
  abline(0, 1, lty=2);
  segments(1-r$sens, 1-r$sens, 1-r$sens, r$spec, lty=2)
  text(0.5, 0.1, paste("Area under ROC:",round(logit.roc.area(r),4)), pos=4)
  title(main = title)
}

logit.plot.ss <- function(r)
{
  plot(r$pts, r$spec, type="n",xlab="Threshold", ylab="value", ylim=c(0,1));
  title(main = "Sensitivity and specificity vs. threshold");
  abline(h=seq(0, 1, by=0.1, lty="dashed", lwd=0.5));
  lines(r$pts, r$spec, col="blue", lwd=1.5);
  lines(r$pts, r$sens, col="green4", lwd=1.5);
  text(0.05, 0.05, pos=4, col="blue", "Specificity");
  text(0.05, 0.95, pos=4, col="green4", "Sensitivity");
}

dkMakeEmptyPlot = function(msg="Drop variable(s) here")
{
  x = try(plot.new(), silent=T)
  if (!inherits(x, "try-error"))
  {
    plot.window(xlim=c(0,1),ylim=c(0,1))
    text(1/2, 1/2, msg)
  }
}

dkPlotAnova = function(mydf, numeric1, factor1) {
  def.par <- par(no.readonly = TRUE);	on.exit(par(def.par))
  n = eval(parse(text=paste(mydf,"$",numeric1)))
  f = eval(parse(text=paste(mydf,"$",factor1)))
  x = anova(lm(n~f)) #aov(n~f))
  layout(matrix(c(1,2,3), 3, 1, byrow=T))
  dkPlotMeans2(n,f, main=paste("ANOVA p =",round(x$"Pr(>F)"[1],5)), xlab=factor1, ylab=paste("Mean of", numeric1))
  boxplot(n~f,  varwidth=T, notch=T, horizontal=F)
  mytukey = TukeyHSD(aov(n~f))
  DKPlotTukey(mytukey)
}

intxplot = function (x, data = sys.parent[1], groups.in, scales, key.length = 1, 
                     key.lines, key = TRUE, trace.factor.name = deparse(substitute(groups.in)), 
                     x.factor.name = x.factor, xlab = x.factor.name, main = list(main.title, 
                                                                                 cex = main.cex), condition.name = "condition", panel = "panel.intxplot", 
                     summary.function = "sufficient", se, ..., data.is.summary = FALSE, 
                     main.title = paste("Interactions of", trace.factor.name, 
                                        "and", x.factor.name, if (length(x[[3]]) > 1) paste("|", 
                                                                                            condition.name.to.use)), main.cex = 1.5) 
{
  # taken from HH package
  M <- sys.call()
  M[[1]] <- as.name("xyplot")
  groups <- eval(substitute(groups.in), data)
  levels.groups <- levels(as.factor(groups))
  if (length(x[[3]]) > 1) {
    x.factor <- deparse(x[[3]][[2]])
    M[[2]][[3]][[2]] <- parse(text = paste("as.numeric.positioned(", 
                                           x.factor, ")"))[[1]]
    condition.name.to.use <- if ((class(x[[3]][[3]]) == "name") && 
                                   missing(condition.name)) 
      deparse(x[[3]][[3]])
    else condition.name
    M$strip = parse(text = paste(sep = "", "function(..., var.name)", 
                                 "strip.default(..., strip.names=c(TRUE,TRUE), var.name='", 
                                 condition.name.to.use, "')"))[[1]]
  }
  else {
    x.factor <- deparse(x[[3]])
    M[[2]][[3]] <- parse(text = paste("as.numeric.positioned(", 
                                      x.factor, ")"))[[1]]
    condition.name.to.use <- ""
  }
  xf <- data[[x.factor]]
  lev.x <- levels(xf)
  num.lev.x <- position(xf)
  y.name <- deparse(x[[2]])
  if (data.is.summary) 
    M$data <- data
  else if (is.null(summary.function)) {
  }
  else if (is.character(summary.function)) {
    switch(summary.function, sufficient = M$data <- sufficient(data, 
                                                               y = y.name, c(x.factor.name, trace.factor.name)), 
           bwplot = stop("bwplot not yet implemented inside 'intxplot'."), 
           stop(paste("summary function ", summary.function, 
                      " not known yet.", sep = "")))
  }
  else stop(paste("summary function ", deparse(substitute(summary.function)), 
                  " not known yet.", sep = ""))
  M$scales <- list(x = list(at = num.lev.x, labels = lev.x, 
                            alternating = FALSE))
  if (!missing(scales)) {
    if (!is.null(scales$x)) 
      M$scales$x[names(scales$x)] <- scales$x
    if (!is.null(scales$y)) 
      M$scales$y[names(scales$y)] <- scales$y
    scales$x <- NULL
    scales$y <- NULL
    if (length(scales) > 0) 
      M$scales[names(scales)] <- scales
  }
  if (missing(xlab)) 
    M$xlab <- x.factor.name
  tpg <- trellis.par.get("superpose.line")
  if (key) {
    key.index <- rep(1:length(tpg$col), length = length(levels.groups))
    M$key <- list(lines = Rows(tpg, key.index), text = list(levels.groups), 
                  columns = key.length, title = trace.factor.name, 
                  cex.title = 1, space = "right", border = 1)
  }
  else M$key <- NULL
  if (missing(main)) 
    M$main <- list(main.title, cex = main.cex)
  if (missing(panel)) 
    M$panel <- panel
  if (!missing(key.lines)) 
    M$key$lines <- key.lines
  M$key.length <- NULL
  M$key.lines <- NULL
  M$condition.name <- NULL
  M$trace.factor.name <- NULL
  M$x.factor.name <- NULL
  M$main.title <- NULL
  M$main.cex <- NULL
  if (!missing(se)) {
    if (!is.logical(substitute(se))) 
      M$se <- eval(substitute(se), M$data)
    else M$se <- M$data$sd/sqrt(M$data$nobs)
  }
  eval(M, sys.parent(1))
}

#kapler-meeier
ggkm <- function(sfit, table=T, returns=F, xlabs='Time', ylabs='survival probability',
                 ystratalabs = NULL,ystrataname=NULL,timeby=100,main='Kaplan-Meier Plot',...){
  
  
  #example
  #data(colon)
  #fit <- survfit(Surv(time,status)~rx, data=colon)
  #ggkm(fit, timeby=500)
  
  
  require(ggplot2)
  require(survival)
  require(gridExtra)
  #     Create a blank plot for place-holding
  blank_pic<- ggplot(df, aes(time,surv))+geom_blank()+theme_bw()+
    opts(axis.text.x=theme_blank(), axis.text.y=theme_blank(),
         axis.title.x=theme_blank(), axis.title.y=theme_blank(),
         axis.ticks=theme_blank(), panel.grid.major=theme_blank(),
         panel.border=theme_blank())
  
  #     Create Kaplan Meier plot
  if(is.null(ystratalabs)) ystratalabs <- as.character(levels(summary(sfit)$strata))
  (m<-max(nchar(ystratalabs)))
  if(is.null(ystrataname)) ystrataname <- 'Strata'
  times  <- seq(0, max(sfit$time), by=timeby)
  df  <- data.frame(
    time    =   sfit$time,
    n.risk  =   sfit$n.risk,
    n.event =   sfit$n.event,
    surv    =   sfit$surv,
    strata  =   summary(sfit, censored=T)$strata,
    upper   =   sfit$upper,
    lower   =   sfit$lower
  )
  levels(df$strata) <- ystratalabs
  zeros <- data.frame(time=0, surv=1, strata = ystratalabs,
                      upper=1, lower=1)
  df  <- rbind.fill(zeros, df)
  d <- length(levels(df$strata))
  p  <- ggplot(df, aes(time, surv, groups=strata))+
    geom_step(aes(linetype=strata), size=0.6)+
    theme_bw()+opts(axis.title.x=theme_text(vjust=0.5))+
    scale_x_continuous(xlabs, breaks=times, limits = c(0, max(sfit$time)))+
    scale_y_continuous(ylabs, limits = c(0,1))+
    opts(panel.grid.minor = theme_blank())+
    opts(legend.position=c(ifelse(m<10,.28,.35),ifelse(d < 4,.25,.35)))+
    opts(legend.key=theme_rect(colour=NA))+
    labs(linetype=ystrataname)+
    opts(plot.margin = unit(c(0, 1, .5, ifelse(m<10,1.5,2.5)),'lines'))+
    opts(title=main)
  sdiff <- survdiff(eval(sfit$call$formula), data=eval(sfit$call$data))
  pval <- pchisq(sdiff$chisq, length(sdiff$n)-1, lower.tail=F)
  pvaltxt <- ifelse(pval < 0.0001, 'P < 0.0001',
                    paste('P =', signif(pval,2)))
  
  if(table){
    #     Create table graphic to include at-risk numbers
    risk.data  <- data.frame(strata = summary(sfit, times=times,
                                              extend=T)$strata,
                             time = summary(sfit, times=times, extend=T)$time,
                             n.risk = summary(sfit, times=times, extend=T)$n.risk)
    data_table  <-  ggplot(risk.data, aes(x=time, y=strata,
                                          label=format(n.risk, nsmall=0)))+#, color=strata))+
      geom_text(size=3.5)+
      theme_bw()+
      scale_y_discrete(breaks=as.character(levels(risk.data$strata)),
                       labels=ystratalabs)+
      #                 scale_y_discrete(#format1ter = abbreviate,
      #                   breaks=1:3,
      #                   labels=ystratalabs)+
      scale_x_continuous('Numbers at risk', limits=c(0, max(sfit$time)))+
      opts(axis.title.x=theme_text(size=10, vjust=1))+
      opts(panel.grid.major   =   theme_blank())+
      opts(panel.grid.minor   =   theme_blank())+
      opts(panel.border       =   theme_blank())+
      opts(axis.text.x        =   theme_blank())+
      opts(axis.ticks         =   theme_blank())+
      opts(axis.text.y       =   theme_text(face='bold', hjust=1))
    
    data_table  <- data_table + opts(legend.position='none')+
      xlab(NULL)+ylab(NULL)
    
    data_table <- data_table+opts(plot.margin=unit(c(-1.5, 1, 0.1, ifelse(m<10,2.5,3.5)-0.28*m),'lines'))
    
    #     Plotting the graphs
    p <- ggplotGrob(p)
    p <- addGrob(p, textGrob(x=unit(.8, 'npc'), y=unit(.25,'npc'), label=pvaltxt,
                             gp=gpar(fontsize=12)))
    grid.arrange(p, blank_pic, data_table, clip=F, nrow=3, ncol=1,
                 heights = unit(c(2,.1,.25), c('null','null','null')))
    if(returns) {
      a <- arrangeGrob(p, blank_pic, data_table, clip=F, nrow=3, ncol=1,
                       heights = unit(c(2, .1, .25), c('null','null','null')))
      return(a)
    }
  } else {
    p <- ggplotGrob(p)
    p <- addGrob(p, textGrob(x=unit(0.5,'npc'), y=unit(0.23,'npc'),
                             label=pvaltxt, gp=gpar(fontsize=12)))
    grid.arrange(p)
    if(returns) return(p)
  }
}

# for arranging multiple ggplots in 1
# taken from http://www.stat.harvard.edu/Research/r/test0suppl.R

vp.layout <- function(x, y) viewport(layout.pos.row=x, layout.pos.col=y) 

ggplotarrange <- function(..., nrow=NULL, ncol=NULL, as.table=FALSE) {  
  dots <- list(...)  
  n <- length(dots)  
  if(is.null(nrow) & is.null(ncol)) { nrow = floor(n/2) ; ncol = ceiling(n/nrow)}  
  if(is.null(nrow)) { nrow = ceiling(n/ncol)}  
  if(is.null(ncol)) { ncol = ceiling(n/nrow)}         
  grid.newpage() 
  pushViewport(viewport(layout=grid.layout(nrow,ncol) ) )  
  ii.p <- 1  
  for(ii.row in seq(1, nrow)){  
    ii.table.row <- ii.row   
    if(as.table) {ii.table.row <- nrow - ii.table.row + 1}   
    for(ii.col in seq(1, ncol)){   
      ii.table <- ii.p   
      if(ii.p > n) break  
      print(dots[[ii.table]], vp=vp.layout(ii.table.row, ii.col))   
      ii.p <- ii.p + 1   
    } 
  } 
}

plot_odds<-function(x, title = NULL){
  tmp<-data.frame(cbind(exp(coef(x)), exp(confint(x))))
  odds<-tmp[-1,]
  names(odds)<-c("OR", "lower", "upper")
  odds$vars<-row.names(odds)
  ticks<-c(seq(.1, 1, by =.1), seq(0, 10, by =1), seq(10, 100, by =10))
  
  ggplot(odds, aes(y= OR, x = reorder(vars, OR))) +
    geom_point() +
    geom_errorbar(aes(ymin=lower, ymax=upper), width=.2) +
    scale_y_log10(breaks=ticks, labels = ticks) +
    geom_hline(yintercept = 1, linetype=2) +
    coord_flip() +
    labs(title = title, x = "Variables", y = "OR") +
    theme_bw()
}


autoplot.lm <- function(x, ..., which=c(1:3, 5), mfrow=c(1,1)){
  
  require(grid)
  df <- fortify(x)
  df <- cbind(df, rows=1:nrow(df))
  
  # residuals vs fitted
  g1 <- ggplot(df, aes(.fitted, .resid)) +
    geom_point()  +
    geom_smooth(se=FALSE) +
    geom_hline(linetype=2, size=.2) +
    scale_x_continuous("Fitted Values") +
    scale_y_continuous("Residual") +
    ggtitle("Residuals vs Fitted")
  
  # normal qq
  a <- quantile(df$.stdresid, c(0.25, 0.75))
  b <- qnorm(c(0.25, 0.75))
  slope <- diff(a)/diff(b)
  int <- a[1] - slope * b[1]
  g2 <- ggplot(df, aes(sample=.resid)) +
    stat_qq() +
    geom_abline(slope=slope, intercept=int) +
    scale_x_continuous("Theoretical Quantiles") +
    scale_y_continuous("Standardized Residuals") +
    ggtitle("Normal Q-Q")
  
  # scale-location
  g3 <- ggplot(df, aes(.fitted, sqrt(abs(.stdresid)))) +
    geom_point() +
    geom_smooth(se=FALSE) +
    scale_x_continuous("Fitted Values") +
    scale_y_continuous("Root of Standardized Residuals") +
    ggtitle("Scale-Location")
  
  # cook's distance
  g4 <-  ggplot(df, aes(rows, .cooksd, ymin=0, ymax=.cooksd)) +
    geom_point() + geom_linerange() +
    scale_x_continuous("Observation Number") +
    scale_y_continuous("Cook's distance") +
    ggtitle("Cook's Distance")
  
  # residuals vs leverage
  g5 <- ggplot(df, aes(.hat, .stdresid)) +
    geom_point() +
    geom_smooth(se=FALSE) +
    geom_hline(linetype=2, size=.2) +
    scale_x_continuous("Leverage") +
    scale_y_continuous("Standardized Residuals") +
    ggtitle("Residuals vs Leverage")
  
  # cooksd vs leverage
  g6 <- ggplot(df, aes(.hat, .cooksd)) +
    geom_point() +
    geom_smooth(se=FALSE) +
    scale_x_continuous("Leverage") +
    scale_y_continuous("Cook's distance") +
    ggtitle("Cook's dist vs Leverage")
  
  plots <- list(g1, g2, g3, g4, g5, g6)
  
  # making the plots
  grid.newpage()
  
  if (prod(mfrow)>1) {
    mypos <- expand.grid(1:mfrow[1], 1:mfrow[2])
    mypos <- mypos[with(mypos, order(Var1)), ]
    pushViewport(viewport(layout = grid.layout(mfrow[1], mfrow[2])))
    formatter <- function(.){}
  } else {
    mypos <- data.frame(matrix(1, length(which), 2))
    pushViewport(viewport(layout = grid.layout(1, 1)))
    formatter <- function(.) {
      .dontcare <- readline("Hit <Return> to see next plot: ")
      grid.newpage()
    }
  }
  
  j <- 1
  for (i in which){
    formatter()
    print(plots[[i]], vp=viewport(layout.pos.row=mypos[j,][1], layout.pos.col=mypos[j,][2]))
    j <- j+1
  }
}