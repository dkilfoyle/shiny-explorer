### Functions to visualise logistic regression
### D G Rossiter  http://www.itc.nl/personal/rossiter
### 19-April-2006 (Patriot's Day)
### bug fix in logit.roc.plot 19-July-2008
###
### source() and enjoy
### GPL; YMMV

### contents
### 1. logit.plot
### 2. logit.plot.quad
### 3. logit.roc
### 4. logit.roc.area
### 5. logit.roc.plot
### 6. logit.plot.ss

### function logit.plot
##
## visualise the success of a logistic model
## plot logistic curve, mean change, T/F, AIC, null deviance
## arguments
##   model   a fitted glm
##   title (optional)
logit.plot <- function(model, title="Success of logistic model") {
                                        # sort the fitted values
    sf <- sort(fitted(model), index=T)
    plot(sf$x, ylim=c(0,1), type="l", col="blue", lwd=3,
         xlab="sorted sample number", ylab="probability")
    text(0, min(fitted(model))-.03,
         "fitted probability",col="blue",pos=4)
    title(title)
    abline(h=mean(fitted(model)), lty=2)
    text(0, mean(fitted(model))+.02, "mean probability", pos=4)
                                        # name of the response field
    field.name <- attr(attr(terms(formula(model)), "factors"),
                       "dimnames")[[1]][1]
                                        # extract the T/F vector
                                        # depends on whether glm was called
                                        #    with a data argument
    eval(parse(text=paste("tmp <- ",
                 ifelse(class(model$data) == "data.frame", "model$data$", ""),
                 field.name, sep="")))
    abline(v=length(tmp)/2,lty=2)
    text(length(tmp)/2,.03,"midpoint",pos=4)
                                        # show T/F
    points(1:length(tmp), tmp[sf$ix],
           pch="|",cex=1,col=ifelse(tmp[sf$ix], "green4", "red"))
    text(0,.03,"FALSE Samples",col="red",pos=4)
    text(0,.97,"TRUE Samples",col="green4",pos=4)
                                        # print model and fit
    text(length(tmp),0.30,paste(
         "Model:", formula(model)[2], formula(model)[1], formula(model)[3],sep=" "),pos=2,font=4)
    text(length(tmp),0.25,paste(
          "AIC:", round(model$aic,0), sep=" "),pos=2,font=4)
    text(length(tmp),0.20,paste(
          "Null deviance:", round(model$null.deviance,0), sep=" "),pos=2,font=4)
}

### function logit.plot.quad
##
## plot logistic curve, threshold, T/F +/-, sensitivity, specificity
## arguments
##   model   a fitted glm
##   threshold  cutoff for sensitivity/specificity, default 0.5
##   title   (optional)
logit.plot.quad <- function(model, threshold=0.5, title="Model success") {
    sf<-sort(fitted(model), index=T)
                                        # leave extra space at bottom
    par(mar=c(6,4,4,2)+.1); par(xaxs="i", yaxs="r")
    plot(sf$x, ylim=c(0,1), type="l", col="blue", lwd=3, xlab="", ylab="probability of change")
    abline(h=c(0,1), lty=1)
                                        # show threshold and crossover point
    abline(h=threshold,lty=2); text(0,threshold+.02,
                         paste("threshold =", threshold), pos=4)
    crossover <- sum(fitted(model) < threshold)
    abline(v=crossover,lty=2)
    text(crossover,.05,"crossover",pos=4)
    text(crossover, threshold-.03,
         "fitted probability of change",col="blue",pos=4)
                                        # name of the response field
    field.name <- attr(attr(terms(formula(model)), "factors"),
                       "dimnames")[[1]][1]
                                        # extract the T/F from it
    eval(parse(text=paste("tmp <- ",
                 ifelse(class(model$data) == "data.frame", "model$data$", ""),
                 field.name, sep="")))
                                        # show T/F as vertical bars at the index
                                        # colours differ with T/F predictions
    points(1:length(tmp),tmp[sf$ix],
           pch="|",cex=1,
           col=ifelse((tmp[sf$ix] == (sf$x>threshold)),"green4","red"))
                                        # compute proportions
    tn <- sum((!tmp[sf$ix]) & (sf$x < threshold))
    fn <- sum((!tmp[sf$ix]) & (sf$x >= threshold))
    tp <- sum(tmp[sf$ix] & (sf$x >= threshold))
    fp <- sum(tmp[sf$ix] & (sf$x < threshold))
    right  <- length(sf$x)*.65
    text(0,.1,paste("True negatives:",tn), col="green4",pos=4)
    text(right,.1,paste("False positives:", fn), col="red",pos=4)
    text(right,.9,paste("True positives:", tp), col="green4",pos=4)
    text(0,.9,paste("False negatives:", fp), col="red",pos=4)
    title(main=title)
    title(sub=paste("Sensitivity:", round(tp/(tp+fp),4), "; Specificity:", round(tn/(tn+fn),4)), line=4)
}

### function logit.roc
##
## compute an empirical ROC curve from a fitted logistic model
## and a data frame with the modelled logical field
##
## results are used as an argument to logit.plot.ss, logit.roc.plot
##
## arguments
##   model   a fitted glm
##   steps   how many thresholds; default 20
## returns
##   logit.roc     data frame with three fields:
##      pts          vector of points along the curve
##      sens, spec   sensitivity, specificity
logit.roc <- function(model, steps=20) {
                                        # get the response field
                                        # from the model object
    field.name <- attr(attr(terms(formula(model)), "factors"),
                       "dimnames")[[1]][1]
                                        # and extract the T/F from it
    eval(parse(text=paste("tmp <- ",
                 ifelse(class(model$data) == "data.frame", "model$data$", ""),
                 field.name, sep="")))
                                        # initialize object
 	r <- data.frame(pts = seq(0, 1-(1/steps), by=1/steps),
                    sens = 0, spec=0);
                                        # compute sens/spec at thresholds
	for (i in 0:steps) {
		thresh <- i/steps;
		r$sens[i] <- sum((fitted(model) >= thresh) & tmp)/sum(tmp);
		r$spec[i] <- sum((fitted(model) < thresh) & !tmp)/sum(!tmp)
		}
	return(r)
 	}

### function logit.roc.area
##
## compute area under a ROC curve computed by logit.roc()
## typical usage: logit.roc.area(roc(model, dataset))
## argument
##   r  an ROC curve returned by logit.roc()
logit.roc.area <- function(r) {
    area <- 0;
    for (i in 1:(length(r$pts)-1))
        area <- area + ((1 - r$sens[i+1]) - (1 - r$sens[i])) * ((r$spec[i+1] + r$spec[i])/2);
    return(area)
    }

### function logit.roc.plot
##
## plot an ROC curve computed by logit.roc()
## typical usage: logit.roc.plot(roc(model, dataset))
## argument
##   r  an ROC curve returned by logit.roc()
logit.roc.plot <- function(r, title="ROC curve") {
	old.par <- par(no.readonly = TRUE); on.exit(par(old.par))
	par(xaxs="i", yaxs="i")
	plot(1 - r$spec, r$sens, xlim=c(0, 1), ylim=c(0,1), type="l",
		xlab="(1 - specificity): false positive rate",
		ylab="sensitivity: true positive rate",
		col="blue", lwd=2);
   	points(1 - r$spec, r$sens, pch=20, cex=2, col="blue");
	abline(0, 1, lty=2);
	segments(1-r$spec, 1-r$spec, 1-r$spec, r$sens, lty=2)
	text(0, 0.9, paste("Area under ROC:",round(logit.roc.area(r),4)), pos=4)
	title(main = title)
	}

### function logit.plot.ss
##
## plot sensitivity, specificity vs. threshold for a logistic model
## typical usage: logit.plot.ss(logit.roc(model, dataset))
## argument
##    r  ROC curve computed by logit.roc()
logit.plot.ss <- function(r) {
	plot(r$pts, r$spec, type="n",
         xlab="Threshold",  ylab="value", ylim=c(0,1));
	title(main = "Sensitivity and specificity vs. threshold");
    abline(h=seq(0, 1, by=0.1, lty="dashed", lwd=0.5));
	lines(r$pts, r$spec, col="blue", lwd=1.5);
	lines(r$pts, r$sens, col="green4", lwd=1.5);
    text(0.05, 0.05, pos=4, col="blue", "Specificity");
    text(0.05, 0.95, pos=4, col="green4", "Sensitivity");
	}

