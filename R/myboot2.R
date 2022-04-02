#' @title myboot2
#'
#' @param iter number of interations as an integer
#' @param x the sample as a vector
#' @param fun the function used for each sample
#' @param alpha a number between 0 and 1 for the confidence interval
#' @param cx cex variable for the histogram
#' @param ... additional values for the histogram
#'
#' @return a histogram with the confidence intervals, and a named list with the
#' confidence interval, function, sample, and the sample after the function is
#' applied
#' @export
#'
#' @examples
#' myboot2(x=rnorm(25, mean=25, sd=10), fun="var", alpha=0.3)
myboot2<-function(iter=10000,x,fun="mean",alpha=0.05,cx=1.5,...){
  n=length(x)

  y=sample(x,n*iter,replace=TRUE)
  rs.mat=matrix(y,nr=n,nc=iter,byrow=TRUE)
  xstat=apply(rs.mat,2,fun)
  ci=quantile(xstat,c(alpha/2,1-alpha/2))
  para=hist(xstat,freq=FALSE,las=1,
            main=paste("Histogram of Bootstrap sample statistics","\n","alpha=",alpha," iter=",iter,sep=""),
            ...)


  mat=matrix(x,nr=length(x),nc=1,byrow=TRUE)

  pte=apply(mat,2,fun)
  abline(v=pte,lwd=3,col="Black")
  segments(ci[1],0,ci[2],0,lwd=4)
  text(ci[1],0,paste("(",round(ci[1],2),sep=""),col="Red",cex=cx)
  text(ci[2],0,paste(round(ci[2],2),")",sep=""),col="Red",cex=cx)

  text(pte,max(para$density)/2,round(pte,2),cex=cx)

  invisible(list(ci=ci,fun=fun,x=x, xstat=xstat))
}
