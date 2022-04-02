#' @title mycltp
#'
#' @param n sample size as a number
#' @param iter the number of iterations
#' @param lambda the lambda value for the poisson distribution as a number
#'
#' @importFrom stats dpois rpois
#'
#' @return a histogram of the sample mean, a barplot for the sampled y, and a
#' barplot for the probability
#' @export
#'
#' @examples
#' mycltp(n=2, iter=10000,lambda=4)
mycltp=function(n,iter,lambda=10){
  y=rpois(n*iter,lambda=lambda)
  data=matrix(y,nrow=n,ncol=iter,byrow=TRUE)
  w=apply(data,2,mean)
  param=hist(w,plot=FALSE)
  ymax=max(param$density)
  ymax=1.1*ymax
  layout(matrix(c(1,1,2,3),nrow=2,ncol=2, byrow=TRUE))
  hist(w,freq=FALSE,  ylim=c(0,ymax), col=rainbow(max(w)),
       main=paste("Histogram of sample mean","\n", "sample size= ",n," iter=",iter," lambda=",lambda,sep=""),
       xlab="Sample mean")
  curve(dnorm(x,mean=lambda,sd=sqrt(lambda/n)),add=TRUE,col="Red",lty=2,lwd=3) # add a theoretical curve
  barplot(table(y)/(n*iter),col=rainbow(max(y)), main="Barplot of sampled y", ylab ="Rel. Freq",xlab="y" )
  x=0:max(y)
  plot(x,dpois(x,lambda=lambda),type="h",lwd=5,col=rainbow(max(y)),
       main="Probability function for Poisson", ylab="Probability",xlab="y")
}
