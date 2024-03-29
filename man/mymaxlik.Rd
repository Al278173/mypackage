% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mymaxlik.R
\name{mymaxlik}
\alias{mymaxlik}
\title{mymaxlik}
\usage{
mymaxlik(lfun, x, param, ...)
}
\arguments{
\item{lfun}{function to be applied to each set of parameters}

\item{x}{data as a vector}

\item{param}{a range of values of unknown parameter as a vector}

\item{...}{other parameters for plot function}
}
\value{
a graph showing line of log likelihood and the maximum likelihood
estimate, and a named list of the index, the maximum likelihood estimate,
the log likelihood, and a vector of slopes.
}
\description{
mymaxlik
}
\examples{
mymaxlik(x=c(3,3,4,3,4,5,5,4),param=seq(0,1,length=1000),lfun=function(x,param) log(dbinom(x,prob=param,size=20)),main="Binomial", cex.main=2)
}
