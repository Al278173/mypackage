#' @title myncurve
#'
#' @param mu mean value as a number
#' @param sigma standard deviation as a number
#' @param a a variable as a number
#'
#' @importFrom graphics curve polygon
#' @importFrom stats dnorm pnorm
#'
#' @return A plot of a normal curve and the probability of X<=a
#' @export
#'
#' @examples
#' myncurve(mu=10, sigma=5, a=6)
myncurve = function(mu, sigma, a){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu +
                                              3*sigma))
  x=seq(mu-3*sigma, a, length=1000)
  y=dnorm(x, mean=mu, sd=sigma)
  polygon(c(mu-3*sigma, x, a), c(0, y, 0), col="Red")
  list(area=round(pnorm(a, mean=mu, sd=sigma), 4))
}
