#' My normal curve function
#'
#' Plots a normal curve and the probability of X<=a
#'
#' This is a normal curve plotting function
#'
#' @param mu mean
#' @param sigma standard deviation
#' @param a the value to calculate prob with
#'
#' @return the probability
#' @export
#'
#' @examples
#' myncurve(10,5,6)
myncurve = function(mu, sigma, a){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  x=seq(mu-3*sigma,a,length=100)
  hx=dnorm(x,mean=mu,sd=sigma)
  polygon(c(mu-3*sigma,x[x<=a],a),c(0,hx[x<=a],0),col="red")
  list("P(X<=a)" = pnorm(a,mu,sigma))
}
