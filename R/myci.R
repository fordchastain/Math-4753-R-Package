#' My confidence interval function
#'
#' Function to form a confidence interval
#'
#' This is a helper function used in Lab 11 of Math4753 that gives a nice and simple way to calculate a confidence interval.
#'
#' @param y the sample data
#' @param alpha the alpha value
#'
#' @return a list containing the confidence interval
#' @export
#'
#' @examples
#' set.seed(23);x=rnorm(30,mean=10,sd=12)
#' myci(x)
myci=function(y,alpha=0.05) {
  n=length(y)
  ybar=mean(y)
  s=sd(y)
  t=qt(1-alpha/2,n-1)
  mp=c(-1,1)
  mu=ybar+mp*t*s/sqrt(n)
  return(list(ci=mu))
}
