#' My pvalue function
#'
#' This function displays p-value areas
#'
#' This is a helper function used in Lab 12 that calculates and displays p value areas
#'
#' @param t0 the t value
#' @param xmax the maximum x value
#' @param n the size of data
#' @param alpha the alpha value
#'
#' @return list containing quantiles and pvalue
#' @export
#'
#' @examples
#' set.seed(55);x1=rnorm(30,mean=25,sd=5)
#' tcalc=(xbar-24)/(sd(x1)/sqrt(length(x1)))
#' mypvalue(tcalc,n=30,alpha=0.05)
mypvalue=function(t0,xmax=4,n=20, alpha=0.05){
  va=round(pt(-t0,df=n-1),4)
  pv=2*va
  curve(dt(x,df=n-1),xlim=c(-xmax,xmax),ylab="T Density",xlab=expression(t),
        main=substitute(paste("P-value=", pv, " alpha=", alpha)))
  xcurve=seq(t0,xmax,length=1000)
  ycurve=dt(xcurve,df=n-1)
  xlcurve=seq(-t0,-xmax,length=1000)
  ylcurve=dt(xcurve,df=n-1)
  polygon(c(t0,xcurve,xmax),c(0,ycurve,0),col="green")
  polygon(c(-t0,xlcurve,-xmax),c(0,ylcurve,0),col="green")
  q=qt(1-alpha/2,n-1)
  abline( v=c(q,-q),lwd=2)
  axis(3,c(q,-q),c(expression(abs(t[alpha/2])),expression(-abs(t[alpha/2]))))
  text(0.5*(t0+xmax),max(ycurve),substitute(paste(area, "=",va)))
  text(-0.5*(t0+xmax),max(ycurve),expression(area))
  return(list(q=q,pvalue=pv))
}
