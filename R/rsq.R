#' My r squared function
#'
#' Creates the r squared value
#'
#' This is a helper function for calculatiing the r squared value of a linear
#' regression model.
#'
#' @param xk the xk value
#' @param data the data frame
#'
#' @return a vector of doubles
#' @export
#'
#' @examples
#' xk <- 18
#' data <- 1:20/20
#' rsq(xk, data)
rsq = function(xk,data){
  df=within(data, X<-(BHDiameter-xk)*(BHDiameter>xk))
  lmp=lm(Height ~ BHDiameter + X, data=df)
  tmp = summary(lmp)
  tmp$r.squared
}
