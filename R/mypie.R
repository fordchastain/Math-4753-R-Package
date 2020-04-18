#' My pie chart function
#'
#' Creates a pie chart with percentages
#'
#' This is a helper function for creating a nice pie chart with percentages for each slice
#'
#' @param slices the slices for the pie chart
#' @param lbls the labels for the slices
#' @param mn the main title of the pie chart
#'
#' @return a pie chart
#' @export
#'
#' @examples
#' slices <- c(20,18,"my pie chart")
#' lbls <- c("slice 1", "slice 2)
#' mypie(slices, lbls)
mypie <- function(slices, lbls, mn) {
  pct <- round(slices/sum(slices)*100)
  lbls <- paste(lbls, pct) # add percents to labels
  lbls <- paste(lbls,"%",sep="") # ad % to labels
  pie(slices,labels = lbls, col=rainbow(length(lbls)),
      main=mn)
}
