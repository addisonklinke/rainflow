#' Visualize rainflow cycles 
#' 
#' @description Given the \code{rainflow} object returned from 
#' \link[rainflow]{CountCycles}, \code{plot.rainflow()} displays the 
#' a scatter plot of cycle mean and amplitude.
#' @author Addison Klinke, \email{agk38@case.edu}
#' 
#' @param r An object of type \code{rainflow} returned from 
#' \link[rainflow]{CountCycles}.
#'
#' @return None.
#' @export

plot.rainflow <- function(r) {
  plot(r$amplitude, r$mean, pch = 19, xlab = "Cycle Amplitude", ylab = "Cycle Mean")
}
