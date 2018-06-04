#' Visualize extracted local extrema 
#' 
#' @description Given the \code{peaks} object returned from 
#' \link[rainflow]{FindPeaks}, \code{plot.peaks()} displays the 
#' original data and locations of the detected local extrema.
#' @author Addison Klinke, \email{agk38@case.edu}
#' 
#' @param p An object of type \code{peaks} returned from 
#' \link[rainflow]{FindPeaks}.
#'
#' @return None.
#' @details Plot shows the raw data in black, moving average smoothed data (if requested)
#' in green, and detected peaks in red.
#' @export

plot.peaks <- function(p) {

  xData <- seq(1, length(p$data))
  plot(xData, p$data, type = "l", lwd = 6, xlab = "Index", ylab = "Value")
  if (!is.na(p$smooth[1])) {
    xSmooth <- seq(p$window/2, length(p$data) - p$window/2, 1)
    lines(xSmooth, p$smooth, lwd = 2, col = "green")
  }
  points(p$indices, p$peaks, pch = 15, cex = 1.5, col = "red")
  lines(p$indices, p$peaks, lwd = 2, col = "red")
}
