#' A helper function for \link[rainflow]{FindPeaks}
#' 
#' @description Checks whether a local minimum or maximum mets the criteria for 
#' an important local extrema based on the compression parameter \code{R}.
#' @author Addison Klinke, \email{agk38@case.edu}
#' 
#' @param ai Numeric, value at the current location in the time series.
#' @param R Numeric, the compression parameter (greater than 1).
#' @param aMin Numeric, value of the most recent local minimum in the time series.
#' @param aMax Numeric, value of the most recent local maximum in the time series.
#'
#' @return Logical indicating whether the criteria (\code{ratio > R}) has been met.
#' @export
#'
#' @examples
#' CheckRatio(ai = 6.1, R = 1.2, aMin = 5)   # returns TRUE
#' CheckRatio(ai = -1.3, R = 1.1, aMax = -1) # returns TRUE

CheckRatio <- function(ai, R, aMin = NULL, aMax = NULL) {
  
  # Check arguments
  if (!is.null(aMin)) {
    if (aMin == 0 | ai == 0) stop("One or more zero values, cannot perform division")
  } else if (!is.null(aMax)) {
    if (aMax == 0 | ai == 0) stop("One or more zero values, cannot perform division")
  } else if (is.null(aMin) & is.null(aMax)) {
    stop("Must supply either aMin or aMax for comparison")
  }

  # Procedure for minimum
  if (!is.null(aMin)) {
    if (ai > 0 & aMin > 0)      { case <- 1 } 
    else if (ai < 0 & aMin < 0) { case <- 2 } 
    else                        { case <- 3 }
    
    ratio <- switch(case, ai/aMin, aMin/ai, ai/abs(aMin))
    return(ratio > R)
  }
  
  # Procedure for maximum
  if (!is.null(aMax)) {
    if (ai > 0 & aMax > 0)      { case <- 1 } 
    else if (ai < 0 & aMax < 0) { case <- 2 } 
    else                        { case <- 3 }
    
    ratio <- switch(case, aMax/ai, ai/aMax, aMax/abs(ai))
    return(ratio > R)
  }
}
