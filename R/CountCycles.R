#' Generic ASTM Rainflow Cycle Counting
#' 
#' @description Implements ASTM E1049-85 rainflow cycle counting to determine 
#' the amplitude and mean of each cycle within a vector (or time series) of 
#' alternating min/max local extrema.
#' @references \url{https://www.astm.org/Standards/E1049.htm}
#' @references \insertRef{dowling_mechanical_2013}{rainflow}
#' @author Addison Klinke, \email{agk38@case.edu}
#' @seealso \link[rainflow]{FindPeaks}, \link[rainflow]{CountCycles.numeric},
#' \link[rainflow]{CountCycles.peaks} 
#' 
#' @param x Object of class \code{peaks} (preferred for best results) or 
#' \code{numeric}. 
#' 
#' @details If \code{x} is class \code{numeric}, the vector should strictly 
#' alternate min/max. If it does not, \code{CountCycles()} will automatically 
#' remove any repeated min or max values and issue a warning.
#' @return Object of type \code{rainflow}: a data frame with the number of cycles 
#' for each amplitude/mean combination. 
#' @export 
#' 
#' @examples
#' # For class 'numeric' (example from ASTM E1049-85 Fig. 6)
#' astm <- c(-2, 1, -3, 5, -1, 3, -4, 4, -2) 
#' r <- CountCycles(astm)
#' summary(r)
#' plot(r)
#' 
#' # For class 'peaks' (using package weather data)
#' data(weather)
#' p <- FindPeaks(weather$temp, R = 1.1, smooth = TRUE, window = 120)
#' r <- CountCycles(p)
#' summary(r)
#' plot(r)

CountCycles <- function(x) {
  UseMethod("CountCycles", x)
}
