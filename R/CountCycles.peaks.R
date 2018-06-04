#' \link[rainflow]{CountCycles} method for S3 object class \code{peaks}
#' 
#' @description Implements ASTM E1049-85 rainflow cycle counting to determine 
#' the amplitude and mean of each cycle within a vector (or time series) of 
#' alternating min/max local extrema.
#' @references \url{https://www.astm.org/Standards/E1049.htm}
#' @author Addison Klinke, \email{agk38@case.edu}
#' @seealso \link[rainflow]{FindPeaks} 
#' 
#' @param p Object of class \code{peaks} returned from \link[rainflow]{FindPeaks}.
#' 
#' @return Object of type \code{rainflow}: a data frame with the number of cycles 
#' for each amplitude/mean combination. 
#' @export 

CountCycles.peaks <- function(x) {
  
  x <- x$peaks

  # Setup variables
  positions <- 1:length(x)
  start <- 1
  remaining <- TRUE
  current <- NULL
  cycles <- matrix(0, nrow = floor(length(x) - 1), ncol = 3)
  cyc <- 1
  
  while (remaining) {
    
    # Ensure at least 3 points are available
    if (length(current) < 3) {
      if (!is.null(current)) {
        if (length(positions[-current]) > 0) {
          current <- c(current, positions[!(positions %in% current)][1])
        } else {
          remaining <- FALSE
        }
      } else {
        current <- positions[1:3]
      }
    }
    
    # Compare ranges X and Y
    len <- length(current)
    x1 <- current[len]
    x2 <- current[len - 1]
    rangeX <- x[x1] - x[x2]
    
    y1 <- current[len - 1]
    y2 <- current[len - 2]
    rangeY <- x[y1] - x[y2]
    
    if (abs(rangeX) >= abs(rangeY)) {
      cycles[cyc, 1] <- abs(rangeY)
      cycles[cyc, 2] <- mean(c(x[y1], x[y2]))
      includesStart <- min(positions) %in% c(y1, y2)
      
      if (!includesStart) {
        cycles[cyc, 3] <- 1
        cyc <- cyc + 1
        positions <- positions[!(positions %in% current[c(len - 1, len - 2)])]
        current <- current[-c(len - 1, len - 2)]
        
      } else {
        cycles[cyc, 3] <- 0.5
        cyc <- cyc + 1
        current <- current[-1]
        positions <- positions[-1]
      }
    } else {
      if (sum(!(positions %in% current)) != 0) {
        current <- c(current, positions[!(positions %in% current)][1])
      } else {
        remaining <- FALSE
      }
    }
  }
  
  # Account for any leftover half cycles
  for (i in 1:(length(current) - 1)) {
    cycles[cyc, 1] <- abs(x[current[i]] - x[current[i + 1]])
    cycles[cyc, 2] <- mean(c(x[current[i]], x[current[i + 1]]))
    cycles[cyc, 3] <- 0.5
    cyc <- cyc + 1
  }
  
  # Format and return
  cycles <- as.data.frame(cycles)
  cycles <- plyr::rename(cycles, c("V1" = "amplitude", "V2" = "mean", "V3" = "cycles"))
  cycles <- cycles[-which(cycles$cycles == 0), ]
  class(cycles) <- c("rainflow", "data.frame")
  return(cycles)
}
