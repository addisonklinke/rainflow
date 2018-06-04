#' Detect Local Extrema 
#' 
#' @description Compress a vector (or time series) by extracting only the 
#' important local extrema (see references for algorithm implemented).
#' @importFrom Rdpack reprompt
#' @importFrom zoo rollmean
#' @references \insertRef{fink_indexing_2004}{rainflow}
#' @author Addison Klinke, \email{agk38@case.edu}
#'
#' @param a Vector of measured values (can be time series or not).
#' @param R Numeric value greater than 1 indicating the threshold for important 
#' local extrema. Larger values mean fewer peaks are selected (higher compression 
#' ratio). Try starting with 1.1 if you are unsure what value to use. 
#' @param smooth Logical, should a moving average filter be applied before extracting 
#' local extrema? If \code{TRUE} (recommended), this is implemented with the 
#' \code{rollmean()} function from the \code{zoo} package. 
#' @param window Numeric, if \code{smooth = TRUE}, a value indicating the window of 
#' points to be used for moving average filter. Else if \code{smooth = FALSE}, this 
#' can be left as \code{NULL} (the default).
#' @param thres Numeric value indicating that points in \code{a} below \code{thres}
#' will be dropped. This occurs before any smoothing or detection of local extrema. If
#' \code{thres = NULL} (the default), all points will be retained for analysis.
#' @param na.pad Logical, should the compressed (\code{x, y}) vectors be padded with 
#' \code{NA} at the ends to make them the same length as the original data? 
#' Useful if you are trying to add a column to a dataframe and need to match the number 
#' of existing rows. Then when plotting, you can use \code{na.rm = TRUE} to plot only 
#' the extrema.
#' 
#' @return Object of class \code{peaks}, a list containing: 
#' \item{indices}{Locations of detected peaks.}
#' \item{peaks}{y-values for detected peaks.}
#' \item{data}{Original data passed to this function.}
#' \item{smooth}{If \code{smooth = TRUE}, the smoothed data, else \code{NA}.} 
#' \item{window}{If \code{smooth = TRUE}, the smoothing window used, else \code{NA}.}
#' \item{compr}{Compression ratio (1 minus number of peaks detected / length of original vector).}
#' \item{corr}{Pearson correlation coefficient between original and compressed vectors.}
#' \item{diff}{Mean difference between original and compressed vectors.}
#' @export
#' 
#' @examples 
#' # Smooth with moving average filter of 120 minutes (2 hours)
#' # Extract local extrema for temperature time series with R=1.1 threshold
#' data(weather)
#' p <- FindPeaks(weather$temp[1:7000], R = 1.1, smooth = TRUE, window = 120)
#' summary(p)
#' 
#' # Plot the compressed and original time series
#' plot(p)

FindPeaks <- function(a, R, smooth, window = NULL, thres = NULL, na.pad = FALSE) {
  
  if(class(a) != "numeric") stop("'a' must be numeric")
  
  # Apply threshold filter (if requested)
  orig <- a
  indices <- 1:length(a)
  if (!is.null(thres)) {
    indices <- indices[a > thres]
    a <- a[a > thres]
  } 
  
  # Apply moving average filter (if requested)
  if (smooth) {
    if (!is.null(window)) {
      a <- zoo::rollmean(a, window, align = "center")
    } else {
      stop("Parameter 'window' is required for smoothing. Please pass a numeric value")
    }
  }
  
  # Find the first important point
  n <- length(a)
  iMin <- 1
  iMax <- 1
  i <- 2
  extr <- rep(0, n)
  iExtr <- rep(0L, n)
  extr[1] <- a[1]
  iExtr[1] <- indices[1]
  count <- 2
  
  while (i < n) {
    minR <- CheckRatio(a[i], R, aMin = a[iMin])
    maxR <- CheckRatio(a[i], R, aMax = a[iMax])
    if (minR) { doMinFirst <- FALSE; break }
    if (maxR) { doMinFirst <- TRUE;  break }
    if (a[i] < a[iMin]) { iMin <- i }
    if (a[i] > a[iMax]) { iMax <- i }
    i <- i + 1
  }
  
  # Don't assign if first important point is a[1] (already accounted for)
  if (iMin != 1 & iMax != 1) { 
    first <- min(iMin, iMax)
    extr[2] <- a[first]
    iExtr[2] <- indices[first]
    count <- count + 1
  } 

  # Iterate through remainder of time series
  while (i < n) {
    
    # Find next important minimum
    # Make sure to alternate min/max
    if (doMinFirst) { 
      iMin <- iMax
      i <- iMax
      isLast <- TRUE
      minR <- FALSE
      while (i < n & (!minR | isLast)) {
        if (a[i] < a[iMin]) { iMin <- i }
        minR <- CheckRatio(a[i], R, aMin = a[iMin])
        if (i == iMin) { isLast <- TRUE } else { isLast <- FALSE }
        i <- i + 1
      } 
      
      if (i < n | a[iMin] < a[i]) {
        extr[count] <- a[iMin]
        iExtr[count] <- indices[iMin]
        count <- count + 1
      }
    } else {
      doMinFirst <- TRUE  # Reset for all iterations other than the first
    }

    # Find next important maximum
    iMax <- iMin
    i <- iMin
    isLast <- TRUE
    maxR <- FALSE
    while (i < n & (!maxR | isLast)) {
      if (a[i] > a[iMax]) { iMax <- i }
      if (i == iMax) { isLast <- TRUE } else {isLast <- FALSE }
      maxR <- CheckRatio(a[i], R, aMax = a[iMax])
      i <- i + 1
    } 
    
    if (i < n | a[iMax] > a[i]) {
      extr[count] <- a[iMax]
      iExtr[count] <- indices[iMax]
      count <- count + 1
    }
  }
  
  # Correct for the offset of moving average filter (if used)
  if (smooth) {
    iExtr[iExtr != 0] <- iExtr[iExtr != 0] + window/2
  }  
  
  # Trim excess pre-allocation and add the final point in the series
  if (na.pad) {
    extr[count] <- a[n]
    extr[extr == 0] <- NA
    iExtr[count] <- ifelse(smooth, indices[n] + window/2, indices[n])
    iExtr[iExtr == 0] <- NA
    compr <- sum(!is.na(extr)) / length(a)
  } else {
    extr  <- c(extr[1:(count - 1)], a[n])
    iExtr <- c(iExtr[1:(count - 1)], ifelse(smooth, indices[n] + window/2, indices[n]))
    compr <- (1 - length(extr) / length(a))
  }
  
  # Determine correlation and mean difference with original time series
  interp <- approx(iExtr, extr, 1:length(orig))$y
  meanDiff <- sum(abs(orig[which(!is.na(interp))] - interp[!is.na(interp)])) / sum(!is.na(interp))
  corr <- cor(interp, orig, use = "pairwise.complete.obs")
  
  # Create 'peaks' object to return
  p <- list("indices" = iExtr, 
            "peaks" = extr, 
            "data" = orig,
            "smooth" = if(smooth) a      else NA,
            "window" = if(smooth) window else NA,
            "compr" = compr, 
            "corr" = corr, 
            "diff" = meanDiff)
  class(p) <- "peaks"
  return(p) 
}
