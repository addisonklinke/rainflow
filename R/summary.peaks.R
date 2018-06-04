#' S3 method for object of type \code{peaks}
#'
#' @author Addison Klinke, \email{agk38@case.edu}
#' 
#' @param p An object of type \code{peaks} returned from 
#' \link[rainflow]{FindPeaks}.
#'
#' @return None.
#' @export

summary.peaks <- function(p) {
  cat(sprintf(paste0("%d local extrema detected\n",
                     "%.1f%% compression ratio\n",
                     "%.3f correlation with original vector\n",
                     "%.3f mean difference with original vector\n"), 
              sum(!is.na(p$indices)),
              p$compr * 100,
              p$corr, 
              p$diff))
}
