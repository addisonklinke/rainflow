#' S3 method for object of type \code{rainflow}
#'
#' @author Addison Klinke, \email{agk38@case.edu}
#' 
#' @param r An object of type \code{rainflow} returned from 
#' \link[rainflow]{CountCycles}.
#'
#' @return None.
#' @export

summary.rainflow <- function(r) {
  cat(sprintf(paste0("%d cycles detected\n",
                     "%.3f average cycle amplitude\n",
                     "%.3f average cycle mean\n"), 
              nrow(r),
              mean(r$amplitude),
              mean(r$mean)))
}
