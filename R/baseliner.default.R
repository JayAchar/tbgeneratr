#' Default method for baseliner()
#'
#' Allow data frames with unspecified object class to pass through
#' @param x data frame containing variables
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbgeneratr}}

baseliner.default <- function(x) {
  
  message("No adm object class detected: baseliner() not applied.")
  x
  
}
