#' Default method for age_generator()
#'
#' Allow data frames with unspecified object class to pass through
#' @param x data frame containing variables
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbgeneratr}}

age_generator.default <- function(x) {
  
  message("No adm object class detected: age_generator() not applied.")
  x  
  
}
