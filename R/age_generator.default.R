#' Default method for age_generator()
#'
#' Allow data frames with unspecified object class to pass through
#' @param x data frame containing variables
#' @param categorise logical - generate additional factor age variable
#' @param paediatric logical - generate additional paediatric age factor variable
#' @param rm_orig remove original variables - TRUE or FALSE
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbgeneratr}}

age_generator.default <- function(x,
                                  categorise = FALSE,
                                  paediatric = FALSE,
                                  rm_orig = TRUE) {
  
  message("No adm object class detected: age_generator() not applied.")
  x  
  
}
