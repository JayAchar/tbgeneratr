#' Default method for converter()
#'
#' Allow data frames with unspecified object class to pass through
#' @param x data frame containing variables
#' @author Jay Achar 
#' @seealso \code{\link{tbgeneratr}}

converter.default <- function(x) {
  
  message("No adm object class detected: converter() not applied.")
  x  
  
}
