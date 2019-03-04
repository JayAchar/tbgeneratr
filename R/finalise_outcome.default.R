#' Finalise TB treatment outcome
#'
#' Finalise TB outcome variable to align with WHO definitions
#' @param x data frame containing outcome variables
#' @param simplify logical to add binary outcome variable
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar 
#' @seealso \code{\link{tbgeneratr}}
#' @export

finalise_outcome.default <- function(x, 
                             simplify = TRUE,
                             ...) {
  
  message("No adm object class detected: finalise_outcome() not applied.")
  x
  
  
}