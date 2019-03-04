#' Finalise TB treatment outcome
#'
#' Finalise TB outcome variable to align with WHO definitions
#' @param x data frame containing outcome variables
#' @param simplify logical to add binary outcome variable
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar 
#' @importFrom assertthat assert_that is.flag
#' @seealso \code{\link{tbgeneratr}}
#' @export

finalise_outcome <- function(x, 
                          simplify = TRUE,
                          ...) {
  
  # check input
  assert_that(is.data.frame(x),
              is.flag(simplify))
  
  # apply correct method
  UseMethod("finalise_outcome", x)
  
}

