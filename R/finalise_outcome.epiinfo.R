#' Finalise TB treatment outcome
#'
#' Finalise TB outcome variable to align with WHO definitions
#' @param x data frame containing outcome variables
#' @param simplify logical to add binary outcome variable
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar 
#' @importFrom assertthat assert_that
#' @seealso \code{\link{tbgeneratr}}
#' @export

finalise_outcome.epiinfo <- function(x, 
                             simplify = TRUE,
                             ...) {

  # check that outcome variable has been cleaned
  assert_that(is.factor(x$outcome))
    
  # adjust outcome variable according to WHO definitions
  ## transfer to cat 4 and on treatment are not valid outcomes

  
}