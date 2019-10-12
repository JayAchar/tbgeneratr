#' Calculate BMI from height and weight
#'
#' Use numerical height and weight variables to calculate BMI
#' @param x data frame containing height and weight variables
#' @param rm_orig remove original variables - TRUE or FALSE
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar 
#' @seealso \code{\link{tbgeneratr}}
#' @importFrom assertthat assert_that
#' @export

bmi_generator <- function(x, rm_orig = TRUE, ...) {
    
# checks
  assert_that(is.data.frame(x))
  assert_that(is.logical(rm_orig))
  
  UseMethod("bmi_generator", x)

}


#' @inherit bmi_generator

bmi_generator.default <- function(x) {
  
  message("No adm object class detected: bmi_generator() not applied.")
  x
  
}
