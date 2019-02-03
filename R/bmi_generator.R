#' Calculate BMI from height and weight
#'
#' Use numerical height and weight variables to calculate BMI
#' @param x data frame containing height and weight variables
#' @param rm_orig remove original variables - TRUE or FALSE
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbgeneratr}}
#' @importFrom assertthat assert_that
#' @export

bmi_generator <- function(x, rm_orig = TRUE, ...) {
    
# checks
  assert_that(is.data.frame(x))
  assert_that(is.logical(rm_orig))
  
  UseMethod("bmi_generator", x)

}
