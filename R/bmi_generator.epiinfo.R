#' Calculate BMI from height and weight
#'
#' Use numerical height and weight variables to calculate BMI
#' @param x data frame containing height and weight variables
#' @param rm_orig remove original variables - TRUE or FALSE
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbgeneratr}}
#' @importFrom assertthat assert_that
#' @importFrom naniar replace_with_na
#' @export

bmi_generator.epiinfo <- function(x, rm_orig = TRUE, ...) {
  
  # define vars
  vars <- c("WEIGHT", "HEIGHT")
  
  # check for presence of variables
  assert_that(all(vars %in% names(x)))
  
  # check variables have correct class
  assert_that(is.numeric(x$WEIGHT))
  assert_that(is.numeric(x$HEIGHT))
  
  # convert all zeros to NA
  x <- naniar::replace_with_na(x, list(WEIGHT = 0,
                                       HEIGHT = 0))

  
  # confirm that height values are in metres
  if (max(x$HEIGHT, na.rm = T) > 3) {
    warning("Check height variable - either high outliers or reported in cms")
  }
  
  # confirm that weight values are in kgs
  if (max(x$WEIGHT, na.rm = T) > 140) {
    warning("Check weight varialbe - possible high outliers > 140")
  }
  
  # generate bmi variable
  x$bmi <- x$WEIGHT / (x$HEIGHT ^ 2)
  
  # remove original variables
  if (rm_orig) {
    x$HEIGHT <- NULL
    x$WEIGHT <- NULL
  }
  x
}
