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

bmi_generator.koch6 <- function(x, rm_orig = TRUE) {
  
  # define vars
  vars <- c("weight", "height")
  
  # check for presence of variables
  assert_that(all(vars %in% names(x)))
  
  # check variables have correct class
  assert_that(is.numeric(x$weight))
  assert_that(is.numeric(x$height))
  
  # convert all zeros to NA
  x <- naniar::replace_with_na(x, list(height = 0,
                                       weight = 0))

  # confirm that height values are in metres
  if (max(x$height, na.rm = T) > 3) {
    warning("Check height variable - either high outliers or reported in cms")
  }
  
  # confirm that weight values are in kgs
  if (max(x$weight, na.rm = T) > 140) {
    warning("Check weight varialbe - possible high outliers > 140")
  }
  
  # generate bmi variable
  x$bmi <- x$weight / (x$height ^ 2)
  
  # remove original variables
  if (rm_orig) {
    x$height <- NULL
    x$weight <- NULL
  }
  x
}
