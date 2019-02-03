#' Calculate baseline culture or smear status
#'
#' This function takes two data frames, including TB admission and 
#' laboratory programme data, uses a unique patient identifier, dates of 
#' treatment start and sample submission and sample results to calculate baseline 
#' culture or smear status. The function requires admission and laboratory data to 
#' have been prepared with the tbcleanr package and for the admission data to have 
#' class of either "epiinfo" or "koch6"
#' @param adm data frame containing TB admission data cleaned and allocated object
#' class by tbcleanr package
#' @param lab data frame containing TB laboratory data cleaned and allocated object
#' class by tbcleanr package
#' @param baseline_test string to define which baseline test result to check
#' @param baseline_days criteria for using pre-treatment samples
#' @keywords TB
#' @export
#' @seealso \code{\link{tbgeneratr}}
#' @importFrom assertthat assert_that


baseliner <- function(adm, lab, 
						baseline_test = c("culture", "smear"),
						baseline_days = 90) {

# check input
  assert_that(is.data.frame(adm))
  assert_that(is.data.frame(lab))
  assert_that(any(c("epiinfo", "groz", "koch6") %in% class(lab)))
  assert_that(is.numeric(baseline_days))
  assert_that(baseline_days >= 0)
  assert_that(baseline_days %% 1 == 0)
  baseline_test <- match.arg(baseline_test)

# Use Method
  UseMethod("baseliner", adm)
  
}

