#' Baseline DST calculator
#'
#' Takes a TB DST laboratory dataset with information on 
#' treatment start time to output the baseline DST according
#' to specific rules designed to reduce missingness
#' @param adm data frame containing TB admission data cleaned and allocated object
#' class by tbcleanr package
#' @param lab data frame containing TB laboratory data cleaned and allocated object
#' class by tbcleanr package
#' @param dst_time absolute historical limit for including specimens
#' @param dst_days additional criteria for including non-rif results
#' @author Jay Achar 
#' @seealso \code{\link{tbgeneratr}}
#' @importFrom assertthat assert_that
#' @export


dst_baseliner <- function(adm, lab, 
							 dst_time = 90, 
							 dst_days = 30) {

# checks
  assert_that(is.data.frame(adm))
  assert_that(is.data.frame(lab))
  assert_that(any(c("epiinfo", "koch6", "grozny") %in% class(lab)))
  assert_that(is.numeric(dst_time))
    assert_that(dst_time >= 0)
  assert_that(is.numeric(dst_days))
    assert_that(dst_days >= 0)

  UseMethod("dst_baseliner", adm)

}

