#' Admission and lab merged TB generator
#'
#' Designed to generate additional variables from within the merged TB 
#' admission and laboratory data file. Baseline DST, culture conversion 
#' dates and baseline smear and culture are generated and outputed with 
#' the id number of the data set. 
#' @param adm data frame containing TB admission data cleaned and allocated object
#' class by tbcleanr package
#' @param lab data frame containing TB laboratory data cleaned and allocated object
#' class by tbcleanr package
#' @param baseline_days criteria for using pre-treatment samples
#' @return admission data frame with additional generated variables for baseline smear
#' and culture status, and smear and culture conversion date. 
#' @author Jay Achar 
#' @seealso \code{\link{tbgeneratr}}
#' @importFrom magrittr %>%
#' @importFrom assertthat assert_that
#' @export


adm_lab_generator <- function(adm, lab,
								baseline_days = 90) {
# checks
# check input
  assert_that(is.data.frame(adm))
  assert_that(is.data.frame(lab))
  assert_that(is.numeric(baseline_days))
  assert_that(baseline_days >= 0)
  assert_that(baseline_days %% 1 == 0)
  assert_that(any(c("epiinfo", "groz", "koch6") %in% class(lab)))
  assert_that(any(c("epiinfo", "koch6") %in% class(adm)))

# # generate unique id numbers
# 	final <- data.frame(id = unique(x$id), stringsAsFactors = F)

# baseline smear and culture status
	data <- baseliner(adm, lab,
							baseline_test = "smear", 
							baseline_days = baseline_days) %>% 
          baseliner(lab,
							baseline_test = "culture", 
							baseline_days = baseline_days) %>% 
	  
# calculate culture conversion date
	  converter(lab = lab, convert_type = "culture") %>% 
	  
# calculate smear conversion date
	  converter(lab = lab, convert_type = "smear")


data
}
