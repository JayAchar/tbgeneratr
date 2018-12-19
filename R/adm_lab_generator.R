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
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
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
							baseline_days = baseline_days)


# # baseline DST calculator
# 	base_dst <- dst_baseliner(x, software = software, 
# 								project = project, 
# 								dst_time = dst_time,
# 								dst_days = dst_days)

# # culture convertor
# 	cc <- converter(x, convert_type = "culture", software = software, 
# 							project = project, file = file)
# 	sc <- converter(x, convert_type = "smear", software = software, 
# 							project = project, file = file)
# 
# 
# # join all to id numbers
# 	final <- final %>%
# 		left_join(sm_base, by = "id") %>%
# 		left_join(cult_base, by = "id") %>%
# 		left_join(cc, by = "id") %>%
# 		left_join(sc, by = "id") %>%
# 		left_join(base_dst, by = c("id" = "id"))


data
}
