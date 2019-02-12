#' Admission TB generator
#'
#' Designed to generate additional variables from within the TB admission
#' data file. Age and BMI are calculated from date of birth, start treatment
#' date, height and weight. Start treatment date is also split into components
#' to allow easier grouping by year, month or date.
#' @param x data frame of cleaned TB admission data
#' @param rm_orig remove original variables - TRUE or FALSE
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbgeneratr}}
#' @importFrom assertthat assert_that
#' @importFrom magrittr %>%
#' @export

adm_generator <- function(x, rm_orig = TRUE) {
  
# check input
    assert_that(is.data.frame(x))


# =================================================================
# generate age
	x <- age_generator(x, rm_orig = rm_orig) %>% 


# generate bmi
	bmi_generator(x, rm_orig = rm_orig)	%>% 
	  
	  # split starttre variable to allow easier stratification
	  start_splittr(rm_orig = FALSE)

x
}
