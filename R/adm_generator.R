#' Admission TB variable generator
#'
#' Designed to generate additional variables from within the TB admission
#' data file. Age and BMI are calculated from date of birth, start treatment
#' date, height and weight. Start treatment date is also split into components
#' to allow easier grouping by year, month or date.
#' @param x data frame of cleaned TB admission data using the `tbcleanr` package
#' @param rm_orig logical argument to remove original variables from age and BMI
#' calculators
#' @param categorise logical flag to add factor variable of ages
#' @param paediatric logical flag to add factor variable for childhood ages
#' @param bin_outcome logical flag to add binary outcome variable
#' @param ... additional arguments to pass
#' @author Jay Achar 
#' @seealso \code{\link{tbgeneratr}}
#' @importFrom assertthat assert_that is.flag
#' @importFrom magrittr %>%
#' @export

adm_generator <- function(x, 
                          rm_orig = TRUE, 
                          categorise = TRUE,
                          paediatric = TRUE,
                          bin_outcome = TRUE,
                          ...) {
  
# check input
    assert_that(is.data.frame(x),
                is.flag(rm_orig))

# =================================================================
# generate age
	x <- age_generator(x, 
	                   rm_orig = rm_orig, 
	                   categorise = categorise,
	                   paediatric = paediatric,
	                   ...) %>% 


# generate bmi
	bmi_generator(x, rm_orig = rm_orig)	%>% 
	  
	  # split starttre variable to allow easier grouping
	  start_splittr(rm_orig = FALSE) %>% 
	  
	  # categorise outcome variable
	  finalise_outcome(bin_outcome = bin_outcome)

x
}
