#' Admission TB generator
#'
#' Designed to generate additional variables from within the TB admission
#' data file. Age and BMI are calculated from date of birth, start treatment
#' date, height and weight. 
#' @param x data frame of cleaned TB admission data
#' @param software define software used for data collection.
#' Values can be "excel", "koch_6", "epiinfo"
#' @param rm_orig remove original variables - TRUE or FALSE
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbgeneratr}}
#' @export
#' @examples
#' \dontrun{
#' adm_generator(p, software = "epiinfo")
#' }


adm_generator <- function(x, 
						software = c("epiinfo", "koch_6", "excel"), 
						rm_orig = TRUE,
						...) {
# check input
	if (!(is.data.frame(x))) {
			stop("input paramter, x, must be a data frame")
	}

# check all args
	software <- match.arg(software)

# =================================================================
# generate age
	x <- age_generator(x, software = software, rm_orig = rm_orig)


# generate bmi
	x <- bmi_generator(x, software = software, rm_orig = rm_orig)	

x
}