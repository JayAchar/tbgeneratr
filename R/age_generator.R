#' Calculate age from two dates
#'
#' Use date of birth and date of starting treatment to calculate 
#' age at baseline. Takes data frame with date of birth and start 
#' of treatment date to generate age variable in years. 
#' @param x data frame containing Koch 6 admission variables 
#' @param software define software used for data collection.
#' Values can be "excel", "koch_6", "epiinfo"
#' @param rm_orig remove original variables - TRUE or FALSE
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbgeneratr}}
#' @importFrom lubridate is.Date years interval
#' @importFrom assertthat assert_that
#' @export
#' @examples
#' \dontrun{
#' age_generator(p, software = "koch_6", rm_orig = FALSE)
#' }

age_generator <- function(x, software = c("epiinfo", "koch_6", "excel"), rm_orig = TRUE) {


# check args
	software <- match.arg(software)

# =================================================================
# set software specific variables 
		if (software == "koch_6") {
			dob <- "dateofbirth"
			start <- "Starttre"
				
		}	
		if (software == "epiinfo") {
			dob <- "BIRTDATE"
			start <- "STARTTRE"

		}
# =================================================================
# check input
	assert_that(is.data.frame(x))
	assert_that(all(c(dob, start) %in% names(x)))

# check variables are dates
	assert_that(class(x[[dob]]) == "Date")
	assert_that(class(x[[start]]) == "Date")

# generate age variable
	x$age <- interval(x[[dob]], x[[start]]) / years(1)

# remove original variables
 	if (rm_orig) {
 		x[[dob]] <- NULL
 	}

x
}