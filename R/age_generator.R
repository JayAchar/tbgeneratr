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
#' @importFrom checkr check_data check_classes
#' @export
#' @examples
#' \dontrun{
#' age_generator(p, dob = "dateofbirth", start = "Starttre")
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
	check_data(x, values = c(dob, start))

# check variables are dates
    check_classes(x[[dob]], "Date")
    check_classes(x[[start]], "Date")

# generate age variable
	x$age <- interval(x[[dob]], x[[start]]) / years(1)


# remove original variables
 	if (rm_orig) {
 		x[[dob]] <- NULL
 	}

x
}