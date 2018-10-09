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
#' @importFrom lubridate is.Date
#' @export
#' @examples
#' \dontrun{
#' age_generator(p, dob = "dateofbirth", start = "Starttre")
#' }

age_generator <- function(x, software = c("epiinfo", "koch_6", "excel"), rm_orig = TRUE) {
# check input
	if (!(is.data.frame(x))) {
			stop("input paramter, x, must be a data frame")
	}

# check all args
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

# check args are valid
	if (! all(c(dob, start) %in% names(x))) {
		stop("Age variables not available - check function args")
	}

# check variables are dates
	if (! all(is.Date(x[[dob]]) & is.Date(x[[start]]) ) ) {
		stop("Variables are not formatted as dates")
	}

# generate age variable
	x$age <- as.numeric(difftime(x[[start]], x[[dob]],
								units = "weeks")) / 52.25


# remove original variables
 	if (rm_orig %in% c("TRUE", "T")) {
 		x[[dob]] <- NULL
 	}

x
}