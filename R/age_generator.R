#' Calculate age from two dates
#'
#' Use date of birth and date of starting treatment to calculate 
#' age at baseline. Takes data frame with date of birth and start 
#' of treatment date to generate age variable in years. 
#' @param x data frame containing Koch 6 admission variables
#' @param db define database being used - "k6", "epi_info"
#' @param rm_orig remove original variables - TRUE or FALSE
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{TB.funs}}
#' @export
#' @examples
#' \dontrun{
#' age_generator(p, dob = "dateofbirth", start = "Starttre")
#' }

age_generator <- function(x, db = "k6", rm_orig = TRUE) {
# acceptable values for "set" arg
	s <- c("k6", "epi_info")

# check input
	if (!(is.data.frame(x))) {
			stop("input paramter, x, must be a data frame")
	}

# check db is within acceptable values
	if (! db %in% s) {
		stop("Specify db argument within specified values")
	}

# =================================================================
# set db specific variables 
		if (db == "k6") {
			dob <- "dateofbirth"
			start <- "Starttre"
				
		}	
		if (db == "epi_info") {
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

	return(x)
}