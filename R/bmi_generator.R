#' Calculate BMI from height and weight
#'
#' Use numerical height and weight variables to calculate BMI
#' @param x data frame containing height and weight variables
#' @param db define database being used - "k6", "epi_info"
#' @param rm_orig remove original variables - TRUE or FALSE
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{TB.funs}}
#' @export
#' @examples
#' \dontrun{
#' bmi_generator(p, weight = "weight", height = "height")
#' }

bmi_generator <- function(x, db = "k6", rm_orig = TRUE, ...) {

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
			weight <- "weight"
			height <- "height"
		}	
		if (db == "epi_info") {
			weight <- "WEIGHT"
			height <- "HEIGHT"
		}
# =================================================================

# check args are valid
	if (! all(c(weight, height) %in% names(x))) {
		stop("Height and weight variables not available - check function args")
	}

# check variables are numericals
	if (! all(is.numeric(x[[weight]]) & is.numeric(x[[height]]) ) ) {
		stop("Variables are not formatted as numericals")
	}

# convert weight/height == 0 to NA
	x[[weight]][x[[weight]] == 0] <- NA
	x[[height]][x[[height]] == 0] <- NA

# message showing number of missing values
	w <- sum(is.na(x[[weight]]))

	w1 <- paste0("Number of missing weight values:", w, sep = " ")
	message(w1)

	h <- sum(is.na(x[[height]]))
	h1 <- paste0("Number of missing height values:", h, sep = " ")
	message(h1)

	c <- sum(is.na(x[[height]]) | is.na(x[[weight]]))
	c1 <- paste0("Number of records with missing height or weight:", c, sep = " ")
	message(c1)

# confirm that height values are in metres
	if (mean(x[[height]], na.rm = T) > 2) {
		warning("Check height variable - either high outliers or reported in cms")
	}

# confirm that weight values are in kgs
	if (max(x[[weight]], na.rm = T) > 140) {
		warning("Check weight varialbe - possible high outliers")
	}

# generate bmi variable
	x$bmi <- as.numeric(x[[weight]] / (x[[height]] ^ 2))

# check for introduction of new NA
	bmi <- sum(is.na(x$bmi))
	if (! bmi == c) {
		warning("New NAs included in BMI variable")
	}

# remove original variables
		 	if (rm_orig %in% c("TRUE", "T")) {
		 		x[[height]] <- NULL
		 		x[[weight]] <- NULL
		 	}

return(x)	
}
