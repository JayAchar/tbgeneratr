#' Calculate BMI from height and weight
#'
#' Use numerical height and weight variables to calculate BMI
#' @param x data frame containing height and weight variables
#' @param software define database being used - "koch_6", "epiinfo"
#' @param rm_orig remove original variables - TRUE or FALSE
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbgeneratr}}
#' @importFrom assertthat assert_that
#' @export
#' @examples
#' \dontrun{
#' bmi_generator(p, software = "koch_6", rm_orig = FALSE)
#' }

bmi_generator <- function(x, software = c("koch_6", "epiinfo"), 
							rm_orig = TRUE, ...) {
    
# checks
  assert_that(is.data.frame(x))
  assert_that(is.logical(rm_orig))
  
# check all args
	software <- match.arg(software)


# =================================================================
# set software specific variables 
		if (software == "koch_6") {
		  assert_that(all(c("weight", "height") %in% names(x)))
			weight <- "weight"
			height <- "height"
		}	
		if (software == "epiinfo") {
		  assert_that(all(c("WEIGHT", "HEIGHT") %in% names(x)))
			weight <- "WEIGHT"
			height <- "HEIGHT"
		}
# =================================================================

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
	if (max(x[[height]], na.rm = T) > 3) {
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
		warning("Likely error - new NAs included in BMI variable")
	}

# remove original variables
		 	if (rm_orig) {
		 		x[[height]] <- NULL
		 		x[[weight]] <- NULL
		 	}

x
}
