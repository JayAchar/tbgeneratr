#' Calculate baseline culture or smear status
#'
#' This function takes a data frame and uses a unique patient identifier, dates of treatment start and sample submission and sample results to provide a baseline culture or smear result.  
#' @param x data frame or data.table with combined admission and lab data. Must contain
#' patient ID number, start treatment date, sample date, and test result
#' @param project define project location to apply.
#' Values can be "kk", "chechnya".
#' @param test define which baseline test result to check
#' @param days criteria for using pre-treatment samples
#' @keywords TB
#' @seealso \code{\link{tbgeneratr}}
#' @import data.table
#' @export

baseline <- function(x, project = c("kk", "chechnya"),
						test = c("culture", "smear"),
						days = 90) {

# check input
	if (!(is.data.frame(x))) {
			stop("input paramter, x, must be a data frame")
	}

# check all args
	project <- match.arg(project)
	test <- match.arg(test)

#=================================================================
# define variable names
	if (project == "chechnya") {
		id <- "dstnumber"
	} else {
		id <- "id"
	}

# define common variable names
		date <- "samp_date"
		start <- "Starttre"
		neg <- "Negative"

# define test variables
	if (test == "culture") {
		result <- "culture"
	} else if (test == "smear") {
		result <- "smear"
	}

#=================================================================
# convert to data.table
x <- as.data.table(x)

# check if result variable is binary
	if (summary(levels(factor(x[[result]])))[1] != 2) {
		stop("Result variable must have 2 levels")
	}

	keyvar <- c(id, start, date)
	setkeyv(x, keyvar)			# set key to sort

# filter out results with no date
	x <- x[!is.na(get(date)),]

# filter out rows with no result
	x <- x[!is.na(get(result))]

# remove ineligible records due to date
	x <- x[get(start) - get(date) <= days, ]
	x <- x[get(start) - get(date) >= -7, ]

# absolute days from treatment start to specimen collection
	x <- x[, abs_days := abs(get(date) - get(start)), ]

# remove duplicate rows - id, time from treatment start, result
	subvar <- c(id, "abs_days", result)
	setkeyv(x, subvar)
	x <- unique(x, by = subvar)


# identify id & time from treatment start duplicates
	x <- x[, dupvar := 1L * (.N > 1L), by = c(id, "abs_days")] 

# remove negative culture if duplicated with positive on same day
	x <- x[!(dupvar == 1L & get(result) == neg), ]

# add row id after sorting
	x <- x[, row_id := 1:.N, by = c(id, start)]

# keep result closest to treatment start
	x <- x[row_id == 1,]

# rename result variable 
	selvar <- c(id, start, date, result)
	x <- x[ , selvar, with = F]
	x <- setnames(x, old = c(result, date), new = c("baseline", "date"))

x
}
