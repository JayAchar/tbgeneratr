#' Calculate baseline culture or smear status
#'
#' This function takes a data frame and uses a unique patient identifier, dates of treatment start and sample submission and sample results to provide a baseline culture or smear result.  
#' @param x data frame or data.table with combined admission and lab data. Must contain
#' patient ID number, start treatment date, sample date, and test result
#' @param project define project location to apply.
#' Values can be "kk", "chechnya".
#' @param baseline_test define which baseline test result to check
#' @param baseline_days criteria for using pre-treatment samples
#' @keywords TB
#' @seealso \code{\link{tbgeneratr}}
#' @import data.table


baseline_old <- function(x, project = c("kk", "chechnya"),
						baseline_test = c("culture", "smear"),
						baseline_days = 90) {

# check input
	if (!(is.data.frame(x))) {
			stop("input paramter, x, must be a data frame")
	}

# check all args
	project <- match.arg(project)
	baseline_test <- match.arg(baseline_test)

#=================================================================
# rename variables	
	x <- nse_renamer(x, project = project,
						fun = "baseline")

# define test variables
	if (baseline_test == "culture") {
		result <- "culture"
	} else if (baseline_test == "smear") {
		result <- "smear"
	}

#=================================================================
# convert to data.table
x <- as.data.table(x)

# check if result variable is binary
if (baseline_test == "culture") {
		if (summary(levels(factor(x[[result]])))[1] != 2) {
		stop("Result variable must have 2 levels")
	}
}

	keyvar <- c(id, start, date)
	setkeyv(x, keyvar)			# set key to sort

# filter out results with no date
	x <- x[!is.na(get(date)),]

# filter out rows with no result
	x <- x[!is.na(get(result))]

# remove ineligible records due to date
	x <- x[get(start) - get(date) <= baseline_days, ]
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
	if (baseline_test == "culture") {
		x <- x[!(dupvar == 1L & get(result) == neg), ]
	} else {
		# keep higher smear grade for duplicates on same day
#		x <- x[!(dupvar == 1L & min(get(result))), ]
		### error - min does not work for ordinal factor variables with '&'
	}

# add row id after sorting
	x <- x[, row_id := 1:.N, by = c(id, start)]

# keep result closest to treatment start
	x <- x[row_id == 1,]

# rename result variable 
	selvar <- c(id, start, date, result)
	x <- x[ , selvar, with = F]
	if (baseline_test == "culture") {
		x <- setnames(x, old = c(result, date), new = c("baseline_culture", "date"))
	}
	if (baseline_test == "smear") {
		x <- setnames(x, old = c(result, date), new = c("baseline_smear", "date"))
	}
	

x
}
