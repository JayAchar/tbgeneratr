#' Calculate TB conversion date
#'
#' This function takes long laboratory data and treatment start times to calculate culture conversion dates.
#' @param x data frame or data.table
#' @param id string representing patient unique ID number
#' @param date sample date (string)
#' @param start patient treatment start date (string)
#' @param result laboratory result variable (string)
#' @param days criteria for conversion
#' @keywords TB
#' @import zoo
#' @export

tb.convert <- function(x, id, date, start, result, days = 30) {

# check args
		# check that result only has 2 levels - i.e. contaminated all removed

# code
	x <- as.data.table(x)
	x <- x[order(get(id), get(date), get(result))]	# order by id, sample date and result

# remove records before treatment start
	x <- x[get(date) > get(start)]
# remove records with no culture result
	x <- x[!is.na(get(result))]

# remove duplicate rows - id, date, result
	subvar <- c(id, date, result)
	setkeyv(x, subvar)
	x <- subset(unique(x))


# identify id and date duplicates
	x <- x[, dupvar := 1L * (.N > 1L), by = c(id, date)] 
# remove negative results if duplicated by id and date
	x <- x[!(dupvar == 1L & get(result) == "Neg"), ]


# generate culture sequence variable 
	x <- x[ , seq := seq_len(.N), by = get(id)]
	x <- x[ , grp_tot := .N, by = get(id)]
############ CROSS Checked = ok

# Calculate the days between each negative culture
	x <- x[ , neg_days := ifelse(seq == 1 & seq < grp_tot & get(result) == "Neg" & shift(get(result), 1L, type = "lead") == "Neg", 0L,
					ifelse(get(result) == "Neg" & seq > 1 & shift(get(result), 1L, type = "lag") == "Neg",							get(date) - shift(get(date), 1L, type = "lag"), NA)), ]

# Set first to 0 
	x <- x[, neg_days := ifelse(!is.na(shift(neg_days, 1L, type = "lead")) & is.na(shift(neg_days, 1L, type = "lag")) & is.na(neg_days) & seq > 1, 0L, neg_days), by = get(id)]

# pick up start of negative 
	x <- x[neg_days == 0, beg_neg := 1, ]


# get consecutive negatives
	x <- x[!is.na(beg_neg), run_neg := cumsum(beg_neg), by = get(id)]
	x <- x[, run_neg := na.locf(run_neg, na.rm = F), by = get(id)]	

# cumulative days
	x <- x[!is.na(run_neg), cum_days := sum(neg_days, na.rm = T), by = c(id, "run_neg")]

# pick out eligible dates
	x <- x[beg_neg == 1 & cum_days >= days, elig_neg := 1, ]

# keep eligible runs
	x <- x[elig_neg == 1, ]

# order
	x <- x[order(get(id), get(date))]

# keep first eligible conversion date
	x <- x[, .SD[1], by = get(id)]

# final tidy
	selvar <- c(id, start, date)
	x <- x[, selvar, with = F]
	x <- setnames(x, old = date, new = "convert")

return(x)
}
