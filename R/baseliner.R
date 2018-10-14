#' Calculate baseline culture or smear status
#'
#' This function takes a data frame and uses a unique patient identifier, dates of treatment start and sample submission and sample results to provide a baseline culture or smear result.  
#' @param x data frame or data.table with combined admission and lab data. Must contain
#' patient ID number, start treatment date, sample date, and test result
#' @param software define software used for data collection.
#' Values can be "excel", "koch_6", "epiinfo"
#' @param project define project location to apply.
#' Values can be "kk", "chechnya".
#' @param file define database file argument to apply.
#' Values can be "adm", "lab",
#' @param baseline_test define which baseline test result to check
#' @param baseline_days criteria for using pre-treatment samples
#' @keywords TB
#' @export
#' @seealso \code{\link{tbgeneratr}}
#' @importFrom tbcleanr nse_renamer
#' @importFrom rlang enquo .data
#' @importFrom dplyr filter distinct mutate group_by top_n ungroup select
#' @importFrom magrittr %>%
#' @importFrom assertthat assert_that


baseliner <- function(x, 
						software = c("koch_6", "epiinfo", "excel"),
						project = c("kk", "chechnya"),
						file = c("adm", "lab"),
						baseline_test = c("culture", "smear"),
						baseline_days = 90) {

# check input
  assert_that(is.data.frame(x))
  assert_that(is.numeric(baseline_days))
  assert_that(baseline_days >= 0)
  assert_that(baseline_days %% 1 == 0)

# check all args
	project <- match.arg(project)
	baseline_test <- match.arg(baseline_test)
	software <- match.arg(software)
	file <- match.arg(file)

#=================================================================
# rename variables for NSE

	x <- nse_renamer(x, software = software, project = project,
						file = file, fun = "baseliner")

		# locate test variable name in df
			place <- match(baseline_test, names(x))
		# replace test variable with "result"
			names(x)[place] <- "result"

# enquo baseline_days for use with NSE
	baseline_days <- enquo(baseline_days)


# clean dataframe
	x <- x %>%
		# remove rows with missing sample date or id number
		# remove rows with no result
		filter(! is.na(.data$samp_date)) %>%
		filter(! is.na(.data$id)) %>%
		filter(! is.na(.data$result)) %>%
		# remove rows after start treatment + 7 days
		filter(.data$samp_date - .data$start < 7) %>%
		# reoove rows beyond defined baseline_days
		filter(.data$start - .data$samp_date < !! baseline_days) %>%
		# remove id, sample date and result duplicate
		distinct(.data$id, .data$samp_date, .data$result, .keep_all = T) %>%
		# generate absolute days variable
		mutate(abs_days = abs(.data$samp_date - .data$start)) %>%
		# keep positive results if duplicated for same id and sample date
			# result variable should be orderd factor
		group_by(.data$id, .data$abs_days) %>%
		top_n(1, as.numeric(.data$result)) %>%
		ungroup() %>%
		# keep closest result to treatment start
		group_by(.data$id) %>%
		top_n(1, desc(.data$abs_days)) %>%
		ungroup() %>%
		# select output variables
		select(.data$id, .data$result)

# generate final output variable name
		var_name <- paste0("base_", baseline_test)
		names(x)[names(x) == "result"] <- var_name

x
}

