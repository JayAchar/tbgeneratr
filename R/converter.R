#' Culture/smear conversion calculator
#'
#' Takes laboratory data with start and end treatment informaiton and
#' calculates culture or smear conversion dates based on >30 days criteria
#' @param x data frame
#' @param type define what type of culture conversion required.
#' @param software define software used for data collection.
#' Values can be "excel", "koch_6", "epiinfo"
#' @param project define project location to apply.
#' Values can be "kk", "chechnya".
#' @param file define database file argument to apply.
#' Values can be "adm", "lab", "clinical_lab",
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @export
#' @importFrom tbcleanr nse_renamer
#' @importFrom dplyr mutate filter group_by recode_factor distinct slice arrange ungroup lag
#' row_number desc select rename %>% top_n
#' @seealso \code{\link{tbgeneratr}}
#' @examples
#' \dontrun{
#' converter(p, type = "culture", software = "koch_6", project = "chechnya", file = "adm")
#' }


converter <- function(x, type = c("culture", "smear"), 
						software = c("excel", "koch_6", "epiinfo"),
						project = c("kk", "chechnya"),
						file = c("adm", "lab", "clinical_lab")) {

# checks
# check input
	if (!(is.data.frame(x))) {
			stop("input paramter, x, must be a data frame")
	}

# check args
	type <- match.arg(type)
	software <- match.arg(software)
	project <- match.arg(project)
	file <- match.arg(file)

# ====================================================
# rename variables for NSE

	x <- nse_renamer(x, software = software, project = project,
						file = file, fun = "converter")

	if (type == "culture") {
		# rename culture variable
		place <- match("culture", names(x))
	} else {
		# rename smear variable
		place <- match("smear", names(x))
	}
		# rename to "result" variable
		names(x)[place] <- "result"
# ====================================================
# convert smear to binary result
	if (type == "smear") {
		x <- x %>%
			mutate(result = recode_factor(.data$result, '1+' = "Positive", 
										'2+' = "Positive", 
										'3+' = "Positive")) 
	}

# ====================================================
# clean data
x <- x %>%
	# sort by id, sample date and result
		arrange(.data$idno, .data$samp_date, .data$result) %>%
	# remove pre-treatment samples
		filter(.data$samp_date > .data$starttre) %>%
	# remove post end of treatment samples
		filter(.data$samp_date < .data$dateend) %>%
	# remove samples with no result
		filter(!is.na(.data$result)) %>%
	# remove idno, samp_date, and result duplicats
		distinct(.data$idno, .data$samp_date, .data$result, .keep_all = TRUE) %>%
	# remove negative result when idno and date are duplicated 
		# and positive result present
			group_by(.data$idno, .data$samp_date) %>%
			top_n(1, .data$result) %>%
			ungroup() %>%
	# generate result sequence variable
		arrange(.data$idno, .data$samp_date) %>%
		group_by(.data$idno) %>%
		mutate(seq = row_number())

# calculate days between consecutive negative results
x <- x %>%
  arrange(.data$idno, .data$samp_date) %>%
  # find consecutive same results
  mutate(result_grp = cumsum(as.character(.data$result)!=lag(as.character(.data$result),default=""))) %>%
  ungroup() %>%
  group_by(.data$idno, .data$result_grp) %>%
  # keep all consecutive results which are negative and have total > 30 days
  filter(.data$result == "Negative" & (max(.data$samp_date) - min(.data$samp_date) )>=30) %>%
  ungroup() %>%
  # keep only the first episode of culture conversion
  group_by(.data$idno) %>%
  filter(.data$result_grp == min(.data$result_grp)) %>%
  top_n(1, desc(.data$seq)) %>%
  ungroup() %>%
  select(.data$idno, .data$samp_date) %>%
  rename(cc_date = .data$samp_date)


x
}