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
#' row_number desc select rename %>%
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
			mutate(result = recode_factor(result, '1+' = "Positive", 
										'2+' = "Positive", 
										'3+' = "Positive")) 
	}

# ====================================================
# clean data
x <- x %>%
	# sort by id, sample date and result
		arrange(idno, samp_date, result) %>%
	# remove pre-treatment samples
		filter(samp_date > starttre) %>%
	# remove post end of treatment samples
		filter(samp_date < dateend) %>%
	# remove samples with no result
		filter(!is.na(result)) %>%
	# remove idno, samp_date, and result duplicats
		distinct(idno, samp_date, result, .keep_all = TRUE) %>%
	# remove negative result when idno and date are duplicated 
		# and positive result present
			group_by(idno, samp_date) %>%
			arrange(desc(result)) %>%
			slice(1) %>%
	# generate result sequence variable
		arrange(idno, samp_date) %>%
		group_by(idno) %>%
		mutate(seq = row_number())

# calculate days between consecutive negative results
x <- x %>%
  group_by(idno) %>%
  arrange(idno, samp_date) %>%
  # find consecutive same results
  mutate(result_grp = cumsum(as.character(result)!=lag(as.character(result),default=""))) %>%
  group_by(idno, result_grp) %>%
  # keep all consecutive results which are negative and have total > 30 days
  filter(result == "Negative" & (max(samp_date) - min(samp_date) )>=30) %>%
  # keep only the first episode of culture conversion
  group_by(idno) %>%
  filter(result_grp == min(result_grp)) %>%
  slice(1) %>%
  ungroup() %>%
  select(idno, samp_date) %>%
  rename(cc_date = samp_date)


x
}