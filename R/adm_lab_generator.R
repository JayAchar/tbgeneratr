#' Admission and lab merged TB generator
#'
#' Designed to generate additional variables from within the merged TB 
#' admission and laboratory data file. Baseline DST, culture conversion 
#' dates and baseline smear and culture are generated and outputed with 
#' the id number of the data set. 
#' @param x data frame of cleaned and merged TB admission and lab data
#' @param software define software used for data collection.
#' Values can be "excel", "koch_6", "epiinfo"
#' @param project define project location to apply.
#' Values can be "kk", "chechnya".
#' @param file define database file argument to apply.
#' Values can be "adm", "lab", "clinical_lab",
#' @param dst_time absolute historical limit for including specimens
#' @param dst_days additional criteria for including non-rif results
#' @param baseline_days criteria for using pre-treatment samples
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbgeneratr}}
#' @importFrom tbcleanr nse_renamer
#' @importFrom magrittr %>%
#' @importFrom dplyr left_join
#' @export


adm_lab_generator <- function(x, 
								software = c("excel", "koch_6", "epiinfo"),
								project = c("kk", "chechnya"),
								file = c("adm", "lab", "clinical_lab"),
								dst_time = 90, 
							 	dst_days = 30,
								baseline_days = 90) {
# checks
# check input
	if (!(is.data.frame(x))) {
			stop("input paramter, x, must be a data frame")
	}

# check args
	software <- match.arg(software)
	project <- match.arg(project)
	file <- match.arg(file)


# generate unique id numbers
	final <- data.frame(id = unique(x$id), stringsAsFactors = F)

# baseline smear and culture status
	sm_base <- baseliner(x, software = software, project = project, 
							file = "lab",
							baseline_test = "smear", 
							baseline_days = baseline_days)
	cult_base <- baseliner(x, software = software, project = project, 
							file = "lab",
							baseline_test = "culture", 
							baseline_days = baseline_days)


# baseline DST calculator
	base_dst <- dst_baseliner(x, project = project, dst_time = dst_time,
								dst_days = dst_days)

# culture convertor
	cc <- converter(x, convert_type = "culture", software = software, 
							project = project, file = file)
	sc <- converter(x, convert_type = "smear", software = software, 
							project = project, file = file)


# join all to id numbers
	final <- final %>%
		left_join(sm_base, by = "id") %>%
		left_join(cult_base, by = "id") %>%
		left_join(cc, by = "id") %>%
		left_join(sc, by = "id") %>%
		left_join(base_dst, by = c("id" = "id"))


final
}