#' Baseline drug DST
#'
#' Find baseline drug DST after baseline rifampicin result defined - 
#' auxillary function for use in dst_baseliner()  
#' @param x data frame from dst_baseliner()
#' @param drug name of drug dst variable from dst_baseliner()
#' @param days number of days allowed between individual drug specimen 
#' and baseline specimen
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbgeneratr}}
#' @importFrom stringr str_remove
#' @importFrom dplyr group_by select mutate arrange filter slice rename
#' @importFrom rlang enquo quo_name quo
#' @importFrom assertthat assert_that


drug_baseliner <- function(x, drug, days = 30) {
  APID <- NULL
  MICRLABN <- NULL
  `:=` <- NULL
  
# checks
  assert_that(is.data.frame(x))
  assert_that(is.numeric(days))
  assert_that(days >= 0)
  assert_that(any(c("epiinfo", "koch6") %in% class(x)))

# define variable names
 	drug <- enquo(drug)

## define based on object class
if ("epiinfo" %in% class(x)) {
  id <- quo(APID)
  labno <- quo(MICRLABN)
}	
 	
# build final drug dst variable name
 	drug_name <- str_remove(quo_name(drug), pattern = "dst_p_")
 	drug_var <- paste0("base_", drug_name)

# generate baseline dst for specific drug
 	x <- x %>%
 			group_by(!! id) %>%
  	# select relevent variables
  		select(!! id, .data$samp_date, .data$baseline_date, .data$baseline_no, 
  		       !! labno, .data$base_rif, .data$rif_res, !! drug) %>%
  	# generate absolute days from baseline specimen collection
  		mutate(base_abs = as.numeric(abs(.data$baseline_date - .data$samp_date))) %>%
    # keep specimens within 'days' arg of baseline specimen
      filter(.data$base_abs <= days) %>%
  	# sort by absolute days from sample to treatment start
  		arrange(!! id, .data$base_abs) %>%
  	# keep specimens with same rifampicin result as baseline
  		filter(.data$base_rif == .data$rif_res) %>%
  	# remove all drug results == NA
  		filter(! is.na(!! drug)) %>% 
  	# remove duplicates by id, date and result
  		group_by(!! id, .data$base_abs, !! drug) %>%
  		slice(1) %>% 
 	    ungroup() %>% 
    # keep more resistant if same base_abs
   		group_by(!! id, .data$base_abs) %>%
   		arrange(!! id, .data$base_abs, desc(!! drug)) %>%
   		slice(1) %>%
   	# keep closest to baseline specimen with same rif result
   		# prioritise duplicated if discordant
   		group_by(!! id) %>%
   		arrange(!! id, .data$base_abs) %>%
   		slice(1) %>% 
   	# keep variables
   		select(!! id, !! drug) %>%
   		rename(!! drug_var := !! drug) %>%
   	# ungroup
   	  ungroup()

x
}
