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
#' @examples
#' \dontrun{
#' drug_baseliner(p, dst_p_pza)
#' }


drug_baseliner <- function(x, drug, days = 30) {

 	drug <- enquo(drug)

# build final drug dst variable name
 	drug_name <- str_remove(quo_name(drug), pattern = "dst_p_")
 	drug_var <- paste0("base_", drug_name)

# generate baseline dst for specific drug
 	x <- x %>%
 			group_by(idno) %>%
  	# select relevent variables
  		select(idno, samp_date, baseline_date, baseline_no, labno, base_rif, rif_res, !! drug) %>%
  	# generate absolute days from baseline specimen collection
  		mutate(base_abs = as.numeric(abs(baseline_date - samp_date))) %>%
    # keep specimens within 'days' arg of baseline specimen
      filter(base_abs <= days) %>%
  	# sort by absolute days from sample to treatment start
  		arrange(idno, base_abs) %>%
  	# keep specimens with same rifampicin result as baseline
  		filter(base_rif == rif_res) %>%
  	# remove all pza results == NA
  		filter(! is.na(!! drug)) %>%
  	# remove duplicates by idno, date and result
  		group_by(idno, base_abs, !! drug) %>%
  		slice(1) %>%
  	# keep more resistant if same base_abs
 		group_by(idno, base_abs) %>%
 		arrange(idno, base_abs, desc(!! drug)) %>%
 		slice(1) %>%
 	# keep closest to baseline specimen with same rif result
 		# prioritise duplicated if discordant
 		group_by(idno) %>%
 		arrange(idno, base_abs) %>%
 		slice(1) %>%
 	# keep variables
 		select(idno, !! drug) %>%
 		rename(!! drug_var := !! drug)

x
}
