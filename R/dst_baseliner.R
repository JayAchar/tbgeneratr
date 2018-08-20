#' Baseline DST calculator
#'
#' Takes a TB DST laboratory dataset with information on 
#' treatment start time to output the baseline DST according
#' to specific rules designed to reduce missingness
#' @param x data frame with TB and treatmetn data
#' @param software define software used for data collection.
#' Values can be "excel", "koch_6", "epiinfo"
#' @param project define which project this dataset refers to
#' @param dst_time absolute historical limit for including specimens
#' @param dst_days additional criteria for including non-rif results
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbgeneratr}}
#' @importFrom dplyr filter mutate select rename group_by slice
#' ungroup arrange left_join distinct
#' @importFrom stringr str_which
#' @importFrom purrr map_at
#' @importFrom tidyr gather
#' @importFrom rlang .data
#' @export
#' @examples
#' \dontrun{
#' dst_baseliner(lab, project = "kk")
#' }

dst_baseliner <- function(x, 
               software = c("excel", "koch_6", "epiinfo"),
               project = c("kk", "chechnya"), 
							 dst_time = 90, 
							 dst_days = 30) {

# checks
# check input
	if (!(is.data.frame(x))) {
			stop("input paramter, x, must be a data frame")
	}

# check args
	project <- match.arg(project)
  software <- match.arg(software)

# ====================================================
# rename variables for NSE
	x <- nse_renamer(x, software = software, project = project,
						file = "lab", fun = "dst_baseliner")

# ===================================================
# clean baseline data and define eligible period for consideration
	x <- x %>%
  		filter(! is.na(.data$samp_date)) %>%
  	# remove specimens > 7 days sfter treatment start
  		filter(.data$samp_date - .data$starttre < 7) %>%
  	# generate absolute days from sample to start treatment
  		mutate(abs = as.numeric(abs(.data$starttre - .data$samp_date))) %>%
  	# remove specimens > time from starttre
  		filter(.data$abs < dst_time)

# convert all DST results to numerical - 1 = sens, 2 = res
  		hain <- str_which(names(x), pattern = "hain_")
  		x_rif <- str_which(names(x), pattern = "xpert_rif")
  		dst_p <- str_which(names(x), pattern = "dst_p_")
  			recode_dst <- c(hain, x_rif, dst_p)
  		x <- map_at(.x = x, .at = recode_dst, .f = as.numeric)
  		x <- as.data.frame(x, stringsAsFactors = FALSE)

# aggregate rifampicin results
	x <- x %>%
  	# remove specimens without rifampicin results
  		filter(! (is.na(.data$xpert_rif) & is.na(.data$hain_rif) & is.na(.data$dst_p_rif))) %>%
  	# take most resistant specimen - 1 = sensitive, 2 = resistant
  		mutate(rif_res = pmax(.data$xpert_rif, .data$hain_rif, .data$dst_p_rif, na.rm = TRUE)) %>%
   		select(-.data$xpert_rif, -.data$hain_rif, -.data$dst_p_rif)

 # aggregate isoniazid results
  	x <- x %>%
  		mutate(dst_p_inh2 = pmax(.data$hain_inh, .data$dst_p_inh, na.rm = TRUE)) %>%
  		rename(p_inh = .data$dst_p_inh, 
  				dst_p_inh = .data$dst_p_inh2) %>%
 		select(- .data$p_inh, - .data$hain_inh)

 # aggregate SLI & FQ results
 	x <- x %>%
 		mutate(dst_p_sli = pmax(.data$dst_p_cm, .data$dst_p_km, na.rm = T)) %>%
 		mutate(dst_p_fq = pmax(.data$dst_p_ofx, .data$dst_p_mfx, na.rm = T))

# ===================================================
# Johanna's method
	# take the closest specimen to treatment start
	# if rif resistant find pre-tx 2nd line DST within 30 days

	# find rif result closest to treatment start
  	baseline_spec <- x %>%	
  	# remove un-used variables	
  		select(.data$id, .data$labno, .data$starttre, .data$samp_date,
         .data$abs, .data$rif_res) %>%

  	# remove identical results from the same day
  		distinct(.data$id, .data$abs, .data$rif_res, .keep_all = T) %>%

  	# take more resistant specimen when discordant results on same day
  		group_by(.data$id, .data$abs) %>%
  		top_n(1, .data$rif_res) %>%
      ungroup() %>%

	# group data and find closest to treatment start
  		group_by(.data$id) %>%
  		top_n(1, desc(.data$abs)) %>%
      ungroup() %>%

  	# select key variables
  		rename(baseline_date = .data$samp_date,
  				baseline_no = .data$labno,
  				base_rif = .data$rif_res) %>%
  		select(.data$id, .data$baseline_no, .data$baseline_date, .data$base_rif)

# ===================================================
# merge baseline_spec with original data
	merged <- left_join(x, baseline_spec, by = "id")

# generate drug specific baseline DSTs
#	dst_drugs <- str_which(names(merged), pattern = "dst_p_")
#	drg_var <- quos(names(merged)[dst_drugs])
#	drugs <- map_dfc(.f = drug_baseliner)
	h_dst <- drug_baseliner(merged, dst_p_inh, days = dst_days)
  	z_dst <- drug_baseliner(merged, dst_p_pza, days = dst_days)
  	e_dst <- drug_baseliner(merged, dst_p_eth, days = dst_days)
  	cm_dst <- drug_baseliner(merged, dst_p_cm, days = dst_days)
  	km_dst <- drug_baseliner(merged, dst_p_km, days = dst_days)
  	ofx_dst <- drug_baseliner(merged, dst_p_ofx, days = dst_days)
  	mfx_dst <- drug_baseliner(merged, dst_p_mfx, days = dst_days)
  	sli_dst <- drug_baseliner(merged, dst_p_sli, days = dst_days)
  	fq_dst <- drug_baseliner(merged, dst_p_fq, days = dst_days)

all_dst <- baseline_spec %>%
	left_join(h_dst, by = "id") %>%
	left_join(z_dst, by = "id") %>%
	left_join(e_dst, by = "id") %>%
	left_join(cm_dst, by = "id") %>%
	left_join(km_dst, by = "id") %>%
	left_join(ofx_dst, by = "id") %>%
	left_join(mfx_dst, by = "id") %>%
	left_join(sli_dst, by = "id") %>%
	left_join(fq_dst, by = "id")

dst_cat_all <- all_dst %>%
	mutate(ds = as.numeric(.data$base_rif == 1 & (.data$base_inh == 1 | is.na(.data$base_inh))),
		   mono_inh = as.numeric(.data$base_rif == 1 & .data$base_inh == 2), 
		   rres = as.numeric(.data$base_rif == 2 & (.data$base_inh == 1 | is.na(.data$base_inh))),
		   mdr = as.numeric(.data$base_rif == 2 & .data$base_inh == 2),
		   pre_fq = as.numeric((.data$mdr == 1 | .data$rres == 1) & .data$base_fq == 2 & 
		   				(.data$base_sli == 1 | is.na(.data$base_sli))),
		   pre_sli = as.numeric((.data$mdr == 1 | .data$rres == 1) & .data$base_sli == 2 & 
		   				(.data$base_fq == 1 | is.na(.data$base_fq))),
		   xdr = as.numeric((.data$mdr == 1 | .data$rres == 1) & 
                              .data$base_fq == 2 & .data$base_sli == 2),
		   mdr = as.numeric(.data$base_rif == 2 & .data$base_inh == 2 &
		   				.data$pre_fq != 1 & .data$pre_sli != 1 & .data$xdr != 1),
		   rres = as.numeric(.data$base_rif == 2 & (.data$base_inh == 1 | is.na(.data$base_inh)) &
		   				.data$pre_fq != 1 & .data$pre_sli != 1 & .data$xdr != 1))


dst_gather <- dst_cat_all %>%
	select(.data$id, .data$ds, .data$mono_inh, .data$rres, 
          .data$mdr, .data$pre_fq, .data$pre_sli, .data$xdr) %>%
	gather(dst_base, count, -.data$id) %>%
	filter(.data$count >= 1) %>%
	select(.data$id, .data$dst_base) 

final_dst <- left_join(dst_cat_all, dst_gather, by = "id")


final_dst
}

