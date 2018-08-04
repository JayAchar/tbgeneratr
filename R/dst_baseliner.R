#' DST baseliner
#'
#' Generate a baseline DST for each subject using a
#' combination of laboratory DST data and treatment start 
#' and end dates. Detects rifampicin result closest to 
#' treatment start, then uses specimens with same rifampicin
#' result within 'days' of this specimen to generate other drug
#' results. Only specimens up to 'time' before treatment start 
#' and < 7 days after treatment start are included
#' @param x data frame containing all DST data and start/end dates
#' @param project define which project's data is being used
#' @param time define pre-treatment period to use specimens
#' @param days define how many days to use to recover other drug DST
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbgeneratr}}
#' @export
#' @importFrom tbcleanr nse_renamer
#' @importFrom dplyr filter mutate select group_by ungroup row_number
#' arrange rename left_join
#' @importFrom stringr str_which
#' @importFrom tidyr gather
#' @importFrom rlang .data
#' 

# time arg to define how far to go historically to use samples


dst_baseliner <- function(x, project = c("kk", "chechnya"), 
							 time = 90, 
							 days = 30) {

# checks
# check input
	if (!(is.data.frame(x))) {
			stop("input paramter, x, must be a data frame")
	}

# check args
	project <- match.arg(project)

# ====================================================
# rename variables for NSE
	x <- nse_renamer(x, softwre = "koch_6", project = project,
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
  		filter(.data$abs < time)

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
  		filter(! (is.na(.data$xpert_rif) &
                is.na(.data$hain_rif) & 
                is.na(.data$dst_p_rif))) %>%
  	# take most resistant specimen - 1 = sensitive, 2 = resistant
  		mutate(rif_res = pmax(.data$xpert_rif, 
                            .data$hain_rif, 
                            .data$dst_p_rif, na.rm = TRUE)) %>%
   		select(-.data$xpert_rif, -.data$hain_rif, -.data$dst_p_rif)

 # aggregate isoniazid results
  	x <- x %>%
  		mutate(dst_p_inh2 = pmax(.data$hain_inh, .data$dst_p_inh, 
                          na.rm = TRUE)) %>%
  		rename(p_inh = .data$dst_p_inh, 
  				dst_p_inh = .data$dst_p_inh2) %>%
 		select(- .data$p_inh, - .data$hain_inh)

 # aggregate SLI & FQ results
 	x <- x %>%
 		mutate(dst_p_sli = pmax(.data$dst_p_cm, .data$dst_p_km, 
                            na.rm = T)) %>%
 		mutate(dst_p_fq = pmax(.data$dst_p_ofx, .data$dst_p_mfx, 
                            na.rm = T))

# ===================================================
# Johanna's method
	# take the closest specimen to treatment start
	# if rif resistant find pre-tx 2nd line DST within 30 days

	# find rif result closest to treatment start
  	baseline_spec <- x %>%	
  	# remove un-used variables	
  		select(.data$idno, .data$labno, .data$starttre, 
             .data$samp_date, .data$abs, .data$rif_res) %>%
  	# remove identical results from the same day
  		group_by(.data$idno, .data$abs, .data$rif_res) %>%
  		filter(row_number() ==1) %>% 
  		ungroup() %>%

  	# take more resistant specimen when discordant results on same day
  		group_by(.data$idno, .data$abs) %>%
  		arrange(.data$idno, .data$abs, desc(.data$rif_res)) %>%
  		filter(row_number() == 1) %>%
      ungroup() %>%
	# group data and find closest to treatment start
  		group_by(.data$idno) %>%
  		arrange(.data$idno, .data$abs) %>%
  		filter(row_number() == 1) 
  	# select key variables
  		rename(baseline_date = .data$samp_date,
  				baseline_no = .data$labno,
  				base_rif = .data$rif_res) %>%
  		select(.data$idno, .data$baseline_no, 
              .data$baseline_date, .data$base_rif)

# ===================================================
# merge baseline_spec with original data
	merged <- left_join(x, baseline_spec, by = "idno")

# generate drug specific baseline DSTs
#	dst_drugs <- str_which(names(merged), pattern = "dst_p_")
#	drg_var <- quos(names(merged)[dst_drugs])
#	drugs <- map_dfc(.f = drug_baseliner)
	h_dst <- drug_baseliner(merged, .data$dst_p_inh, days = days)
  	z_dst <- drug_baseliner(merged, .data$dst_p_pza, days = days)
  	e_dst <- drug_baseliner(merged, .data$dst_p_eth, days = days)
  	cm_dst <- drug_baseliner(merged, .data$dst_p_cm, days = days)
  	km_dst <- drug_baseliner(merged, .data$dst_p_km, days = days)
  	ofx_dst <- drug_baseliner(merged, .data$dst_p_ofx, days = days)
  	mfx_dst <- drug_baseliner(merged, .data$dst_p_mfx, days = days)
  	sli_dst <- drug_baseliner(merged, .data$dst_p_sli, days = days)
  	fq_dst <- drug_baseliner(merged, .data$dst_p_fq, days = days)

all_dst <- baseline_spec %>%
	left_join(.data$h_dst, by = "idno") %>%
	left_join(.data$z_dst, by = "idno") %>%
	left_join(.data$e_dst, by = "idno") %>%
	left_join(.data$cm_dst, by = "idno") %>%
	left_join(.data$km_dst, by = "idno") %>%
	left_join(.data$ofx_dst, by = "idno") %>%
	left_join(.data$mfx_dst, by = "idno") %>%
	left_join(.data$sli_dst, by = "idno") %>%
	left_join(.data$fq_dst, by = "idno")

dst_cat_all <- all_dst %>%
	mutate(ds = as.numeric(.data$base_rif == 1 & (.data$base_inh == 1 | is.na(.data$base_inh))),
		   mono_inh = as.numeric(.data$base_rif == 1 & .data$base_inh == 2), 
		   rres = as.numeric(.data$base_rif == 2 & (.data$base_inh == 1 | is.na(.data$base_inh))),
		   mdr = as.numeric(.data$ase_rif == 2 & .data$base_inh == 2),
		   pre_fq = as.numeric((.data$mdr == 1 | .data$rres == 1) & .data$base_fq == 2 & 
		   				(.data$base_sli == 1 | is.na(.data$base_sli))),
		   pre_sli = as.numeric((.data$mdr == 1 | .data$rres == 1) & .data$base_sli == 2 & 
		   				(.data$base_fq == 1 | is.na(.data$base_fq))),
		   xdr = as.numeric((.data$mdr == 1 | .data$rres == 1) & .data$base_fq == 2 & .data$base_sli == 2),
		   mdr = as.numeric(.data$base_rif == 2 & .data$base_inh == 2 &
		   				.data$pre_fq != 1 & .data$pre_sli != 1 & .data$xdr != 1),
		   rres = as.numeric(.data$base_rif == 2 & (.data$base_inh == 1 | is.na(.data$base_inh)) &
		   				.data$pre_fq != 1 & .data$pre_sli != 1 & .data$xdr != 1)) %>%

  # check only one DST per patient
      mutate(total = pmax(.data$ds, .data$mono_inh, .data$rres, .data$mdr,
                   .data$pre_fq, .data$pre_sli, .data$xdr))

    if (max(dst_cat_all$total, na.rm = TRUE) > 1) {
      warning("More than one DST detected per patient")
    }


dst_gather <- dst_cat_all %>%
	select(.data$idno, .data$ds, .data$mono_inh, .data$rres, .data$mdr,
         .data$pre_fq, .data$pre_sli, .data$xdr) %>%
	gather(.data$dst_base, .data$count, -.data$idno) %>%
	filter(.data$count >= 1) %>%
	select(.data$idno, .data$dst_base) 

final_dst <- left_join(dst_cat_all, dst_gather, by = "idno")


final_dst
}
