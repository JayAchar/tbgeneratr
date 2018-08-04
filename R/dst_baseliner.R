

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
  		filter(! is.na(samp_date)) %>%
  	# remove specimens > 7 days sfter treatment start
  		filter(samp_date - starttre < 7) %>%
  	# generate absolute days from sample to start treatment
  		mutate(abs = as.numeric(abs(starttre - samp_date))) %>%
  	# remove specimens > time from starttre
  		filter(abs < time)

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
  		filter(! (is.na(xpert_rif) & is.na(hain_rif) & is.na(dst_p_rif))) %>%
  	# take most resistant specimen - 1 = sensitive, 2 = resistant
  		mutate(rif_res = pmax(xpert_rif, hain_rif, dst_p_rif, na.rm = TRUE)) %>%
   		select(-xpert_rif, -hain_rif, -dst_p_rif)

 # aggregate isoniazid results
  	x <- x %>%
  		mutate(dst_p_inh2 = pmax(hain_inh, dst_p_inh, na.rm = TRUE)) %>%
  		rename(p_inh = dst_p_inh, 
  				dst_p_inh = dst_p_inh2) %>%
 		select(- p_inh, - hain_inh)

 # aggregate SLI & FQ results
 	x <- x %>%
 		mutate(dst_p_sli = pmax(dst_p_cm, dst_p_km, na.rm = T)) %>%
 		mutate(dst_p_fq = pmax(dst_p_ofx, dst_p_mfx, na.rm = T))

# ===================================================
# Johanna's method
	# take the closest specimen to treatment start
	# if rif resistant find pre-tx 2nd line DST within 30 days

	# find rif result closest to treatment start
  	baseline_spec <- x %>%	
  	# remove un-used variables	
  		select(idno, labno, starttre, samp_date, abs,
  					rif_res) %>%
  	# remove identical results from the same day
  		group_by(idno, abs, rif_res) %>%
  		slice(1) %>% 
  		ungroup() %>%
  	# take more resistant specimen when discordant results on same day
  		group_by(idno, abs) %>%
  		arrange(idno, abs, desc(rif_res)) %>%
  		slice(1) %>%
	# group data and find closest to treatment start
  		group_by(idno) %>%
  		arrange(idno, abs) %>%
  		slice(1) %>%
  	# select key variables
  		rename(baseline_date = samp_date,
  				baseline_no = labno,
  				base_rif = rif_res) %>%
  		select(idno, baseline_no, baseline_date, base_rif)

# ===================================================
# merge baseline_spec with original data
	merged <- left_join(x, baseline_spec, by = "idno")

# generate drug specific baseline DSTs
#	dst_drugs <- str_which(names(merged), pattern = "dst_p_")
#	drg_var <- quos(names(merged)[dst_drugs])
#	drugs <- map_dfc(.f = drug_baseliner)
	h_dst <- drug_baseliner(merged, dst_p_inh, days = days)
  	z_dst <- drug_baseliner(merged, dst_p_pza, days = days)
  	e_dst <- drug_baseliner(merged, dst_p_eth, days = days)
  	cm_dst <- drug_baseliner(merged, dst_p_cm, days = days)
  	km_dst <- drug_baseliner(merged, dst_p_km, days = days)
  	ofx_dst <- drug_baseliner(merged, dst_p_ofx, days = days)
  	mfx_dst <- drug_baseliner(merged, dst_p_mfx, days = days)
  	sli_dst <- drug_baseliner(merged, dst_p_sli, days = days)
  	fq_dst <- drug_baseliner(merged, dst_p_fq, days = days)

all_dst <- baseline_spec %>%
	left_join(h_dst, by = "idno") %>%
	left_join(z_dst, by = "idno") %>%
	left_join(e_dst, by = "idno") %>%
	left_join(cm_dst, by = "idno") %>%
	left_join(km_dst, by = "idno") %>%
	left_join(ofx_dst, by = "idno") %>%
	left_join(mfx_dst, by = "idno") %>%
	left_join(sli_dst, by = "idno") %>%
	left_join(fq_dst, by = "idno")

dst_cat_all <- all_dst %>%
	mutate(ds = as.numeric(base_rif == 1 & (base_inh == 1 | is.na(base_inh))),
		   mono_inh = as.numeric(base_rif == 1 & base_inh == 2), 
		   rres = as.numeric(base_rif == 2 & (base_inh == 1 | is.na(base_inh))),
		   mdr = as.numeric(base_rif == 2 & base_inh == 2),
		   pre_fq = as.numeric((mdr == 1 | rres == 1) & base_fq == 2 & 
		   				(base_sli == 1 | is.na(base_sli))),
		   pre_sli = as.numeric((mdr == 1 | rres == 1) & base_sli == 2 & 
		   				(base_fq == 1 | is.na(base_fq))),
		   xdr = as.numeric((mdr == 1 | rres == 1) & base_fq == 2 & base_sli == 2),
		   mdr = as.numeric(base_rif == 2 & base_inh == 2 &
		   				pre_fq != 1 & pre_sli != 1 & xdr != 1),
		   rres = as.numeric(base_rif == 2 & (base_inh == 1 | is.na(base_inh)) &
		   				pre_fq != 1 & pre_sli != 1 & xdr != 1))


dst_gather <- dst_cat_all %>%
	select(idno, ds, mono_inh, rres, mdr, pre_fq, pre_sli, xdr) %>%
	gather(dst_base, count, -idno) %>%
	filter(count >= 1) %>%
	select(idno, dst_base) 

final_dst <- left_join(dst_cat_all, dst_gather, by = "idno")


final_dst
}




# Very clearly document the methods of this function
# Adjust to ensure it works with K6 data too










