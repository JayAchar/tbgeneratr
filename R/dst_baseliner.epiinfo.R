#' Baseline DST calculator
#'
#' Takes a TB DST laboratory dataset with information on 
#' treatment start time to output the baseline DST according
#' to specific rules designed to reduce missingness
#' @param adm data frame containing TB admission data cleaned and allocated object
#' class by tbcleanr package
#' @param lab data frame containing TB laboratory data cleaned and allocated object
#' class by tbcleanr package
#' @param dst_time absolute historical limit for including specimens
#' @param dst_days additional criteria for including non-rif results
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbgeneratr}}
#' @importFrom dplyr filter mutate select rename group_by slice
#' ungroup arrange left_join distinct bind_cols right_join
#' @importFrom stringr str_which
#' @importFrom purrr map_at map_dfc map as_mapper
#' @importFrom tidyr gather
#' @importFrom rlang .data
#' @importFrom assertthat assert_that
#' @export

dst_baseliner.epiinfo <- function(adm, lab, 
                                  dst_time = 90,
                                  dst_days = 30) {
  . <- NULL
  
  # save adm class
  start_class <- class(adm)
  
  # merge adm and lab data 
  data <- dplyr::left_join(adm, lab, by = "APID")
  
  # clean baseline data and define eligible period for consideration
  data <- data %>%
    filter(! is.na(.data$samp_date)) %>%
    # remove specimens > 7 days sfter treatment start
    filter(.data$samp_date - .data$STARTTRE < 7) %>%
    # generate absolute days from sample to start treatment
    mutate(abs = as.numeric(abs(.data$STARTTRE - .data$samp_date))) %>%
    # remove specimens > time from starttre
    filter(.data$abs < dst_time)

  # aggregate drug dst results by specimen  
  drug_strings <- c("rif", "inh", "cm$|km$", "ofx$|lfx$|mfx")
  dst_data <- map_dfc(drug_strings, 
                  .f = ~aggregate_dst(x = data, drug_class = .x)) %>% 
    bind_cols(data, .)


  # ===================================================
  # Research definition
  ## aims to limit bias associated with number of pre-treatment DSTs performed
  ## take the closest specimen to treatment start
  ## if rif resistant find pre-tx 2nd line DST within 30 days

  # find rif result closest to treatment start
  baseline_spec <- dst_data %>%

    # remove identical results from the same day
    distinct(.data$APID, .data$abs, .data$rif_res, .keep_all = T) %>% 

    # take more resistant specimen when discordant results on same day
    group_by(.data$APID, .data$abs) %>%
    top_n(1, .data$rif_res) %>%
    ungroup() %>%

    # group data and find closest to treatment start
    group_by(.data$APID) %>%
    top_n(1, desc(.data$abs)) %>%
    ungroup() %>%

    # select key variables
    rename(baseline_date = .data$samp_date,
           baseline_no = .data$MICRLABN,
           base_rif = .data$rif_res) %>%
    select(.data$APID, .data$baseline_no, .data$baseline_date, .data$base_rif)
    
    # check that each ID/APID has a single baseline specimen
    assert_that(nrow(baseline_spec) == length(unique(baseline_spec$APID)))

  # # ===================================================
  # merge baseline_spec with original data
  merged <- left_join(dst_data, baseline_spec, by = "APID")
    
  
  # data frame of baseline dst results
    base_dst <- data.frame(APID = adm$APID)
    
  # generate aggregate drug specific baseline DSTs
  aggregate_drugs <- c("inh_res", "sli_res", "fq_res")
  
  # reapply object class
  class(merged) <- start_class
  
    ## define mapper for drug_baseliner and merge back to data
  drug_base_mapper <- purrr::as_mapper(.f = ~drug_baseliner(x = merged, drug = .x) %>% 
                                         dplyr::right_join(base_dst, by = "APID"))
  
  base_dst <- purrr::map(aggregate_drugs, .f = drug_base_mapper) %>% 
    data.frame(stringsAsFactors = FALSE) %>% 
    dplyr::select(- dplyr::matches("^APID."))
  
  return(base_dst)
  
  
  # #	dst_drugs <- str_which(names(merged), pattern = "dst_p_")
  # #	drg_var <- quos(names(merged)[dst_drugs])
  # #	drugs <- map_dfc(.f = drug_baseliner)
  # h_dst <- drug_baseliner(merged, dst_p_inh, days = dst_days)
  # z_dst <- drug_baseliner(merged, dst_p_pza, days = dst_days)
  # e_dst <- drug_baseliner(merged, dst_p_eth, days = dst_days)
  # cm_dst <- drug_baseliner(merged, dst_p_cm, days = dst_days)
  # mfx_dst <- drug_baseliner(merged, dst_p_mfx, days = dst_days)
  # sli_dst <- drug_baseliner(merged, dst_p_sli, days = dst_days)
  # fq_dst <- drug_baseliner(merged, dst_p_fq, days = dst_days)
  # 
  # if (project == "kk") {
  #   km_dst <- drug_baseliner(merged, dst_p_km, days = dst_days)
  #   ofx_dst <- drug_baseliner(merged, dst_p_ofx, days = dst_days)
  # } else if (project == "chechnya") {
  #   am_dst <- drug_baseliner(merged, dst_p_am, days = dst_days)
  #   lfx_dst <- drug_baseliner(merged, dst_p_lfx, days = dst_days)
  # }
  # 
  # 
  # all_dst <- baseline_spec %>%
  #   left_join(h_dst, by = "id") %>%
  #   left_join(z_dst, by = "id") %>%
  #   left_join(e_dst, by = "id") %>%
  #   left_join(cm_dst, by = "id") %>%
  #   
  #   
  #   left_join(mfx_dst, by = "id") %>%
  #   left_join(sli_dst, by = "id") %>%
  #   left_join(fq_dst, by = "id")
  # 
  # if (project == "kk") {
  #   all_dst <- all_dst %>%
  #     left_join(km_dst, by = "id") %>%
  #     left_join(ofx_dst, by = "id")
  # } else if (project == "chechnya") {
  #   all_dst <- all_dst %>%
  #     left_join(am_dst, by = "id") %>%
  #     left_join(lfx_dst, by = "id")
  # }
  # 
  # 
  # dst_cat_all <- all_dst %>%
  #   mutate(ds = as.numeric(.data$base_rif == 1 & (.data$base_inh == 1 | is.na(.data$base_inh))),
  #          mono_inh = as.numeric(.data$base_rif == 1 & .data$base_inh == 2), 
  #          rres = as.numeric(.data$base_rif == 2 & (.data$base_inh == 1 | is.na(.data$base_inh))),
  #          mdr = as.numeric(.data$base_rif == 2 & .data$base_inh == 2),
  #          pre_fq = as.numeric((.data$mdr == 1 | .data$rres == 1) & .data$base_fq == 2 & 
  #                                (.data$base_sli == 1 | is.na(.data$base_sli))),
  #          pre_sli = as.numeric((.data$mdr == 1 | .data$rres == 1) & .data$base_sli == 2 & 
  #                                 (.data$base_fq == 1 | is.na(.data$base_fq))),
  #          xdr = as.numeric((.data$mdr == 1 | .data$rres == 1) & 
  #                             .data$base_fq == 2 & .data$base_sli == 2),
  #          mdr = as.numeric(.data$base_rif == 2 & .data$base_inh == 2 &
  #                             .data$pre_fq != 1 & .data$pre_sli != 1 & .data$xdr != 1),
  #          rres = as.numeric(.data$base_rif == 2 & (.data$base_inh == 1 | is.na(.data$base_inh)) &
  #                              .data$pre_fq != 1 & .data$pre_sli != 1 & .data$xdr != 1))
  # 
  # 
  # dst_gather <- dst_cat_all %>%
  #   select(.data$id, .data$ds, .data$mono_inh, .data$rres, 
  #          .data$mdr, .data$pre_fq, .data$pre_sli, .data$xdr) %>%
  #   gather(dst_base, count, -.data$id) %>%
  #   filter(.data$count >= 1) %>%
  #   select(.data$id, .data$dst_base) 
  # 
  # final_dst <- left_join(dst_cat_all, dst_gather, by = "id")
  # 
  # 
  # final_dst
  
  class(data) <- start_class
merged
  
  }







