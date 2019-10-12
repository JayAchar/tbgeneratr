#' @inherit dst_baseliner
#' @importFrom dplyr filter mutate select rename group_by slice
#' ungroup arrange left_join distinct bind_cols right_join case_when top_n
#' @importFrom stringr str_which
#' @importFrom purrr map_at map_dfc map 
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
    base_dst <- data.frame(APID = adm$APID,
                           stringsAsFactors = FALSE)
    
  # generate aggregate drug specific baseline DSTs
  aggregate_drugs <- c("rif_res",
                       "inh_res", 
                       "dst_p_pza",
                       "dst_p_eth",
                       "dst_p_km",
                       "dst_p_cm",
                       "dst_p_ofx",
                       "dst_p_mfx",
                       "sli_res",
                       "fq_res")
  
  # reapply object class
  class(merged) <- start_class

  base_dst <- purrr::map(aggregate_drugs, .f = ~drug_baseliner(x = merged, drug = .x)) %>% 
    # reduce from list to data frame
    purrr::reduce(left_join, by = "APID") %>% 
    # merge with all APID numbers to generate NAs
    left_join(base_dst, ., by = "APID")

  
  # generate baseline DST variable 
  data <- base_dst %>% 
    mutate(base_dst = dplyr::case_when(
      base_rif    == "Resistant" &
        base_sli  == "Resistant" &
        base_fq  == "Resistant" ~ "XDRTB",
      base_rif    == "Resistant" &
        base_sli  == "Resistant" &
        (base_fq  == "Sensitive" | is.na(base_fq)) ~ "pre-XDRTB (SLI)",
      base_rif    == "Resistant" &
        (base_sli == "Sensitive" | is.na(base_sli)) &
        base_fq   == "Resistant" ~ "pre-XDRTB (FQ)",
      base_rif    == "Resistant" &
        (base_inh == "Sensitive" | is.na(base_inh)) &
        (base_sli == "Sensitive" | is.na(base_sli)) &
        (base_fq  == "Sensitive" | is.na(base_fq)) ~ "RRTB",
      base_rif    == "Resistant" &
        base_inh  == "Resistant" ~ "MDRTB",
      (base_rif   == "Sensitive" | is.na(base_rif)) &
        base_inh  == "Resistant" ~ "Inh-mono",
      base_rif    == "Sensitive" &
        base_inh  == "Resistant" &
        (base_pza == "Resistant" | base_eth == "Resistant") ~ "PDRTB",
      base_rif    == "Sensitive" ~ "DSTB",
      TRUE ~ NA_character_))

  # convert base_dst to ordered factor variable
  data$base_dst <- factor(data$base_dst, 
                          levels = c("DSTB",
                                     "PDRTB",
                                     "Inh-mono",
                                     "RRTB",
                                     "MDRTB",
                                     "pre-XDRTB (SLI)",
                                     "pre-XDRTB (FQ)",
                                     "XDRTB"),
                          ordered = TRUE)
  
  # reapply object class
  class(data) <- start_class

  data
  }







