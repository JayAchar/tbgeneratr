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
#' @author Jay Achar 
#' @seealso \code{\link{tbgeneratr}}
#' @importFrom dplyr filter mutate select rename group_by slice
#' ungroup arrange left_join distinct bind_cols right_join case_when top_n
#' @importFrom stringr str_which
#' @importFrom purrr map_at map_dfc map 
#' @importFrom rlang .data sym
#' @importFrom assertthat assert_that
#' @export
#' 

dst_baseliner.koch6 <- function(adm, lab, 
                                  dst_time = 90,
                                  dst_days = 30) {
  . <- NULL
  # save adm class
  start_class <- class(adm)
  
  # if names in lab data set include 
    # grozny lab data
  if ("grozny" %in% class(lab)) {
    lab_id <- "dstnumber"
    start <- "Starttre"
    id    <- "registrationnb"
    drug_strings <- c("_rif", "_inh", "_cm$|_am$", "_ofx$|_lfx$|_mfx")
    aggregate_drugs <- c("rif_res",
                         "inh_res", 
                         "dst_p_pza",
                         "dst_p_eth",
                         "dst_p_am",
                         "dst_p_cm",
                         "dst_p_lfx",
                         "dst_p_mfx",
                         "dst_p_mfxhigh",
                         "sli_res",
                         "fq_res")
  } else if ("epiinfo" %in% class(lab)) {
    lab_id <- c("registrationnb" = "APID")
    id <- "registrationnb"
    start <- "Starttre"
    drug_strings <- c("_rif", "_inh", "_cm$|_km$", "_ofx$|_lfx$|_mfx")
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
  }
  
  id_sym <- rlang::sym(id)
  start_sym <- rlang::sym(start)

  # merge adm and lab data 
  data <- dplyr::left_join(adm, lab, by = lab_id)

  # clean baseline data and define eligible period for consideration
  data <- data %>%
    filter(! is.na(.data$samp_date)) %>%
    # remove specimens > 7 days sfter treatment start
    filter(.data$samp_date - !! start_sym < 7) %>%
    # generate absolute days from sample to start treatment
    mutate(abs = as.numeric(abs(!! start_sym - .data$samp_date))) %>%
    # remove specimens > time from starttre
    filter(.data$abs < dst_time)


  # aggregate drug dst results by specimen  
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
    distinct(!! id_sym, .data$abs, .data$rif_res, .keep_all = T) %>% 
    
    # take more resistant specimen when discordant results on same day
    group_by(!! id_sym, .data$abs) %>%
    top_n(1, .data$rif_res) %>%
    ungroup() %>%
    
    # group data and find closest to treatment start
    group_by(!! id_sym) %>%
    top_n(1, desc(.data$abs)) %>%
    ungroup() %>%
    
    # select key variables
    rename(baseline_date = .data$samp_date,
           # baseline_no = .data$MICRLABN,
           base_rif = .data$rif_res) %>%
    select(!! id_sym, .data$baseline_date, .data$base_rif)
  
  # check that each ID/APID has a single baseline specimen
  assert_that(nrow(baseline_spec) == length(unique(baseline_spec[[id]])))
  
  
  # # ===================================================
  # merge baseline_spec with original data
  merged <- left_join(dst_data, baseline_spec, by = id)
  
  # data frame of baseline dst results
  base_dst <- data.frame(registrationnb = adm$registrationnb,
                         stringsAsFactors = FALSE)
  
  # reapply object class
  class(merged) <- start_class

  
  base_dst <- purrr::map(aggregate_drugs, .f = ~drug_baseliner(x = merged, drug = .x)) %>% 
    # reduce from list to data frame
    purrr::reduce(left_join, by = id) %>% 
    # merge with all APID numbers to generate NAs
    left_join(base_dst, ., by = id)
  
  
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
