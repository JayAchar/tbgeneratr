#' @inherit fail_culture_reversion
#' @importFrom assertthat assert_that
#' @importFrom dplyr left_join arrange mutate ungroup group_by filter slice 
#' @importFrom magrittr %>% 
#' @importFrom rlang .data

fail_culture_reversion.epiinfo <- function(adm,
                                           lab) {
  
  # confirm ID numbers in adm file are unique
  assertthat::assert_that(nrow(adm) == length(unique(adm$APID)))
  
  # join data
  x <- dplyr::left_join(adm, lab, by = "APID")
  
  # filter data without id
  x <- x[! is.na(x$APID), ]
  
  # filter DS-TB
  x <- x[x$ds_dr == "DR-TB", ]
  
  # filter baseline culture negative 
  x <- x[x$base_culture == "Positive", ]
  
  # filter incompatible culture conversion date
  incompatible_cc <- which(x$cc_date > x$DATEN | x$cc_date < x$STARTTRE)
  
  if (length(incompatible_cc > 0)) {
    x <- x[- incompatible_cc, ]
  }
  
  # filter no culture conversion date
  x <- x[! is.na(x$cc_date), ]
  
  # filter rows where is.na(culture)
  x <- x[! is.na(x$culture), ]
  
  # filter rows where is.na(samp_date)
  x <- x[! is.na(x$samp_date), ]
  
  # filter all specimens from intensive phase (defined as first 8 months)
  x <- x[x$samp_date > x$STARTTRE + 240, ]
  
  # remove ID, samp_date and culture duplicates
  x <- x[! duplicated(x[c("APID", "samp_date", "culture")]), ]
  
  # keep positive specimen where date and ID are duplicated
  x <- x[order(x$APID, x$samp_date, desc(x$culture)), ]
  x <- x[! duplicated(x[c("APID", "samp_date")]), ]
  
  # calculate culture reversion
  x <- x %>%
    arrange(.data$APID, .data$samp_date) %>%
    # find consecutive same results
    mutate(result_grp = cumsum(as.character(.data$culture)!=lag(as.character(.data$culture),default=""))) %>%
    ungroup() %>%
    group_by(.data$APID, .data$result_grp) %>%
    # keep all consecutive results which are negative and have total > 30 days
    filter(.data$culture == "Positive" & (max(.data$samp_date) - min(.data$samp_date)) >= 30) %>%
    ungroup() %>%
    # keep only the first episode of culture conversion
    group_by(.data$APID) %>%
    filter(.data$result_grp == min(.data$result_grp)) %>%
    arrange(.data$samp_date) %>% 
    slice(1) %>% 
    ungroup() %>%
    mutate(fail_reversion = 1L) %>% 
    select(.data$APID, .data$fail_reversion, "fail_reversion_dt" := .data$samp_date)
  
  # merge with original records
  out <- dplyr::left_join(x = adm[, "APID", drop = FALSE], 
                          y = x, 
                          by = "APID")
  
  # convert all is.na(fail_reversion) to 0
  out$fail_reversion[is.na(out$fail_reversion)] <- 0L
  
  # correct output class
  class(out) <- class(adm)
  
  out
  
}
