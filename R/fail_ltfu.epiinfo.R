#' Fail - LTFU 
#'
#' This function detects whether a record diagnosed with DR-TB
#' should receive a failure outcome due to fulfilling the criteria for loss to follow-up.
#' 
#' @inheritParams fail_ltfu
#' @author Jay Achar
#' @seealso \code{\link{tbgeneratr}}
#' @importFrom assertthat assert_that
#' @importFrom dplyr group_by mutate n filter bind_rows anti_join left_join select arrange top_n lag %>% desc
#' @importFrom purrr map
#' @importFrom stringr str_which
#' @importFrom lubridate add_with_rollback
#' @importFrom rlang .data
#' @importFrom stats median

fail_ltfu.epiinfo <- function(adm,
                              adhere) {
  
  
  assertthat::assert_that(is.data.frame(adm),
                          is.data.frame(adhere))
  
  # check that APID - tx_month combination is unique
  if (nrow(unique(adhere[c("APID", "tx_month")])) != nrow(adhere)) {
    warning("Duplicate monthly adherence data detected - check tbcleanr::adhere_cleanr() has been used.")
  }
  
  # select adm variables
  a <- adm[ , c("APID", "STARTTRE")]
  
  # fill adherence data with missing months
  # filter only rows where max(months) != nrow(months)
  d <- adhere %>%
    dplyr::group_by(.data$APID) %>%
    dplyr::mutate(n_months = base::max(.data$tx_month, na.rm = TRUE),
           n_rows = dplyr::n()) %>%
    dplyr::filter(!.data$n_months == .data$n_rows)
  
  # generate grid of months
  make_grid <- function(x, id) {
    x <- x[x$APID == id, ]
    
    max_month <- max(x$tx_month, na.rm = TRUE)
    
    expand.grid(APID = id,
                tx_month = 1:max_month, 
                stringsAsFactors = FALSE)
  }
  
  all_ids <- unique(d$APID)
  
  full_grid <- purrr::map(all_ids, .f = ~make_grid(d, .x)) %>% 
    dplyr::bind_rows()
  
  d <- merge(d, full_grid, all = TRUE) %>% 
    dplyr::anti_join(adhere, by = c("APID", "tx_month"))
  
  adhere <- dplyr::bind_rows(adhere, d)
  
  # remove extra columns
  adhere[ , c("n_months", "n_rows")] <- NULL
  
  # join data 
  data <- dplyr::left_join(a, adhere, by = "APID")
  
  # generate aggregate monthly adherence variable
    # e.g. median of drug-specific intake, or all drug-specific == 0
  data$median_adhere <- apply(data[ , stringr::str_which(names(data), 
                                                         pattern = "^adhere_pct_")], 
                              1, median, na.rm = TRUE)
  
  # detect two consecutive months of median adherence == 0
  out <- data %>%
    dplyr::select(.data$APID, .data$STARTTRE, .data$tx_month, .data$median_adhere) %>% 
    dplyr::group_by(.data$APID) %>% 
    dplyr::arrange(.data$APID, .data$tx_month) %>% 
    # flag second month of 0% median adherence across all drugs - therefore using lag()
    # any NA in median_adherence is treated as 100% = conservative approach
    dplyr::mutate(consec_median_zero = 
                    ifelse(.data$median_adhere == dplyr::lag(.data$median_adhere, 1, default = 100L) & 
                             .data$median_adhere <= 0L, 1, 0)) %>% 
    dplyr::filter(.data$consec_median_zero == 1) %>% 
    # keep the earliest ltfu record per patient
    dplyr::top_n(1, dplyr::desc(.data$tx_month))
    
  # check each record is one row
  assertthat::assert_that(length(unique(out$APID)) == nrow(out))
  
  # generate new date variable from start treatment and month - date should be the end of the second month
    # of 0% adherence
  out$fail_ltfu_dt <- lubridate::add_with_rollback(out$STARTTRE, months(out$tx_month), 
                                        roll_to_first = FALSE, preserve_hms = TRUE)
  
  # remove extra columsn ready for output
  out[, c("STARTTRE", "tx_month", "median_adhere")] <- NULL
  
  # rename variables
  names(out) <- c("APID", "fail_ltfu", "fail_ltfu_dt")
  
  # merge with admission data APID numbers
  # merge with original records and output binary fail variable and date
  out <- dplyr::left_join(x = a[, "APID", drop = FALSE], 
                          y = out, 
                          by = "APID")
  
  # convert all is.na(fail_no_cc) to 0
  out$fail_ltfu[is.na(out$fail_ltfu)] <- 0L
  
  out
  }



