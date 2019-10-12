#' @inherit fail_no_culture_conversion
#' @importFrom assertthat assert_that is.date
#' @importFrom dplyr left_join

fail_no_culture_conversion.epiinfo <- function(adm_lab,
                                               no_cc_days = 240) {
  
  # check data has been cleaned with tbcleanr
  assertthat::assert_that(all(c("ds_dr", "base_culture", "cc_date") %in% names(adm_lab)),
                          is.factor(adm_lab$ds_dr),
                          is.ordered(adm_lab$base_culture),
                          assertthat::is.date(adm_lab$cc_date))
  
  # define database date
  dbdate <- max(max(adm_lab$STARTTRE, na.rm = TRUE),
                max(adm_lab$DATEN, na.rm = TRUE))
  
  # remove records with missing DR-TB status and missing basline culture data
  x <- adm_lab[!is.na(adm_lab$ds_dr) &
                 ! is.na(adm_lab$base_culture) & 
                 ! is.na(adm_lab$STARTTRE), ]
  
  
  # filter DR-TB and baseline culture positive
  x <- x[x$ds_dr == "DR-TB" & 
                 x$base_culture == "Positive", ]
  
  # check patient ID is unique
  assertthat::assert_that(length(unique(x$APID)) == nrow(x))
  
  # check culture conversion date is before end date and after start date
  incompatible_cc <- which(x$cc_date > x$DATEN | x$cc_date < x$STARTTRE)
  
  if (length(incompatible_cc > 0)) {
    message(paste0(length(incompatible_cc), " records with cc_date before start or",
                            " after end of treatment."))
    
    # remove records where culture conversion date is incompatible
    x <- x[- incompatible_cc, ]
  }
  
  # impute dbdate if no end date and no cc_time in raw data
  x$DATEN <- as.Date(ifelse(is.na(x$DATEN) & is.na(x$cc_date),
                    dbdate, 
                    x$DATEN),
                    origin = "1970-01-01")
  
  # if treatment duration is > no_cc_days and is.na(cc_date) == fail_no_cc
  x$fail_no_cc <- ifelse(x$DATEN - x$STARTTRE > no_cc_days & is.na(x$cc_date),
                         1, 0)
  
  # if cc_date is after no_cc_days or is.na(cc_date) from treatment start == fail_no_cc
  x$fail_no_cc <- ifelse(x$cc_date - x$STARTTRE > no_cc_days &
                           ! is.na(x$cc_date), 
                         1, x$fail_no_cc)
  
  # generate date of new fail outcome
  x$fail_no_cc_dt <- as.Date(ifelse(x$fail_no_cc == 1, 
                            x$STARTTRE + no_cc_days, 
                            NA_integer_),
                            origin = "1970-01-01")
    
  # merge with original records and output binary fail variable and date
  out <- dplyr::left_join(x = adm_lab[, "APID", drop = FALSE], 
                   y = x[, c("APID", "fail_no_cc", "fail_no_cc_dt"), drop = FALSE], 
                   by = "APID")
  
  # convert all is.na(fail_no_cc) to 0
  out$fail_no_cc[is.na(out$fail_no_cc)] <- 0L
  
  out
}
