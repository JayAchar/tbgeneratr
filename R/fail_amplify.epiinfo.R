



fail_amplify.epiinfo <- function(adm,
                                 lab) {

  # confirm ID numbers in adm file are unique
  assertthat::assert_that(nrow(adm) == length(unique(adm$APID)))
  
  # remove NA's from admission data
  adm_clean <- adm[complete.cases(adm[ , c("APID", "ds_dr", "STARTTRE")]), ]

  # remove NA's from lab data
  lab_clean <- lab[complete.cases(lab[ , c("APID", "samp_date")]), ]
  
  # join data
  x <- dplyr::left_join(adm_clean, lab_clean, by = "APID")
  
  # remove specimens before treatment start or after treatment end
  
  
  
}