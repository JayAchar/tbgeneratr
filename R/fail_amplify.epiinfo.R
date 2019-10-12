#' @inherit fail_amplify
#' @importFrom assertthat assert_that
#' @importFrom dplyr left_join arrange group_by slice inner_join
#' @importFrom purrr map map_int
#' @importFrom stringr str_which
#' @importFrom stats complete.cases

fail_amplify.epiinfo <- function(adm,
                                 lab) {

  # confirm presence of aggregated DST variables
  assertthat::assert_that(all(c("base_fq", "base_sli") %in% names(adm)))
  
  # confirm ID numbers in adm file are unique
  assertthat::assert_that(nrow(adm) == length(unique(adm$APID)))
  
  # remove NA's from admission data
  adm_clean <- adm[complete.cases(adm[ , c("APID", "ds_dr", "STARTTRE")]), ]

  # remove NA's from lab data
  lab_clean <- lab[complete.cases(lab[ , c("APID", "samp_date")]), ]
  
  # join data - use inner_join to keep only records with samples
  x <- dplyr::inner_join(adm_clean, lab_clean, by = "APID")
  
  # remove specimens before treatment start or after treatment end
  x <- x[x$samp_date >= x$STARTTRE &
           x$samp_date <= x$DATEN, ]
  
  # function to define fail_amplify logic
  detect_amplify <- function(base, dst1, dst2) {
    # if is.na(base_dst) assume resistant
    base[is.na(base)] <- "Resistant"
    
    # if is.na(sample_dst) assume sensitive so that NA does not result in failure
    dst1[is.na(dst1)] <- "Sensitive"
    dst2[is.na(dst2)] <- "Sensitive"
    
    # fail if Sensitive to resistan
    as.integer(base == "Sensitive" & (dst1 == "Resistant" | dst2 == "Resistant"))
  }
  
  # drug names
  drugs <- list(
  fq_match <- c("_fq$", "^dst_p_ofx$", "^dst_p_mfx$"),
  sli_match <- c("_sli$", "^dst_p_km$", "^dst_p_cm$")
  )
  
  # dataframe of detect_amplify outputs 
  amplify_df <-
    purrr::map(drugs, .f = ~purrr::map_int(.x, function(y) stringr::str_which(names(x), pattern = y))) %>% 
      purrr::map(.f = ~ detect_amplify(base = x[[.x[1]]], 
                                       dst1 = x[[.x[2]]],
                                       dst2 = x[[.x[3]]]
                                       )) %>% 
      data.frame()
    names(amplify_df) <- c("fq_amplify", "sli_amplify")
  
  assertthat::assert_that(nrow(amplify_df) == nrow(x))
  
  # generate amplify_sample vector
  x$amplify_sample <- apply(amplify_df, 1, max, na.rm = TRUE)
  
  # keep all samples with amplify_sample == 1
  x <- x[x$amplify_sample == 1, ]
  
  # keep earliest specimen within each patient's samples
  x <- x %>% 
    group_by(.data$APID) %>% 
    arrange(.data$APID, .data$samp_date) %>% 
    slice(1)
  
  # generate binary fail_amplify variable 
  x <- x[, c("APID", "amplify_sample", "samp_date")]
  
  # use samp_date as fail_amplify_dt variable
  names(x) <- c("APID", "fail_amplify", "fail_amplify_dt")
  
  # merge with original records
  out <- dplyr::left_join(x = adm[, "APID", drop = FALSE], 
                          y = x, 
                          by = "APID")
  
  # convert all is.na(fail_no_cc) to 0
  out$fail_amplify[is.na(out$fail_amplify)] <- 0L
  
  # class of output
  class(out) <- class(adm)

  out
}
