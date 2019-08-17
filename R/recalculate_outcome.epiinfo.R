#' Recalculate outcome
#'
#' @inherit recalculate_outcome description
#' 
#' @inherit recalculate_outcome details
#' 
#' @inheritParams recalculate_outcome
#' @author Jay Achar
#' @seealso \code{\link{tbgeneratr}}
#' @inherit recalculate_outcome return
#' @importFrom assertthat assert_that
#' @importFrom dplyr left_join 
#' @importFrom purrr map_int modify_at reduce

recalculate_outcome.epiinfo <- function(adm,
                                adhere,
                                change,
                                lab,
                                stop_days = 30,
                                no_cc_days = 240) {
  
  # calculate baseline DST results
  dst <- tbgeneratr::dst_baseliner(adm, lab)
  
  # add baseline DST results to admission data
  adm_dst <- dplyr::left_join(adm, dst, by = "APID")
  
  # generate adm_lab using tbgeneratr
  adm_lab <- tbgeneratr::adm_lab_generator(adm, lab)
  
  # check whether fail_amplify is true
  amplify <- fail_amplify.epiinfo(adm = adm_dst, 
                       lab = lab)
  
  no_cc <- fail_no_culture_conversion.epiinfo(adm_lab = adm_lab, 
                                     no_cc_days = no_cc_days)
  
  revert <- fail_culture_reversion.epiinfo(adm = adm_lab, 
                                     lab = lab)
  
  ltfu <- fail_ltfu.epiinfo(adm = adm, 
                    adhere = adhere)
  
  drugs <- fail_drugs.epiinfo(adm = adm, 
                     change = change, 
                     stop_days = stop_days)
  
  # generate list of fail_ dfs
  failures <- list(
    amplify = amplify,
    drugs = drugs,
    ltfu = ltfu,
    no_cc = no_cc,
    revert = revert
  )
  
  # confirm all failure data frames have the same nrow()
  assertthat::assert_that(length(unique(purrr::map_int(failures, nrow))) == 1L)
  
  # merge all failure data frames
  fails <- purrr::reduce(failures, dplyr::left_join, by = "APID") 
  
  # merge original start and end treatment dates
  fails <- dplyr::left_join(adm[, c("APID", "STARTTRE", "DATEN")], fails, by = "APID")
  
  # any fail dates outside start and end are changed to NA
  dates <- grep(pattern = "_dt$", x = names(fails))
  assertthat::assert_that(length(dates) == length(failures))
  
  fails <- purrr::modify_at(.x = fails,
                   .at = dates,
                   .f = ~ as.Date(ifelse(.x >= fails$DATEN | .x <= fails$STARTTRE, NA_integer_,
                                         .x), origin = "1970-01-01"))
  
  # confirm that one row per record
  assertthat::assert_that(length(unique(fails$APID)) == nrow(fails))
  
  # find earliest dates per record
  ## keep rows with >= 1 failure date
  # fail_vars <- stringr::str_which(names(fails), pattern = "^fail_[a-z]*[_]*[c]*$")
  
  fails$cum_fails <- rowSums(! is.na(fails[dates]))
  
  fails <- fails[fails$cum_fails > 0, ]
  
  # ## columns with dates
  fails$fail_date <- as.Date(apply(fails[, dates], 1, min, na.rm = TRUE), origin = "1970-01-01")
  
  fails[c("APID", "fail_date")]
}


