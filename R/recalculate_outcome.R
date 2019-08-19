#' Recalculate outcome
#'
#' @description This function detects whether DR-TB failure criteria have been 
#' encourntered during the course of a treatment episode.
#' 
#' @details The purpose of this function is to detect whether failure criteria for DR-TB
#' have occured during the course of a treatment record and to propose a new "failure" date.
#' 
#' This proposed failure date may then be compared to the recorded failure date to see 
#' whether a new treament episode should be defined for any ongoing treatment being provided. This 
#' decision may depend upon, amongst other things, whether the ongoing treatment length was significant,
#' or whether a new regimen was initiated. 
#' 
#' @param adm data frame of individual patient admission records cleaned 
#' with `tbcleanr`.
#' @param adhere data frame of individual patient follow-up data including
#' monthly drug-specific adherence data cleaned by `tbcleanr::adhere_cleanr()`
#' @param change data frame of individual patient follow-up data including
#' monthly drug-specific change data cleaned by `tbcleanr::change_cleanr()`
#' @param lab data frame of laboratory TB data cleaned using `tbcleanr`
#' @param stop_days define interval in days where two drugs being stopped would result in failure. 
#' @param no_cc_days time cut-off in days to define failure to culture convert - default = 240 days
#' @author Jay Achar
#' @seealso \code{\link{tbgeneratr}}
#' @return tibble with same class as input admission data with two columns. The first represents
#' the unique record ID number and second "fail_date" identifies all dates where a failure criteria
#' was detected within the treatment course. 
#' @importFrom assertthat assert_that
#' @export

recalculate_outcome <- function(adm,
                                adhere,
                                change,
                                lab,
                                stop_days = 30,
                                no_cc_days = 240) {
  
  
  
  # check args
  assert_that(is.data.frame(adm),
              is.data.frame(adhere), 
              is.data.frame(change),
              is.data.frame(lab),
              is.numeric(stop_days),
              length(stop_days) == 1L)
  
  UseMethod("recalculate_outcome", adm)
  
}


#' Recalculate outcome
#'
#' @inheritParams recalculate_outcome
#' @author Jay Achar
#' @seealso \code{\link{tbgeneratr}}

recalculate_outcome.default <- function(adm,
                              adhere,
                              change,
                              lab,
                              stop_days = 30) {
  
  message("No object class detected: recalculate_outcome() not applied.")
  adm
  
}
