#' Fail - no culture conversion
#'
#' This function detects with a record diagnosed with DR-TB
#' should receive a failure outcome due to lack of culture conversion
#' before a defined time-point. The WHO 2014 definitions suggests this
#' time-point to be 8 months after treatment commencement.
#' 
#' @param adm_lab data frame of individual patient records cleaned 
#' with `tbcleanr` and combined with laboratory data using `tbgeneratr::adm_lab_generator()`
#' @param no_cc_days time cut-off in days to define failure to culture convert. 
#' Default value (240 days) taken from 2014 WHO definition of 8 months
#' @author Jay Achar
#' @seealso \code{\link{tbgeneratr}}
#' @importFrom assertthat assert_that


fail_no_culture_conversion <- function(adm_lab,
                                       no_cc_days = 240) {
  
  # check args
  assert_that(is.data.frame(adm_lab),
              is.numeric(no_cc_days),
              length(no_cc_days) == 1)
  
  UseMethod("fail_no_culture_conversion", adm_lab)
  
}


#' @inherit fail_no_culture_conversion

fail_no_culture_conversion.default <- function(adm_lab) {
  
  message("No  object class detected: fail_no_culture_conversion() not applied.")
  adm_lab
  
}
