#' Fail - culture reversion
#'
#' This function detects whether a record diagnosed with DR-TB
#' should receive a failure outcome due to culture reversion
#' during the continuation phase. The WHO 2014 definitions suggests this
#' time-point to be 8 months after treatment commencement.
#' 
#' @param adm data frame of individual patient admission records cleaned 
#' with `tbcleanr` and combined with laboratory data using `tbgeneratr::adm_lab_generator()`
#' @param lab data frame of laboratory TB data cleaned using `tbcleanr`
#' @author Jay Achar
#' @seealso \code{\link{tbgeneratr}}
#' @importFrom assertthat assert_that


fail_culture_reversion <- function(adm,
                                   lab) {
  
  # check args
  assert_that(is.data.frame(adm),
              is.data.frame(lab))
  
  UseMethod("fail_culture_reversion", adm)
  
}


#' @inherit fail_culture_reversion

fail_culture_reversion.default <- function(adm, lab) {
  
  message("No object class detected: fail_culture_reversion() not applied.")
  adm
  
}
