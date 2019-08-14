#' Fail - 2 drugs changed 
#'
#' This function detects whether a record diagnosed with DR-TB
#' should receive a failure outcome due to fulfilling the criteria for loss to follow-up.
#' 
#' @param adm data frame of individual patient admission records cleaned 
#' with `tbcleanr`.
#' @param change data frame of individual patient follow-up data including
#' monthly drug-specific adherence data cleaned by `tbcleanr::adhere_cleanr()`
#' @author Jay Achar
#' @seealso \code{\link{tbgeneratr}}
#' @importFrom assertthat assert_that

fail_drugs <- function(adm,
                      change) {
  
  
  # check args
  assert_that(is.data.frame(adm),
              is.data.frame(change))
  
  UseMethod("fail_change", adm)
  
}


#' Fail - 2 drugs changed
#'
#' @inheritParams fail_drugs
#' @author Jay Achar
#' @seealso \code{\link{tbgeneratr}}

fail_drugs.default <- function(adm, change) {
  
  message("No object class detected: fail_drugs() not applied.")
  adm
  
}
