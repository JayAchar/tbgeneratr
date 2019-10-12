#' Fail - LTFU 
#'
#' This function detects whether a record diagnosed with DR-TB
#' should receive a failure outcome due to fulfilling the criteria for loss to follow-up.
#' 
#' @param adm data frame of individual patient admission records cleaned 
#' with `tbcleanr`.
#' @param adhere data frame of individual patient follow-up data including
#' monthly drug-specific adherence data cleaned by `tbcleanr::adhere_cleanr()`
#' @author Jay Achar
#' @seealso \code{\link{tbgeneratr}}
#' @importFrom assertthat assert_that

fail_ltfu <- function(adm,
                      adhere) {
  
  
  # check args
  assert_that(is.data.frame(adm),
              is.data.frame(adhere))
  
  UseMethod("fail_ltfu", adm)
  
}


#' @inherit fail_ltfu

fail_ltfu.default <- function(adm, adhere) {
  
  message("No object class detected: fail_ltfu() not applied.")
  adm
  
}
