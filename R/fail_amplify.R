#' Fail - amplify resistance
#'
#' This function detects whether a record diagnosed with DR-TB
#' should receive a failure outcome due to amplification of resistance. 
#' 
#' @param adm data frame of individual patient admission records cleaned 
#' with `tbcleanr` and combined with laboratory data using \code{tbgeneratr::adm_lab_generator()}. 
#' Must also include baseline drug DST generated data from \code{tbgeneratr::dst_baseliner()}
#' @param lab data frame of laboratory TB data cleaned using `tbcleanr`
#' @author Jay Achar
#' @seealso \code{\link{tbgeneratr}}
#' @importFrom assertthat assert_that

fail_amplify <- function(adm,
                         lab) {
  
  
  # check args
  assert_that(is.data.frame(adm),
              is.data.frame(lab))
  
  UseMethod("fail_amplify", adm)
  
}


#' @inherit fail_amplify

fail_amplify.default <- function(adm, lab) {
  
  message("No object class detected: fail_amplify() not applied.")
  adm
  
}
