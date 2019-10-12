#' Reassign lab numbers
#' 
#' Check admission data set for diagnostic laboratory sample 
#' identification numbers and link to laboratory data set where
#' this is absent in original data. 
#'
#' @param adm data frame of admission data cleaned with the tbcleanr package
#' @param lab data frame of laboratory data cleaned with the tbcleanr package
#'
#' @return data frame containing lab data with updated patient ID numbers where 
#' they were previously missing 
#' @seealso \code{\link{tbgeneratr}}
#' @export
#'

reassign_lab_no <- function(adm, lab) {
  
  # check input
  assert_that(is.data.frame(adm))
  assert_that(is.data.frame(lab))
  
  # Use Method
  UseMethod("reassign_lab_no", adm)
  
}




#' @inherit reassign_lab_no

reassign_lab_no.default <- function(adm, lab) {
  
  message("No adm object class detected: reassign_lab_no() not applied.")
  lab
  
}
