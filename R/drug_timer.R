#' Drug duration calculator
#'
#' This function calculates the number of days a specific drug was prescribed
#' to a patient. It uses information from the admission data set to establish 
#' the baseline treatment regimen, then adds days based on treatment adjustments
#' documented in the change data set. 
#' 
#' @param adm admission data set cleaned using `tbcleanr`
#' @param change change data set cleaned using `tbcleanr`
#' @param drug define which drug to calculate
#' @author Jay Achar
#' @seealso \code{\link{tbgeneratr}}
#' @importFrom assertthat assert_that
#' @return admission data frame with additional drug days variable added
#' @export

drug_timer <- function(adm, change, drug) {
  
  # check args
  assert_that(is.data.frame(adm),
              is.data.frame(change))
  assert_that(length(drug) == 1,
              is.character(drug))
  
  UseMethod("drug_timer", adm)
  
}
