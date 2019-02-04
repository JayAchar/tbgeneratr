#' Default method for drug_timer()
#'
#' Allow data frames with unspecified object class to pass through
#' @param adm admission data set cleaned using `tbcleanr`
#' @param change change data set cleaned using `tbcleanr`
#' @param drug define which drug to calculate
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbgeneratr}}

drug_timer.default <- function(adm, change, drug) {
  
  message("No adm object class detected: drug_timer() not applied.")
  adm
  
}
