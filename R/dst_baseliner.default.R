#' Default method for dst_baseliner()
#'
#' Allow data frames with unspecified object class to pass through
#' @param adm data frame containing TB admission data cleaned and allocated object
#' class by tbcleanr package
#' @param lab data frame containing TB laboratory data cleaned and allocated object
#' class by tbcleanr package
#' @param dst_time absolute historical limit for including specimens
#' @param dst_days additional criteria for including non-rif results
#' @author Jay Achar 
#' @seealso \code{\link{tbgeneratr}}

dst_baseliner.default <- function(adm, lab, 
                                  dst_time = 90, 
                                  dst_days = 30) {
  
  message("No adm object class detected: dst_baseliner() not applied.")
  adm
  
}
