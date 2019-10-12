#' Culture/smear conversion calculator
#'
#' Takes laboratory data with start and end treatment informaiton and
#' calculates culture or smear conversion dates based on >30 days criteria
#' @param adm data frame containing TB admission data cleaned and allocated object
#' class by tbcleanr package
#' @param lab data frame containing TB laboratory data cleaned and allocated object
#' class by tbcleanr package
#' @param convert_type string to define which baseline test result to check
#' @author Jay Achar 
#' @export
#' @importFrom assertthat assert_that
#' @seealso \code{\link{tbgeneratr}}

converter <- function(adm, lab,
                      convert_type = c("culture", "smear")) {
  # check inputs
  assert_that(is.data.frame(adm))
  assert_that(is.data.frame(lab))
  assert_that(any(c("epiinfo", "grozny", "koch6") %in% class(lab)))
  convert_type <- match.arg(convert_type)
  
  UseMethod("converter", adm)
  
}


#' @inherit converter

converter.default <- function(adm) {
  
  message("No adm object class detected: converter() not applied.")
  adm  
  
}

