#' @title Regimen calculator
#'
#' @description This function will generate a string indicating the treatment regimen at any given 
#' date following initiation based on baseline regimen and drug change data. 
#' 
#' @param adm data frame of individual patient admission records cleaned 
#' with `tbcleanr`.
#' @param change data frame of TB  drug change data cleaned using `tbcleanr`
#' @param days single integer to define the timepoint following treatment initiation 
#' to generate regimen string
#' @author Jay Achar
#' @export 
#' @seealso \code{\link{tbgeneratr}}
#' @importFrom assertthat assert_that

regimen_calculator <- function(adm,
                               change,
                               days = 0L) {

  # check args
  assert_that(is.data.frame(adm),
              is.data.frame(change),
              is.numeric(days))
  
  UseMethod("regimen_calculator", adm)
  
}


#' @inherit regimen_calculator title
#' @inherit regimen_calculator description
#'
#' @inheritParams regimen_calculator
#' @author Jay Achar
#' @seealso \code{\link{tbgeneratr}}

regimen_calculator.default <- function(adm, change, days = 0L) {
  
  message("No object class detected: regimen_calculator() not applied.")
  adm
  
}
