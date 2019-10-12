#' Calculate age from two dates
#'
#' Use date of birth and date of starting treatment to calculate 
#' age at baseline. Takes data frame with date of birth and start 
#' of treatment date to generate age variable in years. 
#' @param x data frame containing TB admission variables 
#' @param categorise logical - generate additional factor age variable
#' @param paediatric logical - generate additional paediatric age factor variable
#' @param rm_orig remove original variables - TRUE or FALSE
#' @author Jay Achar 
#' @seealso \code{\link{tbgeneratr}}
#' @importFrom assertthat assert_that is.flag
#' @export

age_generator <- function(x, 
                          categorise = FALSE, 
                          paediatric = FALSE,
                          rm_orig = TRUE) {

  # check input
  assert_that(is.data.frame(x),
              is.flag(rm_orig),
              is.flag(categorise),
              is.flag(paediatric))
    
  UseMethod("age_generator", x)

}


#' @inherit age_generator

age_generator.default <- function(x,
                                  categorise = FALSE,
                                  paediatric = FALSE,
                                  rm_orig = TRUE) {
  
  message("No adm object class detected: age_generator() not applied.")
  x  
  
}
