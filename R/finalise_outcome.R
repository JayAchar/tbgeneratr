#' Finalise TB treatment outcome
#'
#' Finalise TB outcome variable to align with WHO definitions
#' 
#' This is a generic function. Methods for EpiInfo and Koch6 data have been defined 
#' within this package. They are designed to standardise outcome factor levels
#' according to \href{https://www.who.int/tb/publications/definitions/en/}{WHO (2013) TB definitions}. 
#' 
#' If \code{bin_outcome} is \code{TRUE}, an additional simplified factor outcome variable is generated with 
#' levels \code{Successful}, \code{Unsuccessful} and \code{Not evaluated}, corresponding to definitions
#' in the WHO document. 
#' 
#' The original data frame with generated variables appended is returned. 
#' 
#' Any original data not represented in the WHO definitions are converted to \code{NA}.
#' 
#' @param x data frame containing outcome variables
#' @param bin_outcome create new binary factor variable simplifying treatment outcome
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar 
#' @importFrom assertthat assert_that is.flag
#' @seealso \code{\link{tbgeneratr}}
#' @export

finalise_outcome <- function(x,
                             bin_outcome = TRUE,
                             ...) {
  
  # check input
  assert_that(is.data.frame(x),
              is.flag(bin_outcome))
  
  # apply correct method
  UseMethod("finalise_outcome", x)
  
}


#' @inherit finalise_outcome

finalise_outcome.default <- function(x,
                                     bin_outcome = TRUE,
                                     ...) {
  
  message("No adm object class detected: finalise_outcome() not applied.")
  x

}
