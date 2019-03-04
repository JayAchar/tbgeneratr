#' Finalise TB treatment outcome
#'
#' Finalise TB outcome variable to align with WHO definitions
#' @param x data frame containing outcome variables
#' @param simplify logical to add binary outcome variable
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar 
#' @importFrom assertthat assert_that
#' @importFrom forcats fct_collapse fct_recode
#' @seealso \code{\link{tbgeneratr}}
#' @export

finalise_outcome.epiinfo <- function(x, 
                             simplify = TRUE,
                             ...) {

  # check that outcome variable has been cleaned
  assert_that(is.factor(x$outcome))
    
  # adjust outcome variable according to WHO definitions
  ## Merge failure levels
  x$final_outcome <- fct_collapse(x$outcome,
                                  Fail = c("Fail", "Fail & amplify")) %>% 
    ## convert inappropriate outcomes to NA
    fct_recode(NULL = "Transfer to Cat 4",
               NULL = "On treatment") 
  
  
  ## collapse factors for success and unsuccessful treatment
  if (simplify) {
    x$binary_outcome <- fct_collapse(x$final_outcome, 
                                     Successful = c("Cured", "Completed"),
                                     Unsuccessful = c("Death", "Fail", "LTFU"),
                                     Not_evaluated = "Transfer out")
  }
  

  x
}