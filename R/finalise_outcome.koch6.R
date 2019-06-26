#' Finalise TB treatment outcome
#'
#' Finalise TB outcome variable to align with WHO definitions
#' @inheritParams finalise_outcome
#' @author Jay Achar
#' @importFrom assertthat assert_that
#' @importFrom forcats fct_collapse fct_recode
#' @seealso \code{\link{tbgeneratr}}
#' @export

finalise_outcome.koch6 <- function(x,
                                   bin_outcome = TRUE,
                                   ...) {
  
  # check that outcome variable has been cleaned
  assert_that(is.factor(x$outcome))
  
  # adjust outcome variable according to WHO definitions
  ## convert inappropriate outcomes to NA
  x$outcome_who <- fct_recode(x$outcome, 
                                NULL = "Transfer back to SCC",
                                NULL = "On treatment",
                                NULL = "Other",
                              "Not evaluated" = "Transfer out") 
  
  
  ## collapse factors for success and unsuccessful treatment
  if (bin_outcome) {
    x$outcome_bin <- fct_collapse(x$outcome_who, 
                                     Successful = c("Cured", "Completed"),
                                     Unsuccessful = c("Death", "Fail", "LTFU"),
                                     "Not evaluated" = "Not evaluated")
  }
  
  
  x
}