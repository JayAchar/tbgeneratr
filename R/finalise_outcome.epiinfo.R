#' @inherit finalise_outcome
#' @importFrom assertthat assert_that
#' @importFrom forcats fct_collapse fct_recode
#' @importFrom magrittr %>%
#' @export

finalise_outcome.epiinfo <- function(x,
                                     bin_outcome = TRUE,
                                     ...) {
  

  # check that outcome variable has been cleaned
  assert_that(is.factor(x$outcome))
    
  # adjust outcome variable according to WHO definitions
  ## Merge failure levels
  x$outcome_who <- fct_collapse(x$outcome,
                                  Fail = c("Fail", "Fail & amplify")) %>% 
    ## convert inappropriate outcomes to NA
    fct_recode(NULL = "Transfer to Cat 4",
               NULL = "On treatment",
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
