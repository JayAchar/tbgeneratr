#' Calculate age
#'
#' Use date of birth and date of starting treatment to calculate 
#' age at baseline. Takes data frame with date of birth and start 
#' of treatment date to generate age variable in years. 
#' @param x data frame containing Koch 6 admission variables 
#' @param rm_orig remove original variables - TRUE or FALSE
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbgeneratr}}
#' @importFrom lubridate years interval
#' @importFrom assertthat assert_that

age_generator.koch6 <- function(x, rm_orig = TRUE) {
  
  # check variables are present
  assert_that(all(c("dateofbirth", "Starttre") %in% names(x)))
  
  # check variables' class
  assert_that(class(x$dateofbirth) == "Date")
  assert_that(class(x$Starttre) == "Date")
  
  # generate age variable
  x$age_years <- interval(x$dateofbirth, x$Starttre) / years(1)
  
  # warn if age < 0
  neg_age <- sum(x$age_years <= 0, na.rm = TRUE)
  
  if (neg_age > 0) {
    neg_age_warn <- paste0(neg_age, " records have age <= 0 years - converted to NA")
    warning(neg_age_warn)
    
    # convert all negative ages to NA
    x$age_years[x$age_years <= 0] <- NA_integer_
  }
  
  # remove original variables
  if (rm_orig) x$dateofbirth <- NULL

  x
}
