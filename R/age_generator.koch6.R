#' Calculate age
#'
#' Use date of birth and date of starting treatment to calculate 
#' age at baseline. Takes data frame with date of birth and start 
#' of treatment date to generate age variable in years. 
#' @param x data frame containing Koch 6 admission variables 
#' @param categorise logical - generate additional factor age variable
#' @param paediatric logical - generate additional paediatric age factor variable
#' @param rm_orig remove original variables - TRUE or FALSE
#' @author Jay Achar 
#' @seealso \code{\link{tbgeneratr}}
#' @importFrom lubridate years interval
#' @importFrom assertthat assert_that
#' @export

age_generator.koch6 <- function(x, 
                                categorise = FALSE,
                                paediatric = FALSE,
                                rm_orig = TRUE) {
  
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
  
  # generate additional factor variable
  if (categorise) {
    
    x$age_cat <- cut(x$age_years,
                     breaks = c(0, 15, 45, max(x$age_years, na.rm = TRUE) + 1),
                     labels = c(1:3)) %>%
      
      factor(levels = c(1:3),
             labels = c("<= 15y",
                        "15 - <= 45y",
                        ">45y"))
      
  }
  
  # generate additional paediatric factor variable
  if (paediatric) {
    
    x$age_paeds <- cut(x$age_years,
                       breaks = c(0, 2, 5, 12, 18),
                       labels = c(1:4)) %>% 
      factor(levels = c(1:4),
             labels = c("<= 2y",
                        "2 - <= 5y",
                        "5 - <= 12y",
                        "12 - <= 18y"))
    
  }
  
  # remove original variables
  if (rm_orig) x$dateofbirth <- NULL

  x
}
