#' Split start treatment variable into year/month/day
#'
#' Takes cleaned admission data frame and splits start treatment date variable
#' into component parts (year, month, day) to allow filtered analyses.
#' @param x data frame containing cleaned TB admission data
#' @param rm_orig remove original variables - TRUE or FALSE
#' @author Jay Achar 
#' @export
#' @importFrom assertthat assert_that
#' @importFrom tidyr separate
#' @importFrom dplyr mutate_at
#' @seealso \code{\link{tbgeneratr}}

start_splittr <- function(x, rm_orig = FALSE) {
  
  # save class attributes
  class_start <- class(x)
  
  # check inputs
  assert_that(is.data.frame(x))
  assert_that(is.logical(rm_orig))
  
  # find starttre variable position in df
    # var name options for EpiInfo and Koch 6
  var <- which(names(x) %in% c("STARTTRE", "Starttre"))
  
  assert_that(length(var) == 1)
  assert_that(class(var) == "integer")  
  
  x <- tidyr::separate(data = x, col = var, sep = "-", 
                    into = c("start_year", "start_month", "start_day"), remove = F) %>% 
      mutate_at(c("start_year", "start_month", "start_day"), as.numeric)
  
  # re-assign class
  class(x) <- class_start
  
  x
}

