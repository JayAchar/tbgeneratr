#' Baseline culture or smear status
#'
#' This function takes two data frames, including TB admission and 
#' laboratory programme data, uses a unique patient identifier, dates of 
#' treatment start and sample submission and sample results to calculate baseline 
#' culture or smear status. The function requires admission and laboratory data to 
#' have been prepared with the tbcleanr package and for the admission data to have 
#' class of either "epiinfo" or "koch6"
#' @param adm data frame containing TB admission data cleaned and allocated object
#' class by tbcleanr package
#' @param lab data frame containing TB laboratory data cleaned and allocated object
#' class by tbcleanr package
#' @param baseline_test string to define which baseline test result to check
#' @param baseline_days number of days prior to treatment start to define baseline 
#' acceptable period
#' @keywords TB
#' @importFrom lubridate is.Date
#' @importFrom rlang enquo .data sym
#' @importFrom dplyr filter distinct mutate group_by top_n ungroup select left_join right_join rename
#' @importFrom magrittr %>%
#' @importFrom assertthat assert_that
#' @seealso \code{\link{tbgeneratr}}
#' @export

baseliner.epiinfo <- function(adm, lab,
                              baseline_test = c("culture", "smear"),
                              baseline_days = 90) {
  
  # save adm class
  start_class <- class(adm)
  
  # merge adm and lab data 
  data <- dplyr::left_join(adm, lab, by = "APID")
  
  # select relevent variables
  data <- data[ , c("APID", "STARTTRE", "samp_date", "culture", "smear")]
  
  # check data structure
  assertthat::assert_that(lubridate::is.Date(data$samp_date))
  assertthat::assert_that(lubridate::is.Date(data$STARTTRE))
  
  # enquo baseline_days for use with NSE
  baseline_days <- enquo(baseline_days)
  
  # rename relevent variable
  if (baseline_test == "culture") {
    data <- dplyr::rename(data, result = .data$culture)
  
    } else {
    
    data <- dplyr::rename(data, result = .data$smear)
    }
  
  # check result variable class
  assert_that(is.factor(data$result))
  
  # clean dataframe
  data <- data %>%
    # remove rows with missing sample date or id number
    # remove rows with no result
    filter(! is.na(.data$samp_date)) %>%
    filter(! is.na(.data$APID)) %>%
    filter(! is.na(.data$result)) %>%
    # remove rows after start treatment + 7 days
    filter(.data$samp_date - .data$STARTTRE < 7) %>%
    # reoove rows beyond defined baseline_days
    filter(.data$STARTTRE - .data$samp_date < !! baseline_days) %>%
    # generate absolute days variable
    mutate(abs_days = abs(.data$samp_date - .data$STARTTRE)) %>%
    # remove id, sample abs date and result duplicate
    distinct(.data$APID, .data$abs_days, .data$result, .keep_all = T) %>%
    # keep positive results if duplicated for same id and sample date
    # result variable should be orderd factor
    group_by(.data$APID, .data$abs_days) %>%
    top_n(1, as.numeric(.data$result)) %>%
    ungroup() %>%
    # keep closest result to treatment start
    group_by(.data$APID) %>%
    top_n(1, desc(.data$abs_days)) %>%
    ungroup() %>% 
    # select output variables
    select(.data$APID, .data$result) %>% 
    # merge with admission data frame
    right_join(adm, by = "APID")
  
  
  # generate final output variable name
  var_name <- paste0("base_", baseline_test)
  names(data)[names(data) == "result"] <- var_name
  
  # reapply adm class
  class(data) <- start_class
  
  data
  
}
