#' Baseline culture or smear status
#'
#' This function takes a list, including nested data frames of TB admission and 
#' laboratory programme data, uses a unique patient identifier, dates of 
#' treatment start and sample submission and sample results to calculate baseline 
#' culture or smear status. The function requires admission and laboratory data to 
#' have been prepared with the tbcleanr package and for the admission data to have 
#' class of either "epiinfo" or "koch6"
#' @param adm data frame containing TB admission data cleaned and allocated object
#' class by tbcleanr package
#' @param lab data frame containing TB laboratory data cleaned and allocated object
#' class by tbcleanr package
#' @param baseline_test define which baseline test result to check
#' @param baseline_days criteria for using pre-treatment samples
#' @keywords TB
#' @seealso \code{\link{tbgeneratr}}
#' @importFrom lubridate is.Date
#' @importFrom rlang enquo .data sym
#' @importFrom dplyr filter distinct mutate group_by top_n ungroup select left_join right_join rename
#' @importFrom magrittr %>%
#' @importFrom assertthat assert_that
#' @export


baseliner.koch6 <- function(adm, lab,
                      baseline_test = c("culture", "smear"),
                      baseline_days = 90) {

  # define id variable name depending on lab database
    # merge lab and adm data frames
  if ("grozny" %in% class(lab)) {
    id <- "registrationnb"
    data <- dplyr::left_join(adm, lab, by = "dstnumber")
    
  } else if ("koch6" %in% class(lab)) {
    id <- "registrationnb"
    data <- dplyr::left_join(adm, lab, by = "registrationnb")
  
  } else if ("epiinfo" %in% class(lab)) {
    id <- "registrationnb"
    data <- dplyr::left_join(adm, lab, by = c("registrationnb" = "APID"))
    
  } else {
    warning("Baseliner: Lab object class incorrect.")
    return(adm)
  }
  

  # select relevent variables
  data <- data[ , c(id, "Starttre", "samp_date", "culture", "smear")]
  
  # check data structure
  assert_that(lubridate::is.Date(data$samp_date))
  assert_that(lubridate::is.Date(data$Starttre))
  
  # enquo baseline_days for use with NSE
  baseline_days <- enquo(baseline_days)
  
  id_sym <- rlang::sym(id)

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
    filter(! is.na(!! id_sym)) %>%
    filter(! is.na(.data$result)) %>%
    # remove rows after start treatment + 7 days
    filter(.data$samp_date - .data$Starttre < 7) %>%
    # reoove rows beyond defined baseline_days
    filter(.data$Starttre - .data$samp_date < !! baseline_days) %>% 
    # remove id, sample date and result duplicate
    distinct(!!id_sym, .data$samp_date, .data$result, .keep_all = T) %>% 
    # generate absolute days variable
    mutate(abs_days = abs(.data$samp_date - .data$Starttre)) %>%
    # keep positive results if duplicated for same id and sample date
    # result variable should be orderd factor
    group_by(!! id_sym, .data$abs_days) %>%
    top_n(1, as.numeric(.data$result)) %>%
    ungroup() %>%
    # keep closest result to treatment start
    group_by(!! id_sym) %>%
    top_n(1, desc(.data$abs_days)) %>% 
    ungroup() %>%
    # select output variables
    select(!! id_sym, .data$result) %>%
    # merge with admission data frame
    right_join(adm, by = id)


  # generate final output variable name
  var_name <- paste0("base_", baseline_test)
  names(data)[names(data) == "result"] <- var_name
  
  data
}


