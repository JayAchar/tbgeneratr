#' Culture/smear conversion calculator
#'
#' Takes laboratory data with start and end treatment informaiton and
#' calculates culture or smear conversion dates based on >30 days criteria
#' @param adm data frame containing TB admission data cleaned and allocated object
#' class by tbcleanr package
#' @param lab data frame containing TB laboratory data cleaned and allocated object
#' class by tbcleanr package
#' @param convert_type string to define which baseline test result to check
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @importFrom assertthat assert_that
#' @importFrom dplyr left_join rename mutate recode arrange filter distinct group_by 
#' top_n ungroup row_number desc lag
#' @importFrom magrittr %>%
#' @seealso \code{\link{tbgeneratr}}

converter.koch6 <- function(adm, lab,
                              convert_type = c("culture", "smear")) {
  
  # check inputs
  assert_that(all(c("registrationnb", "Starttre", "dateend") %in% names(adm)))
  assert_that(class(lab$samp_date) == "Date")
  assert_that(class(adm$Starttre) == "Date")
  assert_that(class(adm$dateend) == "Date")
  assert_that(any(class(lab$culture) == "factor"))
  assert_that(any(class(lab$smear) == "factor"))
  
  # save adm class
  start_class <- class(adm)
  
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
  
  # quote generated id variable
  id_sym <- rlang::sym(id)
  
  # # rename relevent variable
  if (convert_type == "culture") {
    data <- dplyr::rename(data, result = .data$culture)
    
  } else {
    
    data <- dplyr::rename(data, result = .data$smear)
  }
  
  
  # convert smear to binary result
  ## retain levels and ordered status
  if (convert_type == "smear") {
    data <- data %>%
      mutate(result = recode(.data$result, 
                             "Scanty" = "Positive",
                             '1+' = "Positive", 
                             '2+' = "Positive", 
                             '3+' = "Positive")) 
  }
  
  # clean data
  data <- data %>%
    # sort by id, sample date and result
    arrange(!! id_sym, .data$samp_date, .data$result) %>%
    # remove pre-treatment samples
    filter(.data$samp_date > .data$Starttre) %>%
    # remove post end of treatment samples
    filter(.data$samp_date < .data$dateend
           ) %>%
    # remove samples with no result
    filter(!is.na(.data$result)) %>%
    # remove idno, samp_date, and result duplicats
    distinct(!! id_sym, .data$samp_date, .data$result, .keep_all = TRUE) %>%
    # remove negative result when idno and date are duplicated 
    # and positive result present
    group_by(!! id_sym, .data$samp_date) %>%
    top_n(1, .data$result) %>%
    ungroup() %>%
    # generate result sequence variable
    arrange(!! id_sym, .data$samp_date) %>%
    group_by(!! id_sym) %>%
    mutate(seq = row_number())
  

  # calculate days between consecutive negative results
  data <- data %>%
    arrange(!! id_sym, .data$samp_date) %>%
    # find consecutive same results
    mutate(result_grp = cumsum(as.character(.data$result)!=lag(as.character(.data$result),default=""))) %>%
    ungroup() %>%
    group_by(!! id_sym, .data$result_grp) %>%
    # keep all consecutive results which are negative and have total > 30 days
    filter(.data$result == "Negative" & (max(.data$samp_date) - min(.data$samp_date) )>=30) %>%
    ungroup() %>%
    # keep only the first episode of culture conversion
    group_by(!! id_sym) %>%
    filter(.data$result_grp == min(.data$result_grp)) %>%
    top_n(1, desc(.data$seq)) %>%
    ungroup() %>%
    select(!! id_sym, .data$samp_date) 
  
  # rename conversion date variable
  if (convert_type == "smear") {
    data <- rename(data, smc_date = .data$samp_date)
  } else {
    data <- rename(data, cc_date = .data$samp_date)
  }
  

  # merge with admission data frame
  data <- left_join(adm, data, by = "registrationnb")
  
  class(data) <- start_class
  
  data
}

