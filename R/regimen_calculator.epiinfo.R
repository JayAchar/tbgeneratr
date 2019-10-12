#' @inherit regimen_calculator
#' @importFrom dplyr mutate %>% select left_join filter bind_rows group_by slice ungroup
#' @importFrom tidyr gather
#' @importFrom purrr modify_at
#' @importFrom rlang .data
#' @importFrom stringr str_extract
#' @export


regimen_calculator.epiinfo <- function(adm,
                                       change, 
                                       days = 0L) {
  # calculate starttreaent + days
  filter_date <- adm %>% 
    dplyr::mutate(filter_date = .data$STARTTRE + days) %>% 
    dplyr::select(.data$APID, .data$filter_date)
  
  # filter change records according to start and end dates in admission data
  change <- change %>% 
    dplyr::left_join(adm[, c("APID", "STARTTRE", "DATEN")], by = "APID") %>% 
    dplyr::filter(.data$change_dt >= .data$STARTTRE & 
                    .data$change_dt <= .data$DATEN) %>% 
    dplyr::select(-.data$STARTTRE, -.data$DATEN)
  
  # select drug variables and start treatment date from admission data
  drug_vars <- unique(drug_look_up$epi_drug)
  adm <- adm[, c("APID", "STARTTRE", drug_vars)]
  
  # function to refactor baseline drug variables
  drug_var_changer <- function(x) {
    factor(as.character(x), levels = c("Yes", "No"),
                labels = c("Start", "Stop"))
  }
  
  # convert all adm baseline drug vars to "Start" or "Stop" factor
  adm <- purrr::modify_at(.x = adm,
                          .at = drug_vars,
                          .f = drug_var_changer)
  
  # change STARTTRE variable name to change_dt
  new_var_names <- unique(drug_look_up$change_drug_name)[drug_look_up$epi_drug %in% drug_vars]
  new_var_names <- new_var_names[!is.na(new_var_names)]
  new_adm_names <- c("APID", "change_dt", new_var_names)
  
  names(adm) <- new_adm_names
  
  # row_bind with change data
  data <- dplyr::bind_rows(adm, change)
  
  # merge filter_dates
  data <- dplyr::left_join(data, filter_date, by = "APID")  
  
  # filter according to args(days)
  data <- data[data$change_dt <= data$filter_date, ]
  data$filter_date <- NULL
  
  # convert to long
  data <- tidyr::gather(data, key = "drug", value = "change", -.data$APID, -.data$change_dt)
  
  # remove rows with no drug change information
  data <- data[!is.na(data$change), ]
  
  # change drug names
  pattern = "^[a-z]*"
  data$drug <- stringr::str_extract(data$drug, pattern = pattern) %>% toupper()
  
  # for each drug look at last available change 
  data <- data %>%
    dplyr::group_by(.data$APID, .data$drug) %>%
    dplyr::filter(.data$change_dt == max(.data$change_dt)) %>%
    dplyr::filter(.data$change == "Start") %>%
    dplyr::group_by(.data$APID) %>%
    dplyr::mutate(regimen = paste(.data$drug, collapse = ", ")) %>%
    dplyr::slice(1) %>% 
    dplyr::ungroup()
  
  # format for output
  out <- data[, c("APID", "regimen")]
  
  # rename output variable
  regimen_var_name <- paste0("regimen_", days, "d")
  
  names(out) <- c("APID", regimen_var_name)
  
  out
  
}



