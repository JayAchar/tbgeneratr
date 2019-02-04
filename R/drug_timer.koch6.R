#' Drug duration calculator
#'
#' This function calculates the number of days a specific drug was prescribed
#' to a patient. It uses information from the admission data set to establish 
#' the baseline treatment regimen, then adds days based on treatment adjustments
#' documented in the change data set. 
#' 
#' @param adm admission data set cleaned using `tbcleanr`
#' @param change change data set cleaned using `tbcleanr`
#' @param drug define which drug to calculate
#' @author \strong{Jay Achar:} \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbgeneratr}}
#' @importFrom rlang enquo .data sym :=
#' @importFrom dplyr filter distinct %>% bind_rows semi_join group_by ungroup left_join case_when n
#' @importFrom tidyr gather
#' @return admission data frame with additional drug days variable added
#' @export
#'

drug_timer.koch6 <- function(adm, change, drug) {
  
  # fix CMD note
  . <- NULL
  
  # use internal data look up table to find drug variable name (adm)
  adm_drug <- drug_look_up$k6_drug[drug_look_up$drug_string == tolower(drug)]
  
  # use internal data look up table to find drug change variable name (change)
  change_drug <- drug_look_up$change_drug_name[drug_look_up$drug_string == tolower(drug)]
  
  # define ideal drug name to add to final output variable
  ideal_name <- drug_look_up$ideal_drug_name[drug_look_up$drug_string == tolower(drug)]
  
  # define final drug_days variable name
  drug_days <- paste0(quo_name(ideal_name), "_days")
  
  # convert drug name to quosure
  adm_drug <- sym(adm_drug)
  change_drug <- sym(change_drug)
  
  # calculate database time
  dbtime <- min(max(adm$Starttre, na.rm = T), max(change$change_dt, na.rm = T))
  
  # Find ID numbers for all patients who received drug - start treatment
  interim <- adm %>% 
    filter(!! adm_drug == "Yes") %>% 
    distinct(.data$registrationnb)
  
  # combine all ID numbers from start and change data sets who started drug
  patients <- change %>% 
    filter(!! change_drug == "Start") %>% 
    select(.data$registrationnb) %>% 
    bind_rows(interim) %>% 
    distinct(.data$registrationnb)
  
  # check all ID numbers in patients are unique
  assert_that(length(unique(patients$registrationnb)) == length(patients$registrationnb))
  
  message(paste0(ideal_name, ": ", nrow(patients), " patients identified who received this drug."))
  
  # filter adm data to only include patients receiving drug
  adm_filtered <- adm %>% 
    # keep only patients IDs who received drug
    semi_join(patients, by = "registrationnb") %>% 
    # keep ID number, start and end treatment time
    select(.data$registrationnb, .data$Starttre, .data$dateend) %>% 
    # wide to long adjustment to prepare for row bind with change df
    gather(key = !! change_drug, value = "change_dt", -.data$registrationnb) %>% 
    # recode adm drug change variable to match change df
    mutate(!! change_drug := ifelse(!! change_drug == "Starttre", 
                                    "Start", "Stop")) %>% 
    mutate(!! change_drug := factor(!! change_drug))
  
  # filter change data to only include patients receiving drug
  change_filtered <- change %>% 
    # keep only patients IDs who received drug
    semi_join(patients, by = "registrationnb") %>% 
    # keep ID number, change date and drug change variable
    select(.data$registrationnb, .data$change_dt, !! change_drug)
  
  # bind rows of adm_filtered and change_filtered for final calculation
  full_long <- dplyr::bind_rows(adm_filtered, change_filtered) %>% 
    # remove rows where other drugs where changed - i.e. no change in selected drug
    filter(! is.na(!! change_drug)) %>% 
    # remove rows where no date available - e.g. where treatment still ongoing
    filter(! is.na(.data$change_dt))
  
  # new var: days between each start and stop
  full_days <- full_long %>% 
    group_by(.data$registrationnb) %>% 
    arrange(.data$registrationnb, .data$change_dt) %>% 
    mutate(n_group = n()) %>% 
    mutate(days_sum = case_when(lag(!! change_drug, 1) == "Start" ~ 
                                  as.numeric(.data$change_dt - lag(.data$change_dt, 1)),
                                # if no start and stop - use dbdate as stop date                                
                                .data$change_dt == last(.data$change_dt) & !! change_drug == "Start" ~ 
                                  as.numeric(dbtime - .data$change_dt),
                                TRUE ~ 0)) %>% 
    # sum days between start and stop
    mutate(!! drug_days := max(cumsum(.data$days_sum))) %>% 
    # subset unique patients
    ungroup() %>% 
    distinct(.data$registrationnb, .keep_all = TRUE) %>% 
    # tidy ready for output
    select(.data$registrationnb, !! drug_days) %>% 
    # merge with admission data
    left_join(adm, ., by = "registrationnb")
  
  # how many patients merged back into admisison data
  merged_number <- sum(!is.na(full_days[[drug_days]]))
  
  message(paste0(ideal_name, ": ", merged_number, " patient records merged back into admission data."))
  
  full_days
}
