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
#' @importFrom rlang enquo .data
#' @importFrom dplyr filter distinct %>% bind_rows
#' @return 
#' @export
#'

drug_timer.epiinfo <- function(adm, change, drug) {
  
  # use internal data look up table to find drug variable name (adm)
  adm_drug <- drug_look_up$epi_drug[drug_look_up$drug_string == tolower(drug)]
  
  # use internal data look up table to find drug change variable name (change)
  change_drug <- drug_look_up$change_drug_name[drug_look_up$drug_string == tolower(drug)]

  # convert drug name to quosure
  adm_drug <- sym(adm_drug)
  change_drug <- sym(change_drug)

  # calculate database time
  dbtime <- min(max(adm$STARTTRE, na.rm = T), max(change$change_dt, na.rm = T))
  
  # Find ID numbers for all patients who received drug - start treatment
  interim <- adm %>% 
    filter(!! adm_drug == "Yes") %>% 
    distinct(.data$APID)
  
  # combine all ID numbers from start and change data sets who started drug
  patients <- change %>% 
    filter(!! change_drug == "Start") %>% 
    select(.data$APID) %>% 
    bind_rows(interim) %>% 
    distinct(.data$APID)  
  
  
 
}
