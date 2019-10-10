#' @inherit reassign_lab_no
#' @importFrom stringr str_pad str_extract
#' @importFrom dplyr lag lead
#' @export
#'

reassign_lab_no.epiinfo <- function(adm, 
                                    lab) {
  
  # generate testing lab no from admission data
  ## for those lab numbers without "-", pad to include "0" to right length with "-"
  adm$id_stem1 <- ifelse(!grepl(pattern = "-", x = adm$NB1),
                   paste0("-", stringr::str_pad(adm$NB1, width = 5, side = "left", pad = "0")),
                   NA_character_)
  
  ## extract year from baseline lab id number
  adm$id_yr <- ifelse(!is.na(adm$NB2),
                      str_extract(adm$NB2, "^[0-9]{2}"),
                      NA_character_)
  
  ## where baseline lab id number == NA, search for year from above first
  ## then below for year from adjacent baseline lab id number
  adm$id_yr <- ifelse(is.na(adm$id_yr),
                      str_extract(dplyr::lag(adm$NB2, 1), "^[0-9]{2}"),
                      adm$id_yr)
  
  adm$id_yr <- ifelse(is.na(adm$id_yr),
                      str_extract(dplyr::lead(adm$NB2, 1), "^[0-9]{2}"),
                      adm$id_yr)
  
  ## generate previous year alternative to id stem
  adm$id_yr_prev <- stringr::str_pad(as.character(as.numeric(adm$id_yr) - 1),
                                     pad = "0", 
                                     width = 2,
                                     side = "left")
  
  ## generate testing lab id numbers - include any original baseline lab
  ## id numbers in lab_id variable
  adm$lab_id <- ifelse((!is.na(adm$id_stem1) & !is.na(adm$id_yr)),
                       paste0(adm$id_yr, adm$id_stem1),
                       adm$NB1)
  
  adm$lab_id_prev <- ifelse((!is.na(adm$id_stem1) & !is.na(adm$id_yr_prev)),
                       paste0(adm$id_yr_prev, adm$id_stem1),
                       NA_character_)
  
  ## remove testing lab id numbers which have the wrong format
  adm$lab_id <- ifelse(str_detect(adm$lab_id, pattern = "^[0-9]{2}-[0-9]{5}"),
                       adm$lab_id,
                       NA_character_)
  
  
  # check if ID number of testing lab no available in lab data
  ## admission lab_id and ID number
  adm_diag <- adm[ , c("APID", "lab_id")]
  
  ## filter lab data to keep only those lab numbers with match in adm
  lab_diag <- dplyr::semi_join(lab, adm_diag, by = c("MICRLABN" = "lab_id"))
  
  ## if lab ID number missing add corresponding ID number from adm data set
  lab$APID <- ifelse(is.na(lab$APID), adm_diag$APID[match(lab$MICRLABN, adm_diag$lab_id)],
                      lab$APID)
  
  ## admission lab_id_prev and ID number
  adm_diag <- adm[ , c("APID", "lab_id_prev")]
  ## filter lab data to keep only those lab numbers with match in adm
  lab_diag <- dplyr::semi_join(lab, adm_diag, by = c("MICRLABN" = "lab_id_prev"))
  
  ## if lab ID number missing add corresponding ID number from adm data set
  lab$APID <- ifelse(is.na(lab$APID), adm_diag$APID[match(lab$MICRLABN, adm_diag$lab_id_prev)],
                     lab$APID)
  
  lab
}
