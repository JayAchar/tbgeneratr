# develop test data for reassign_lab_no()

new_patient <- function(APID, 
                        include_APID_in_lab,
                        diag_no,
                        base_no,
                        micro_no, 
                        micro_previous_year = FALSE) {
  
  APID <- paste0("ABC", stringr::str_pad(APID, 4, "left", "0"))
  
  if(!is.na(base_no)) {
    base_no <- paste0("10-", stringr::str_pad(base_no, 5, "left", "0"))  
  }
  
  if(micro_previous_year) {
    micro_no <- paste0("09-", stringr::str_pad(micro_no, 5, "left", "0"))
  } else {
    micro_no <- paste0("10-", stringr::str_pad(micro_no, 5, "left", "0"))
  }
  
  adm <- data.frame(APID = APID,
                    NB1 = as.character(diag_no),
                    NB2 = base_no,
                    stringsAsFactors = FALSE)
  
  if(! include_APID_in_lab) {
    APID <- NA_character_
  }
  
  lab <- data.frame(APID = APID,
                    MICRLABN = micro_no,
                    stringsAsFactors = FALSE)

  list(adm = adm,
       lab = lab)
}

# generate testing records

## Args
    # APID, 
    # include_APID_in_lab,
    # diag_no,
    # base_no,
    # micro_no, 
    # micro_previous_year = FALSE
records <- list(
  list(1, TRUE, 1, 2, 1, FALSE),
  list(2, FALSE, 3, 4, 3, FALSE),
  list(3, FALSE, 5, NA_integer_, 5, FALSE),
  list(4, FALSE, 6, NA_integer_, 6, TRUE),
  list(5, TRUE, "10-00007", NA_integer_, 7, FALSE),
  list(6, FALSE, "10-00008", NA_integer_, 8, FALSE),
  list(7, FALSE, "10-00009", 10, 9, FALSE),
  list(8, FALSE, "L-010", 11, 10, FALSE),
  list(9, FALSE, 13, 14, 13, TRUE),
  list(10, FALSE, NA_character_, 15, 14, FALSE)
)



epi <- purrr::map(records, .f = ~ new_patient(.x[[1]], .x[[2]], .x[[3]], .x[[4]], .x[[5]], .x[[6]])) %>% 
  purrr::pmap(dplyr::bind_rows)

# apply class
class(epi$adm) <- c("data.frame", "epiinfo")
class(epi$lab) <- c("data.frame", "epiinfo")

# save epiinfo data
saveRDS(epi, "inst/testdata/reassign_lab_no_epi.rds")
