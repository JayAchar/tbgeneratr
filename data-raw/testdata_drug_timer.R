library(magrittr)
# Function to define patients
new_patient <- function(unique_id_int,
                        change_time,
                        change_instructions,
                        drug_at_start) {
  
  assertthat::assert_that(length(change_time) == length(change_instructions))
  
  start_date <- lubridate::dmy("1/1/2010")
  
# define change dates  
  change_dt <- start_date + change_time
# gen APID vector  
  id <- paste0("XYZ", unique_id_int)
  id_number <- rep(id, length(change_dt))

  change_inst <- factor(change_instructions, levels = c("Start", "Stop"))
  

change <- data.frame(APID = id_number,
                     change_dt = change_dt,
                     bdq_change = change_inst,
                     stringsAsFactors = FALSE)   

adm <- data.frame(APID = id,
                  STARTTRE = start_date,
                  BDQDBDQ = factor(drug_at_start, levels = c("Yes", "No")),
                  DATEN = start_date + 365,
                  stringsAsFactors = FALSE)
list(adm = adm,
     change = change)
}

# define each patient

records <- list(
  # start then stop
  list(1, c(30, 35), c("Start", "Stop"), NA_character_),
  # start in baseline regimen then stop
  list(2, 5, "Stop", "Yes"),
  # start - stop, start - stop
  list(3, c(10, 30, 50, 90), rep(c("Start", "Stop"), 2), NA_character_),
  # start - stop, baseline regimen inclusion "No" check
  list(4, c(30, 35), c("Start", "Stop"), "No"),
  # start at baseline, stop, restart and stop using end of treatment date
  list(5, c(30, 35), c("Stop", "Start"), "Yes"),
  # all missing - replicates data for this particular drug not present in data set
  list(6, c(30, 55), c(NA_character_, NA_character_), "No"),
  # start at baseline, never stop but ID number present in change data set
  list(7, 10, NA_character_, "Yes"),
  # start treatment with pre-treatment start date - should be excluded
  list(8, c(-10, 30, 50), c("Start", "Start", "Stop"), "No"),
  # only stop flag available - should be no result
  list(9, 25, "Stop", NA_character_),
  # stop drug after end of treatment
  list(10, 400, "Stop", "Yes")
)

epi <- purrr::map(records, .f = ~ new_patient(.x[[1]], .x[[2]], .x[[3]], .x[[4]])) %>% 
  purrr::pmap(dplyr::bind_rows)

# add record in admission data with no change data
epi$adm <- dplyr::bind_rows(epi$adm, data.frame(APID = "XYZ0", 
                                         STARTTRE = lubridate::dmy("1/1/2010"),
                                         BDQDBDQ = factor("Yes", levels = c("Yes", "No")), 
                                         DATEN = lubridate::dmy("1/1/2011"), 
                                         stringsAsFactors = FALSE))
# apply class
class(epi$adm) <- c("data.frame", "epiinfo")
class(epi$change) <- c("data.frame", "epiinfo")

# save epiinfo data
saveRDS(epi, "inst/testdata/drug_timer_epi.rds")

# change variable names for Koch 6 raw data
koch <- epi

names(koch$adm) <- c("registrationnb", "Starttre", "Bdq", "dateend")
names(koch$change) <- c("registrationnb", "change_dt", "bdq_change")

# apply class
class(koch$adm) <- c("data.frame", "koch6")
class(koch$change) <- c("data.frame", "koch6")

# save koch6 data
saveRDS(koch, "inst/testdata/drug_timer_koch.rds")
