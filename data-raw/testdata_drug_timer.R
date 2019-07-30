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

p1 <- new_patient(1, c(30, 35), c("Start", "Stop"), NA_character_)
p2 <- new_patient(2, 5, "Stop", "Yes")
p3 <- new_patient(3, c(10, 30, 50, 90), rep(c("Start", "Stop"), 2), NA_character_)

epi <- purrr::pmap(.l = list(p1, p2, p3), .f = bind_rows)

# save epiinfo data
saveRDS(epi, "inst/testdata/drug_timer_epi.rds")

# change variable names for Koch 6 raw data
koch <- epi

names(koch$adm) <- c("registrationnb", "Starttre", "Bdq", "dateend")
names(koch$change) <- c("registrationnb", "change_dt", "bdq_change")

# save koch6 data
saveRDS(koch, "inst/testdata/drug_timer_koch.rds")
