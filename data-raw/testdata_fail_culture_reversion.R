# test data for fail_culture_reversion()
library(magrittr)

new_data <- function(unique_id_int,
                     ds_or_dr, 
                     base_culture,
                     cc_time,
                     end_tx, 
                     sample_dates, 
                     culture_results) {
  
  assertthat::assert_that(length(sample_dates) == length(culture_results))
  
  start_date <- lubridate::dmy("1/1/2010")
  
  # gen APID vector  
  id <- paste0("XYZ", unique_id_int)
  
  # define if ds_dr 
  dst <- factor(ds_or_dr,
                levels = 0:1,
                labels = c("DS-TB", "DR-TB"))
  
  base_culture <- factor(base_culture,
                         levels = 0:1,
                         labels = c("Negative", "Positive"),
                         ordered = TRUE)
  
  # start treatment date
  start <- start_date
  
  # define recorded culture conversion date
  cc_dt <- start_date + cc_time
  
  # include recorded end treatment date
  end_dt <- start_date + end_tx
  
  # output adm dataframe
  adm <- data.frame(APID = id,
             ds_dr = dst,
             base_culture = base_culture,
             STARTTRE = start, 
             cc_date = cc_dt,
             DATEN = end_dt,
             stringsAsFactors = FALSE)
  
  samp_date <- start_date + sample_dates
  
  culture <- factor(culture_results,
                    levels = 0:1,
                    labels = c("Negative", "Positive"),
                    ordered = TRUE)
  
  id_lab <- rep(id, length(samp_date))
  
  lab <- data.frame(APID = id_lab,
                    samp_date = samp_date,
                    culture = culture, 
                    stringsAsFactors = FALSE)
  
  list(adm = adm, 
       lab = lab)
  
}

records <- list(
  # baseline DST != DR-TB
  list(1, 0, 1, NA_integer_, 180, c(0, 35, 65), c(1, 0, 1)),
  # baseline culture negative
  list(2, 1, 0, NA_integer_, 720, c(0, 35, 65), c(1, 0, 1)),
  # culture converts then no culture conversion before end of treatment
  list(3, 1, 1, 50, 650, c(0, 20, 50, 250, 300), c(1, 0, 0, 0, 0)),
  # culture convert then revert before end of treatment
  list(4, 1, 1, 90, 650, c(0, 20, 50, 250, 300), c(1, 0, 0, 1, 1)),
  # duplicated post conversion samples
  list(5, 1, 1, 90, 650, c(0, 20, 50, 250, 250, 300), c(1, 0, 0, 1, 1, 0)),
  # duplicated sample dates, but different results
  list(6, 1, 1, 90, 650, c(0, 20, 50, 250, 250, 300), c(1, 0, 0, 1, 0, 1)),
  # is.na(culture) between two positive cultures during continuation phase
  list(7, 1, 1, 90, 650, c(0, 20, 50, 250, 270, 300), c(1, 0, 0, 1, NA_integer_, 1)),
  # negative culture between two positive cultures during continuation phase
  list(8, 1, 1, 90, 650, c(0, 20, 50, 250, 270, 300), c(1, 0, 0, 1, 0, 1)),
  # two positive cultures 20 days apart
  list(9, 1, 1, 90, 650, c(0, 20, 50, 250, 270), c(1, 0, 0, 1, 1)),
  # two positive cultures - first positive during the first 8 months
  list(10, 1, 1, 90, 650, c(0, 20, 50, 200, 270), c(1, 0, 0, 1, 1)),
  # two positive cultures - followed by one negative
  list(11, 1, 1, 90, 650, c(0, 20, 50, 250, 290, 300), c(1, 0, 0, 1, 1, 0)),
  # two culture reversions in continuation phase
  list(12, 1, 1, 90, 650, c(0, 20, 50, 250, 290, 300, 320, 355), c(1, 0, 0, 1, 1, 0, 1, 1))
  
  # Errors and warnings
  # culture conversion date is after end of treatment date
  # list(101, 1, 1, 200, 150)
)

epi <- purrr::map(records, .f = ~ new_data(.x[[1]], .x[[2]], .x[[3]], .x[[4]], .x[[5]], .x[[6]], .x[[7]])) %>% 
  purrr::pmap(dplyr::bind_rows)

# apply class
class(epi$adm) <- c("data.frame", "epiinfo")
class(epi$lab) <- c("data.frame", "epiinfo")

# save koch6 data
saveRDS(epi, "inst/testdata/fail_culture_reversion_epi.rds", version = 2)



