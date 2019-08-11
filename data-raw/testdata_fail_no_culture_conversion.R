# test data for fail_no_culture_conversion()
library(magrittr)

new_data <- function(unique_id_int,
                     ds_or_dr, 
                     base_culture,
                     cc_time,
                     end_tx) {
  
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

# output dataframe
data.frame(APID = id,
                  ds_dr = dst,
                  base_culture = base_culture,
                  STARTTRE = start, 
                  cc_date = cc_dt,
                  DATEN = end_dt,
                  stringsAsFactors = FALSE)
}

records <- list(
  # DR baseline culture negative
  list(1, 1, 0, 50, 650),
  # DR baseline culture positive - cc before 8 months
  list(2, 1, 1, 180, 650),
  # DR baseline culture NA - cc before 8 months
  list(3, 1, NA_integer_, 180, 650),
  # DR baseline culture negative - cc == NA
  list(4, 1, 0, NA_integer_, 650),
  # DR baseline culture positive - cc == NA
  list(5, 1, 1, NA_integer_, 650),
  # DR baseline culture positive - cc 8 months
  list(6, 1, 1, 260, 650),
  # DR baseline culture positive - no end date
  list(7, 1, 1, 150, NA_integer_),
  # DR baseline culture positive - cc after 8 months - no end date
  list(8, 1, 1, 260, NA_integer_), 
  # DR baseline early treatment end date - no cc
  list(9, 1, 1, NA_integer_, 150),
  # DR baseline, no cc and no end date
  list(10, 1, 1, NA_integer_, NA_integer_),
  
  
  # Errors and warnings
  # culture conversion date is after end of treatment date
  list(101, 1, 1, 200, 150)
)

epi <- purrr::map(records, .f = ~ new_data(.x[[1]], .x[[2]], .x[[3]], .x[[4]], .x[[5]])) %>% 
  dplyr::bind_rows()

# apply class
class(epi) <- c("data.frame", "epiinfo")

# save koch6 data
saveRDS(epi, "inst/testdata/fail_no_culture_conversion_epi.rds", version = 2)






