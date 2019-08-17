# test data for fail_drugs()
library(magrittr)

new_data <- function(unique_id_int,
                     change_time,
                     bdq,
                     lzd,
                     mfx) {
  
  assertthat::assert_that(is.numeric(change_time),
                          is.numeric(bdq),
                          length(change_time) == length(bdq))
  
  # gen APID vector
  id <- paste0("XYZ", unique_id_int)
  
  start_date <- lubridate::dmy("1/1/2010")
  end_date <- lubridate::dmy("31/12/2012")
  
  adm <- data.frame(APID = id,
                    STARTTRE = start_date,
                    DATEN = end_date,
                    stringsAsFactors = FALSE)
  
  
  change <- data.frame(APID = id,
                       change_dt = start_date + change_time,
                       bdq_change = factor(bdq, levels = 1:2, labels = c("Start", "Stop")),
                       lzd_change = factor(lzd, levels = 1:2, labels = c("Start", "Stop")),
                       mfx_change = factor(mfx, levels = 1:2, labels = c("Start", "Stop")),
                       dlm_change = factor(NA_integer_),
                       cfz_change = factor(NA_integer_),
                       stringsAsFactors = FALSE)
  
  list(adm = adm,
       change = change)
}

records <- list(
  # Start - no stop
  list(1, c(20, 51, 100), c(1, NA_integer_, NA_integer_), c(1, NA_integer_, NA_integer_), 
       c(1, NA_integer_, NA_integer_)),
  # Stop 3 drugs after 1 month
  list(2, c(20, 30, 100), c(1, 2, NA_integer_), c(1, 2, NA_integer_), 
       c(1, 2, NA_integer_)),
  # Stop 1 drug after 20 days, then a second after 30 days
  list(3, c(20, 30, 100), c(2, NA_integer_, NA_integer_), c(1, 2, NA_integer_), 
       c(1, NA_integer_, NA_integer_)),
  # Second stop 60 days after first
  list(4, c(20, 80, 100), c(2, NA_integer_, NA_integer_), c(1, 2, NA_integer_), 
       c(1, NA_integer_, NA_integer_)), 
  # Second stop 60 days after first, then third 20 days after second
  list(5, c(20, 80, 100), c(2, NA_integer_, NA_integer_), c(1, 2, NA_integer_), 
       c(1, NA_integer_, 2)), 
  # two groups of two stops within 30 days of each other
  list(6, c(20, 40, 90, 100), c(2, NA_integer_, NA_integer_, 2), c(1, 2, NA_integer_, NA_integer_), 
       c(1, NA_integer_, 2, NA_integer_)), 
  # first double stop on same day, followed by stop x2 on different days
  list(7, c(20, 80, 100), c(2, NA_integer_, NA_integer_), c(2, 2, NA_integer_), 
       c(1, NA_integer_, 2))
)

## construct formula to generate data frame
num <- length(records[[1]])
args <- paste0(".x[[", 1:num, "]]", collapse = ", ")
form <- as.formula(paste0("~ new_data(", args, ")"))

epi <- purrr::map(records, .f = form) %>% 
  purrr::pmap(dplyr::bind_rows)

# apply class
class(epi$adm) <- c("data.frame", "epiinfo")
class(epi$change) <- c("data.frame", "epiinfo")

# save koch6 data
saveRDS(epi, "inst/testdata/fail_drugs_epi.rds", version = 2)

