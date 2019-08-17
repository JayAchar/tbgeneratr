# test data for regimen_calculator()
library(magrittr)

new_data <- function(unique_id_int,
                     change_time,
                     bdq,
                     lzd,
                     mfx, 
                     start_bdq, 
                     start_lzd,
                     start_mfx) {
  
  assertthat::assert_that(is.numeric(change_time),
                          is.numeric(bdq),
                          length(change_time) == length(bdq))
  
  # gen APID vector
  id <- paste0("XYZ", unique_id_int)
  
  start_date <- lubridate::dmy("1/1/2010")
  end_date <- lubridate::dmy("31/12/2012")
  
  bdq_start <- factor(start_bdq, levels = 1:2, labels = c("No", "Yes"))
  lzd_start <- factor(start_lzd, levels = 1:2, labels = c("No", "Yes"))
  mfx_start <- factor(start_mfx, levels = 1:2, labels = c("No", "Yes"))
  
  adm <- data.frame(APID = id,
                    STARTTRE = start_date,
                    DATEN = end_date,
                    BDQDBDQ = bdq_start,
                    LZDDLZD = lzd_start,
                    MXDMX = mfx_start,
                    DLMDDLM = factor(1L, levels = 1:2, labels = c("No", "Yes")), 
                    IMPDIMP = factor(1L, levels = 1:2, labels = c("No", "Yes")), 
                    HDH = factor(2L, levels = 1:2, labels = c("No", "Yes")), 
                    RDR = factor(2L, levels = 1:2, labels = c("No", "Yes")), 
                    EDE = factor(1L, levels = 1:2, labels = c("No", "Yes")), 
                    ZDZ = factor(1L, levels = 1:2, labels = c("No", "Yes")), 
                    SDS = factor(1L, levels = 1:2, labels = c("No", "Yes")), 
                    KADKA = factor(1L, levels = 1:2, labels = c("No", "Yes")), 
                    OFLDOFL = factor(1L, levels = 1:2, labels = c("No", "Yes")), 
                    CAPDCAP = factor(1L, levels = 1:2, labels = c("No", "Yes")), 
                    ETHDETH = factor(1L, levels = 1:2, labels = c("No", "Yes")), 
                    CYCLDCYCL = factor(1L, levels = 1:2, labels = c("No", "Yes")),
                    AMXDAMX = factor(1L, levels = 1:2, labels = c("No", "Yes")), 
                    PASDPAS = factor(1L, levels = 1:2, labels = c("No", "Yes")), 
                    CLADCLA = factor(1L, levels = 1:2, labels = c("No", "Yes")), 
                    CLODCLO = factor(1L, levels = 1:2, labels = c("No", "Yes")), 
                    LXDLX = factor(1L, levels = 1:2, labels = c("No", "Yes")), 
                    PTDPT = factor(1L, levels = 1:2, labels = c("No", "Yes")), 
                    stringsAsFactors = FALSE)
  
  change <- data.frame(APID = id,
                       change_dt = start_date + change_time,
                       bdq_change = factor(bdq, levels = 1:2, labels = c("Start", "Stop")),
                       lzd_change = factor(lzd, levels = 1:2, labels = c("Start", "Stop")),
                       mfx_change = factor(mfx, levels = 1:2, labels = c("Start", "Stop")),
                       dlm_change = factor(NA_integer_, levels = 1:2, labels = c("Start", "Stop")), 
                       cfz_change = factor(NA_integer_, levels = 1:2, labels = c("Start", "Stop")), 
                       lfx_change = factor(NA_integer_, levels = 1:2, labels = c("Start", "Stop")), 
                       imp_change = factor(NA_integer_, levels = 1:2, labels = c("Start", "Stop")), 
                       inh_change = factor(NA_integer_, levels = 1:2, labels = c("Start", "Stop")), 
                       rif_change = factor(NA_integer_, levels = 1:2, labels = c("Start", "Stop")), 
                       eth_change = factor(NA_integer_, levels = 1:2, labels = c("Start", "Stop")), 
                       pza_change = factor(NA_integer_, levels = 1:2, labels = c("Start", "Stop")), 
                       str_change = factor(NA_integer_, levels = 1:2, labels = c("Start", "Stop")), 
                       kan_change = factor(NA_integer_, levels = 1:2, labels = c("Start", "Stop")), 
                       cap_change = factor(NA_integer_, levels = 1:2, labels = c("Start", "Stop")), 
                       ofx_change = factor(NA_integer_, levels = 1:2, labels = c("Start", "Stop")), 
                       eto_change = factor(NA_integer_, levels = 1:2, labels = c("Start", "Stop")), 
                       cyc_change = factor(NA_integer_, levels = 1:2, labels = c("Start", "Stop")), 
                       pas_change = factor(NA_integer_, levels = 1:2, labels = c("Start", "Stop")), 
                       amx_change = factor(NA_integer_, levels = 1:2, labels = c("Start", "Stop")), 
                       pto_change = factor(NA_integer_, levels = 1:2, labels = c("Start", "Stop")), 
                       stringsAsFactors = FALSE)
  
  list(adm = adm,
       change = change)
}

records <- list(
  # Start - no stop
  list(1, c(20, 50, 100), c(NA_integer_, NA_integer_, NA_integer_), c(NA_integer_, NA_integer_, NA_integer_), 
       c(NA_integer_, NA_integer_, NA_integer_), 2, 2, 2),
  # Stop 3 drugs after 20 days
  list(2, c(20, 50, 100), c(2, NA_integer_, NA_integer_), c(2, NA_integer_, NA_integer_), 
       c(2, NA_integer_, NA_integer_), 2, 2, 2), 
  # stop 3 drugs after 50 days
  list(3, c(20, 50, 100), c(NA_integer_, 2, NA_integer_), c(NA_integer_, 2, NA_integer_), 
       c(NA_integer_, 2, NA_integer_), 2, 2, 2), 
  # bdq not included in baseline regimen
  list(4, c(20, 50, 100), c(NA_integer_, 1, NA_integer_), c(NA_integer_, 1, NA_integer_), 
       c(NA_integer_, 1, NA_integer_), 1, 2, 2),
  # stop bdq at 20 days then restart at 50 days
  list(5, c(20, 50, 100), c(2, 1, NA_integer_), c(2, 1, NA_integer_), 
       c(NA_integer_, 1, 2), 2, 2, 2)
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
saveRDS(epi, "inst/testdata/regimen_calculator_epi.rds", version = 2)

