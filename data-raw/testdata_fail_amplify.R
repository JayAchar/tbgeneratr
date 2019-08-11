# test data for fail_amplify()
library(magrittr)

new_data <- function(unique_id_int,
                     ds_or_dr,
                     end_tx, 
                     base_ofx,
                     base_mfx,
                     base_cm,
                     base_km, 
                     sample_date, 
                     dst_ofx, 
                     dst_mfx,
                     dst_cm, 
                     dst_km) {
  
  assertthat::assert_that(length(sample_date) == length(dst_ofx),
                          length(dst_ofx) == length(dst_mfx),
                          length(dst_ofx) == length(dst_cm),
                          length(dst_ofx) == length(dst_km))

  start_date <- lubridate::dmy("1/1/2010")

  # gen APID vector
  id <- paste0("XYZ", unique_id_int)

  # define if ds_dr
  dst <- factor(ds_or_dr,
                levels = 0:1,
                labels = c("DS-TB", "DR-TB"))
 
  # include recorded end treatment date
  end_dt <- start_date + end_tx
  
  # baseline DST results
  ofx <- factor(base_ofx,
                levels = 0:1,
                labels = c("Sensitive", "Resistant"))
  
  mfx <- factor(base_mfx,
                levels = 0:1,
                labels = c("Sensitive", "Resistant"))

  cm <- factor(base_cm,
                levels = 0:1,
                labels = c("Sensitive", "Resistant"))
  
  km <- factor(base_km,
                levels = 0:1,
                labels = c("Sensitive", "Resistant"))
  
  # output adm dataframe
  adm <- data.frame(APID = id,
                    ds_dr = dst,
                    STARTTRE = start_date,
                    DATEN = end_dt,
                    base_ofx = ofx,
                    base_mfx = mfx,
                    base_cm = cm,
                    base_km = km,
                    stringsAsFactors = FALSE)

  samp_date <- start_date + sample_date
  
  dst_ofx <- factor(dst_ofx,
                    levels = 0:1,
                    labels = c("Sensitive", "Resistant"))
  
  dst_mfx <- factor(dst_mfx,
                    levels = 0:1,
                    labels = c("Sensitive", "Resistant"))

  dst_cm <- factor(dst_cm,
                    levels = 0:1,
                    labels = c("Sensitive", "Resistant"))
  
  dst_km <- factor(dst_km,
                    levels = 0:1,
                    labels = c("Sensitive", "Resistant"))
  
  id_lab <- rep(id, length(samp_date))

  lab <- data.frame(APID = id_lab,
                    samp_date = samp_date,
                    dst_p_ofx = dst_ofx,
                    dst_p_mfx = dst_mfx,
                    dst_p_km = dst_km,
                    dst_p_cm = dst_cm,
                    stringsAsFactors = FALSE)

  list(adm = adm,
       lab = lab)
}

records <- list(
  # baseline DST != DR-TB
  list(1, 0, 720, 0, NA_integer_, 1, 0, 
       c(10, 50), c(0, 0), c(NA_integer_, NA_integer_), c(0, 0), c(0, 0)),
  # no amplification
  list(2, 1, 720, 0, NA_integer_, 0, 0, 
       c(10, 50), c(0, 0), c(NA_integer_, NA_integer_), c(0, 0), c(0, 0)),
  # amplify ofx
  list(3, 1, 720, 0, NA_integer_, 0, 0, 
       50, 1, NA_integer_, 0, 0), 
  # is.na(ofx) at baseline to resistant
  list(4, 1, 720, NA_integer_, NA_integer_, 0, 0, 
       50, 1, NA_integer_, 0, 0),   
  # baseline is.na(mfx) to resistant with baseline ofx resistant
  list(5, 1, 720, 1, NA_integer_, 0, 0, 
       50, 0, 1, 0, 0) 
  
  # Errors and warnings
  # culture conversion date is after end of treatment date
  # list(101, 1, 1, 200, 150)
)

epi <- purrr::map(records, 
                  .f = ~ new_data(.x[[1]], .x[[2]], .x[[3]], 
                                  .x[[4]], .x[[5]], .x[[6]], 
                                  .x[[7]], .x[[8]], .x[[9]],
                                  .x[[10]], .x[[11]], .x[[12]])) %>% 
  purrr::pmap(dplyr::bind_rows)

# apply class
class(epi$adm) <- c("data.frame", "epiinfo")
class(epi$lab) <- c("data.frame", "epiinfo")

# save koch6 data
saveRDS(epi, "inst/testdata/fail_amplify_epi.rds")

