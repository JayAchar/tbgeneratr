# test data for fail_amplify()
library(magrittr)

new_data <- function(unique_id_int,
                     ds_or_dr,
                     end_tx, 
                     base_fq,
                     base_sli,
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
  fq <- factor(base_fq,
                levels = 0:1,
                labels = c("Sensitive", "Resistant"))
  
  sli <- factor(base_sli,
                levels = 0:1,
                labels = c("Sensitive", "Resistant"))

  # output adm dataframe
  adm <- data.frame(APID = id,
                    ds_dr = dst,
                    STARTTRE = start_date,
                    DATEN = end_dt,
                    base_fq = fq,
                    base_sli = sli,
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
  list(1, 0, 720, 0, 1, 
       c(10, 50), c(0, 0), c(NA_integer_, NA_integer_), c(0, 0), c(0, 0)),
  # no amplification
  list(2, 1, 720, 0, 0, 
       c(10, 50), c(0, 0), c(NA_integer_, NA_integer_), c(0, 0), c(0, 0)),
  # baseline fq resistant - amplify ofx
  list(3, 1, 720, 0, 0, 
       50, 1, NA_integer_, 0, 0), 
  # baseline is.na(fq) - amplify ofx
  list(4, 1, 720, NA_integer_, 0, 
       50, 1, NA_integer_, 0, 0),   
  # baseline fq resistant amplify mfx
  list(5, 1, 720, 1, 0, 
       50, 0, 1, 0, 0),
  # amplify resistance after end of treatment
  list(6, 1, 720, 0, 0, 
       721, 1, NA_integer_, 0, 0),
  # missing baseline DS-DR information
  list(7, NA_integer_, 720, 0, 0, 
       100, 1, NA_integer_, 0, 0),
  # missing sample date
  list(8, 1, 720, 0, 0, 
       NA_integer_, 1, NA_integer_, 0, 0),
  # baseline sli sensitive, amplify km
  list(9, 1, 720, 0, 0, 
       100, 0, 0, 0, 1),
  # amplify twice - select the earlier
  list(10, 1, 720, 0, 0, 
       c(100, 150), c(0, 0), c(0, 0), c(1, 1), c(0, 0))

  
  # Errors and warnings
  # culture conversion date is after end of treatment date
  # list(101, 1, 1, 200, 150)
)

epi <- purrr::map(records, 
                  .f = ~ new_data(.x[[1]], .x[[2]], .x[[3]], 
                                  .x[[4]], .x[[5]], .x[[6]], 
                                  .x[[7]], .x[[8]], .x[[9]],
                                  .x[[10]])) %>% 
  purrr::pmap(dplyr::bind_rows)

# apply class
class(epi$adm) <- c("data.frame", "epiinfo")
class(epi$lab) <- c("data.frame", "epiinfo")

# save koch6 data
saveRDS(epi, "inst/testdata/fail_amplify_epi.rds", version = 2)

