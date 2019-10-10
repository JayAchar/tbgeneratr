library(magrittr)
# function to add new patient to test data

new_patient <- function(unique_id_int,
                        sample_time,
                        culture_results,
                        smear_results) {

  start_date <- lubridate::dmy("1/1/2010")

  assertthat::assert_that(is.numeric(unique_id_int),
                          is.numeric(sample_time),
                          is.numeric(culture_results),
                          is.numeric(smear_results),
                          length(culture_results) == length(smear_results),
                          length(culture_results) == length(sample_time))

  # define sample dates
  sample_dt <- start_date + sample_time

  # gen APID vector
  id <- paste0("XYZ", unique_id_int)
  id_number <- rep(id, length(sample_dt))

  adm <- data.frame(APID = id,
                    STARTTRE = start_date,
                    stringsAsFactors = FALSE)

  lab = data.frame(APID = id_number,
                   samp_date = sample_dt,
                   culture = factor(culture_results,
                                    levels = 0:1,
                                    labels = c("Negative", "Positive"),
                                    ordered = TRUE),
                   smear = factor(smear_results,
                                  levels = 0:4,
                                  labels = c("Negative", "Scanty", "1+", "2+", "3+"),
                                  ordered = TRUE),
                   stringsAsFactors = FALSE)
  list(adm = adm,
       lab = lab)
}

# define each patient
records <- list(
  # use result closest to treatment start
  list(1, c(-7, 2), c(0, 1), c(3, 1)),
  # test baseline days = 90
  list(2, -91, 1, 4),
  # NA result
  list(3, c(-15, -5), c(1, NA_integer_), c(2, NA_integer_)),
  # discrepant results same day
  list(4, c(-5, -5), c(1, 0), c(0, 4)),
  # results after starting treatment + 7
  list(5, 8, 1, 1),
  # same negative results on the same day
  list(6, c(-5, 5), c(1, 1), c(0, 0))
)

epi <- purrr::map(records, .f = ~ new_patient(.x[[1]], .x[[2]], .x[[3]], .x[[4]])) %>%
  purrr::pmap(dplyr::bind_rows)

class(epi$adm) <- c("data.frame", "epiinfo")
class(epi$lab) <- c("data.frame", "epiinfo")

# save epiinfo data
saveRDS(epi, "inst/testdata/baseliner_epi.rds", version = 2)

# Koch 6
# rename variables for koch6 adm and lab
koch6 <- epi
names(koch6$adm) <- c("registrationnb", "Starttre")
names(koch6$lab)[1] <- c("registrationnb")

# change class of Koch 6 data
class(koch6$adm) <- c("data.frame", "koch6")
class(koch6$lab) <- c("data.frame", "koch6")

# save koch6 data
saveRDS(koch6, "inst/testdata/baseliner_k6.rds", version = 2)

# Grozny
# rename variables for koch6 adm and Grozny lab
groz <- epi
names(groz$adm) <- c("registrationnb", "Starttre")
names(groz$lab)[1] <- c("registrationnb")

# add DST number variable
id_numbers <- stringr::str_remove(groz$adm$registrationnb, pattern = "^XYZ")
groz$adm$dstnumber <- stringr::str_pad(id_numbers, width = 5, side = "left", pad = "0")

id_numbers <- stringr::str_remove(groz$lab$registrationnb, pattern = "^XYZ")
groz$lab$dstnumber <- stringr::str_pad(id_numbers, width = 5, side = "left", pad = "0")
groz$lab$registrationnb <- NULL

# change class of Grozny data
class(groz$adm) <- c("data.frame", "koch6")
class(groz$lab) <- c("data.frame", "grozny")


# save grozny data
saveRDS(groz, "inst/testdata/baseliner_groz.rds", version = 2)
