library(magrittr)

## EpiInfo test data
### Admission data
baseline_epi_adm <- data.frame(APID = paste0("XYZ", 1:6),
                      STARTTRE = lubridate::dmy("1/1/12"),
                      stringsAsFactors = FALSE)

class(baseline_epi_adm) <- c(class(baseline_epi_adm), "epiinfo")
saveRDS(baseline_epi_adm, "inst/testdata/baseline_epi_adm.rds")

### Lab data
baseline_epi_lab <- data.frame(APID = rep(paste0("XYZ", 1:6), 3), 
                      stringsAsFactors = FALSE) %>% 
              dplyr::arrange(APID) %>% 
              dplyr::mutate(samp_date = lubridate::dmy(rep(c("1/9/11",
                                                       "25/12/11",
                                                       "3/1/12"), 4),
                                                       c("1/9/11",
                                                         "25/12/11",
                                                         "3/4/12",
                                                         "14/12/11",
                                                         "30/12/11",
                                                         "30/12/11")),
                            culture = as.factor(c("Negative", "Positive", "Positive",
                                        "Positive", "Negative", "Negative",
                                        NA_character_, "Positive", "Negative",
                                        "Positive", "Negative", "Positive",
                                        "Negative", "Positive", "Negative",
                                        "Positive", "Negative", "Positive")),
                            smear = as.factor(c("Negative", "2+", "Scanty",
                                              "3+", "Negative", "Scanty",
                                              NA, "3+", "Scanty",
                                              "1+", "2+", "3+", 
                                              "Scanty", "Negative", "3+",
                                              "3+", "Scanty", "2+")))

class(baseline_epi_lab) <- c(class(baseline_epi_lab), "epiinfo")  

saveRDS(baseline_epi_lab, "inst/testdata/baseline_epi_lab.rds")


## Koch 6 test data
### Admission data
baseline_k6_adm <- baseline_epi_adm %>% 
  rename(registrationnb = APID,
         Starttre = STARTTRE)

class(baseline_k6_adm) <- stringr::str_replace(class(baseline_k6_adm), "epiinfo", "koch6")

saveRDS(baseline_k6_adm, "inst/testdata/baseline_k6_adm.rds")


### Lab data
baseline_k6_lab <- baseline_epi_lab %>% 
  rename(registrationnb = APID)


class(baseline_k6_lab) <- stringr::str_replace(class(baseline_k6_lab), "epiinfo", "koch6")

saveRDS(baseline_k6_lab, "inst/testdata/baseline_k6_lab.rds")


## Grozny test data
### Admission data
baseline_groz_adm <- baseline_epi_adm %>% 
  rename(registrationnb = APID,
         Starttre = STARTTRE) %>% 
  mutate(dstnumber = paste0("0000", c(1:5, 1)),
         Starttre = lubridate::dmy(c(rep("1/1/2012", 5), "1/1/2013")))

class(baseline_groz_adm) <- c(class(baseline_groz_adm), "koch6")
saveRDS(baseline_groz_adm, "inst/testdata/baseline_groz_adm.rds")


### Lab data
baseline_groz_lab <- baseline_epi_lab %>% 
  rename(registrationnb = APID) %>% 
  left_join(select(baseline_groz_adm, registrationnb, dstnumber), by = "registrationnb") %>% 
  select(-registrationnb)

  baseline_groz_lab$samp_date[16] <- lubridate::dmy("14/12/2012")
  baseline_groz_lab$samp_date[17] <- lubridate::dmy("30/12/2012")
  baseline_groz_lab$samp_date[18] <- lubridate::dmy("02/01/2013")

class(baseline_groz_lab) <- c(class(baseline_groz_lab), "grozny")

saveRDS(baseline_groz_lab, "inst/testdata/baseline_groz_lab.rds")

