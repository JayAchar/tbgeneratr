# Generate testdata for converter()

## EpiInfo admission and lab
### Admission data
epi_adm <- data.frame(APID = paste0("XYZ", 1:3), stringsAsFactors = F) %>% 
  dplyr::mutate(STARTTRE = lubridate::dmy("1/1/10"),
                DATEN = lubridate::dmy("31/12/10"))

class(epi_adm) <- c(class(epi_adm), "epiinfo")
saveRDS(epi_adm, "inst/testdata/converter_epi_adm.rds")


### Laboratory data
epi_lab <- data.frame(APID = rep(paste0("XYZ", 1:3), 7), stringsAsFactors = F) %>% 
  dplyr::arrange(APID) %>% 
  dplyr::mutate(samp_date = lubridate::dmy(rep(c("1/10/09",
                                                 "1/3/10", 
                                                 "18/3/10", 
                                                 "5/4/10",
                                                 "5/4/10",
                                                 "3/6/10",
                                                 "2/1/11"), 3)),
  culture = factor(c("Positive", "Negative", NA_character_, "Negative", "Negative", "Positive", "Positive",
                      "Negative", "Negative", "Positive", "Negative", "Negative", "Negative", "Positive",
                      "Negative", "Positive", "Negative", "Negative", "Positive", "Negative", "Negative"),
                      levels = c("Negative", "Positive"),
                   ordered = TRUE),
  smear = factor(c("3+", "Negative", NA_character_, "Negative", "Negative", "1+", "2+",
                    "Negative", "Negative", "2+", "Negative", "Negative", "Negative", "Scanty",
                    "Negative", "Scanty", "Negative", "Negative", "1+", "Negative", "Negative"), 
                    levels = c("Negative", "Scanty", 
                               "1+", "2+", "3+"),
                    ordered = TRUE))

class(epi_lab) <- c(class(epi_lab), "epiinfo")
saveRDS(epi_lab, "inst/testdata/converter_epi_lab.rds")
                  


## Koch6 admission and lab
### Admission



### Laboratory




## Grozny lab
### Laboratory