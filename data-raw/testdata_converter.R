# Generate testdata for converter()

## EpiInfo admission and lab
### Admission data
epi_adm <- data.frame(APID = paste0("XYZ", 1:4), stringsAsFactors = F) %>% 
  dplyr::mutate(STARTTRE = lubridate::dmy("1/1/10"),
                DATEN = lubridate::dmy("31/12/10"))

class(epi_adm) <- c(class(epi_adm), "epiinfo")
saveRDS(epi_adm, "inst/testdata/converter_epi_adm.rds")


### Laboratory data
epi_lab <- data.frame(APID = c(rep(paste0("XYZ", 1), 3),
           rep(paste0("XYZ", 2:4), 4)),
           stringsAsFactors = F) %>% 
  dplyr::arrange(APID) %>% 
  dplyr::mutate(samp_date = lubridate::dmy("06/06/2009", "3/1/10", "15/2/10",
                                           "3/1/10", "15/2/10", "18/3/10", "28/4/10",
                                           "15/2/10", "15/2/10", "18/3/10", "28/4/10",
                                           "3/1/10", "15/2/10", "18/3/10", "28/4/10"),
                culture = factor(c("Negative", "Negative", "Negative",
                                   "Negative", "Positive", "Negative", "Negative",
                                   "Negative", "Positive", "Negative", "Negative",
                                   "Positive", NA_character_, "Negative", "Negative"),
                                 ordered = TRUE),
                smear = factor(c("Negative", "Negative", "Negative",
                                 "Negative", "2+", "Negative", "Negative",
                                 "Negative", "1+", "Negative", "Negative",
                                 "3+", NA_character_, "Negative", "Negative"), 
                               levels = c("Negative", "Scanty", 
                                          "1+", "2+", "3+"),
                               ordered = TRUE))


class(epi_lab) <- c(class(epi_lab), "epiinfo")
saveRDS(epi_lab, "inst/testdata/converter_epi_lab.rds")

## Koch6 admission and lab
### Admission
k6_adm <- epi_adm %>% 
  rename(registrationnb = APID,
         Starttre = STARTTRE,
         dateend = DATEN)
class(k6_adm) <- stringr::str_replace(class(k6_adm), "epiinfo", "koch6")
saveRDS(k6_adm, "inst/testdata/converter_k6_adm.rds")

### Laboratory
k6_lab <- epi_lab %>% 
  rename(registrationnb = APID)
class(k6_lab) <- stringr::str_replace(class(k6_adm), "epiinfo", "koch6")
saveRDS(k6_lab, "inst/testdata/converter_k6_lab.rds")

## Grozny lab
### Laboratory