# Generate testdata for converter()

## EpiInfo admission and lab
### Admission data
epi_adm <- data.frame(APID = paste0("XYZ", 1:4), stringsAsFactors = F) %>% 
  dplyr::mutate(STARTTRE = lubridate::dmy(c(rep("1/1/10", 3), "1/1/12")),
                DATEN = lubridate::dmy(c(rep("31/12/10", 3), "31/12/12")))

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
                                           "3/1/12", "15/2/12", "18/3/12", "28/4/12"),
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
### Admission Chechnya
groz_adm <- k6_adm %>% 
  mutate(dstnumber = paste0("0000100", c(1:3, 1)))
class(groz_adm) <- c(class(groz_adm), "koch6")
saveRDS(groz_adm, "inst/testdata/converter_groz_adm.rds")  

### Laboratory
groz_lab <- epi_lab %>% 
  rename(dstnumber = APID) %>% 
  mutate(dstnumber = c(rep("00001001", 3),
                       rep("00001002", 4),
                       rep("00001003", 4),
                       rep("00001001", 4)))
class(groz_lab) <- c(class(groz_lab), "grozny")
saveRDS(groz_lab, "inst/testdata/converter_groz_lab.rds")













