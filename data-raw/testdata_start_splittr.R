JA.funs::setwd_test()
list.files()

# test data - Koch 6 - start_splittr()
start_splittr_koch6 <- readr::read_tsv("20170630 Nukus SCR admission_dataV5.txt" ) %>% 
  tbcleanr::adm_data_cleanr() %>% 
  dplyr::select(registrationnb, Starttre, dateend) %>% 
  dplyr::mutate(registrationnb = paste0("XYZ", 1:nrow(.))) %>% 
  slice(1:10)

JA.funs::set.wd("packages/tbgeneratr")
usethis::use_data(start_splittr_koch6)

# test data - EpiInfo - start_splittr()
JA.funs::setwd_test()
list.files()

start_splittr_epi <- readr::read_csv("2018_07_18 KK admission.csv") %>% 
  tbcleanr::adm_data_cleanr() %>% 
  dplyr::select(APID, STARTTRE, DATEN) %>% 
  dplyr::mutate(APID = paste0("XYZ", 1:nrow(.))) %>% 
  slice(1:10)

JA.funs::set.wd("packages/tbgeneratr")
usethis::use_data(start_splittr_epi)

