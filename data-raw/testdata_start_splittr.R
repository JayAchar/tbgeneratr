# test data - Koch 6 - start_splittr()
start_splittr_koch6 <- structure(list(registrationnb = c("XYZ1", "XYZ2", "XYZ3", "XYZ4", "XYZ5", "XYZ6", "XYZ7", "XYZ8", "XYZ9", "XYZ10"), 
                                      Starttre = structure(c(17137,17289, 16998, 17208, 17281, 17191, 16230, 17036, 17108, NA), 
                                                           class = "Date"), 
                                      dateend = structure(c(NA, NA, 17270, 17256, NA, NA, 16322, NA, NA, NA), class = "Date")),
                                row.names = c(NA, -10L), 
                                class = c("tbl_df", "tbl", "data.frame", "koch6"))

saveRDS(start_splittr_koch6, "inst/testdata/start_splittr_koch6.rds", version = 2)

# test data - EpiInfo - start_splittr()
start_splittr_epi <- start_splittr_koch6 %>% 
  rename(APID = "registrationnb",
         STARTTRE = "Starttre",
         DATEN = "dateend")
class(start_splittr_epi)[4] <- "epiinfo"

saveRDS(start_splittr_epi, "inst/testdata/start_splittr_epiinfo.rds", version = 2)


