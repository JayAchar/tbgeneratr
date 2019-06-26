## code to prepare `testdata-finalise_outcome` dataset goes here

# Epiinfo

epi <- data.frame(APID = paste0("XYZ", 1:9),
           outcome = factor(c(
               "On treatment",
               "Cured",
               "Completed",
               "Death",
               "Fail",
               "LTFU",
               "Transfer out",
               "Fail & amplify",
               "Transfer to Cat 4"
             )),
           stringsAsFactors = FALSE)

class(epi) <- c(class(epi), "epiinfo")

saveRDS(epi, "inst/testdata/finalise_outcome_epi.rds")

# Koch 6

k6 <- data.frame(APID = paste0("XYZ", 1:9),
                 outcome = factor(c("On treatment", "Cured", "Completed", "Death", "Fail", "LTFU", 
                                    "Transfer out", "Other", "Transfer back to SCC")),
                 stringsAsFactors = FALSE)

class(k6) <- c(class(k6), "koch6")

saveRDS(k6, "inst/testdata/finalise_outcome_k6.rds")
