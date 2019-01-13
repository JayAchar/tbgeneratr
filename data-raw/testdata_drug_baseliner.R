# Generate test data for drug_baseliner()

# generate EpiInfo data using dst_p_cm as example
epi <- structure(list(APID = "XYZ1", 
               MICRLABN = "01-00005", 
               baseline_no = "01-00007", 
               samp_date = structure(12422, class = "Date"), 
               baseline_date = structure(12422, class = "Date"), 
               base_rif = structure(2L, .Label = c("Sensitive", "Resistant"), class = "factor"), 
               rif_res = structure(2L, .Label = c("Sensitive", "Resistant"), class = "factor"), 
               dst_p_cm = structure(1L, .Label = c("Sensitive", "Resistant"), class = "factor"), 
               EMPL = 4), 
          row.names = c(NA, -1L), 
          class = c("tbl_df", "tbl", "data.frame", "epiinfo"))

# save test data
saveRDS(epi, "inst/testdata/drug_baseliner_epi.rds")

# correct results  
correct <- data.frame(APID = c("XYZ1"),
                      base_cm = c("Sensitive"),
                      stringsAsFactors = FALSE)