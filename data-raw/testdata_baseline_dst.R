# Generate testdata for dst_baseliner()

## EpiInfo admission and lab
### Admission data
epi_adm <- data.frame(APID = paste0("XYZ", 1:5), stringsAsFactors = F) %>% 
  dplyr::mutate(STARTTRE = lubridate::dmy(c(rep("1/1/10", 5))))

class(epi_adm) <- c(class(epi_adm), "epiinfo")
saveRDS(epi_adm, "inst/testdata/baseline_dst_epi_adm.rds")
                

### Laboratory data
 
epi_lab <- structure(list(APID = c("XYZ1", "XYZ2", "XYZ3", "XYZ4", "XYZ4", "XYZ5"), 
                 hain_inh = structure(c(1L, 2L, 1L, NA, 1L, NA), .Label = c("Sensitive", "Resistant"), class = "factor"), 
                 hain_rif = structure(c(1L, 1L, 1L, 2L, 1L, NA), .Label = c("Sensitive", "Resistant"), class = "factor"), 
                 samp_date = structure(c(14608, 14608, 14608, 14608, NA, 14608), class = "Date"), 
                 xpert_res = structure(c(2L, 2L, 1L, 1L, 1L, NA), .Label = c("Negative","Positive"), class = "factor"), 
                 xpert_rif = structure(c(NA, 1L, 1L, 1L, 1L, NA), .Label = c("Not detected", "Detected"), class = "factor"), 
                 dst_p_rif = structure(c(1L, 1L, 1L, 1L, 1L, NA), .Label = c("Sensitive","Resistant"), class = "factor"), 
                 dst_p_inh = structure(c(1L, 1L, 2L, 1L, 1L, NA), .Label = c("Sensitive", "Resistant"), class = "factor"), 
                 dst_p_pza = structure(c(1L, 1L, 1L, 1L, 1L, NA), .Label = c("Sensitive","Resistant"), class = "factor"), 
                 dst_p_eth = structure(c(1L, 1L, 1L, 1L, 1L, NA), .Label = c("Sensitive", "Resistant"), class = "factor"), 
                 dst_p_cm = structure(c(1L, 1L, 1L, 2L, 1L, NA), .Label = c("Sensitive", "Resistant"), class = "factor"), 
                 dst_p_mfx = structure(c(NA, 1L, 1L, 1L, 2L, NA), .Label = c("Sensitive","Resistant"), class = "factor"), 
                 dst_p_km = structure(c(1L, 2L, 1L, 1L, 1L, NA), .Label = c("Sensitive", "Resistant"), class = "factor"), 
                 dst_p_ofx = structure(c(1L, 1L, 2L, 1L, 1L, NA), .Label = c("Sensitive", "Resistant"), class = "factor")), 
            row.names = c(NA, -6L), class = c("tbl_df", "tbl", "data.frame", "epiinfo"))

saveRDS(epi_lab, "inst/testdata/baseline_dst_epi_lab.rds")

