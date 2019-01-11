# Generate testdata for dst_baseliner()

## EpiInfo admission and lab
### Admission data
epi_adm <- data.frame(APID = paste0("XYZ", 1:6), stringsAsFactors = F) %>% 
  dplyr::mutate(STARTTRE = lubridate::dmy(c(rep("1/1/10", 6))),
                indictor_dst = c("DS-TB", "Inh mono", "Inh mono", 
                                 "pre-XDR SLI", "NA", "MDR-TB"))

class(epi_adm) <- c(class(epi_adm), "epiinfo")
saveRDS(epi_adm, "inst/testdata/baseline_dst_epi_adm.rds")
                

### Laboratory data
 
epi_lab <-
  structure(
    list(
      APID = c("XYZ1", "XYZ2", "XYZ3", "XYZ4", "XYZ4",
               "XYZ5", "XYZ6", "XYZ6"),
      MICRLABN = c(
        "09-0001",
        "09-0002",
        "09-0003",
        "09-0004",
        "09-0005",
        "09-0006",
        "09-0007",
        "09-0008"
      ),
      samp_date = structure(c(
        14608,
        14608, 14608, 14608, NA, 14608, 14558, 14608
      ), class = "Date"),
      hain_inh = structure(
        c(1L, 2L, 1L, NA, 1L, NA, NA, NA),
        .Label = c("Sensitive",
                   "Resistant"),
        class = "factor"
      ),
      hain_rif = structure(
        c(1L,
          1L, 1L, 2L, 1L, NA, NA, NA),
        .Label = c("Sensitive", "Resistant"),
        class = "factor"
      ),
      xpert_res = structure(
        c(2L, 2L, 1L,
          1L, 1L, NA, 2L, 2L),
        .Label = c("Negative", "Positive"),
        class = "factor"
      ),
      xpert_rif = structure(
        c(NA, 1L, 1L, 1L, 1L, NA, 1L, 2L),
        .Label = c("Not detected",
                   "Detected"),
        class = "factor"
      ),
      dst_p_rif = structure(
        c(1L,
          1L, 1L, 1L, 1L, NA, NA, NA),
        .Label = c("Sensitive", "Resistant"),
        class = "factor"
      ),
      dst_p_inh = structure(
        c(1L, 1L, 2L,
          1L, 1L, NA, NA, NA),
        .Label = c("Sensitive", "Resistant"),
        class = "factor"
      ),
      dst_p_pza = structure(
        c(1L, 1L, 1L, 1L, 1L, NA, NA, NA),
        .Label = c("Sensitive",
                   "Resistant"),
        class = "factor"
      ),
      dst_p_eth = structure(
        c(1L,
          1L, 1L, 1L, 1L, NA, NA, NA),
        .Label = c("Sensitive", "Resistant"),
        class = "factor"
      ),
      dst_p_cm = structure(
        c(1L, 1L, 1L,
          2L, 1L, NA, NA, NA),
        .Label = c("Sensitive", "Resistant"),
        class = "factor"
      ),
      dst_p_mfx = structure(
        c(NA, 1L, 1L, 1L, 2L, NA, NA, NA),
        .Label = c("Sensitive",
                   "Resistant"),
        class = "factor"
      ),
      dst_p_km = structure(
        c(1L,
          2L, 1L, 1L, 1L, NA, NA, NA),
        .Label = c("Sensitive", "Resistant"),
        class = "factor"
      ),
      dst_p_ofx = structure(
        c(1L, 1L, 2L,
          1L, 1L, NA, NA, NA),
        .Label = c("Sensitive", "Resistant"),
        class = "factor"
      )
    ),
    row.names = c(NA,
                  -8L), class = c("tbl_df", "tbl", "data.frame"))%>% 
  dplyr::mutate(MICRLABN = paste0("09-000", 1:nrow(.))) %>% 
  dplyr::select(APID, MICRLABN, samp_date, dplyr::everything())

class(epi_lab) <- c(class(epi_lab), "epiinfo")
saveRDS(epi_lab, "inst/testdata/baseline_dst_epi_lab.rds")

