# Generate test data for drug_baseliner()

# generate EpiInfo data using dst_p_cm as example
epi <- structure(
  list(
    APID = c("XYZ1", "XYZ1", "XYZ2", "XYZ2", "XYZ3", "XYZ3", "XYZ4", "XYZ4", "XYZ5", "XYZ5", "XYZ5", "XYZ6", "XYZ6"),
    MICRLABN = c(
      "01-00005",
      "01-00006",
      "01-00014",
      "01-00015",
      "01-00017",
      "01-00018",
      "01-00027",
      "01-00028",
      "01-00040",
      "01-00041",
      "01-00042",
      "01-00052",
      "01-00053"
    ),
    baseline_no = c(
      "01-00007",
      "01-00007",
      "01-00018",
      "01-00018",
      "01-00025",
      "01-00025",
      "01-00043",
      "01-00043",
      "01-00048",
      "01-00048",
      "01-00048",
      "01-00054",
      "01-00054"
    ),
    samp_date = structure(c(
      12390, 12422, 12422, 12422, 12421, 12422, 12501, 12501, 12508, 12508, 12508, 12513, 12475
    ), class = "Date"),
    baseline_date = structure(c(
      12422, 12422, 12411, 12411, 12420, 12420, 12502, 12502, 12501, 12501, 12501, 12513, 12513
    ), class = "Date"),
    base_rif = structure(
      c(2L, 2L, 2L, 2L, 2L, 2L, 1L, 1L, 2L, 2L, 2L, 2L, 2L),
      .Label = c("Sensitive", "Resistant"),
      class = "factor"
    ),
    rif_res = structure(
      c(2L, 2L, 1L, 2L, 2L, 2L, 1L, 1L, 2L, 2L, 2L, 2L, 2L),
      .Label = c("Sensitive", "Resistant"),
      class = "factor"
    ),
    inh_res = structure(
      c(2L, 1L, 1L, 2L, NA_integer_, 1L, 2L, 2L, 1L, 2L, 2L, NA_integer_, 2L),
      .Label = c("Sensitive", "Resistant"),
      class = "factor"
    ),
    EMPL = sample(1:5, size = 13, replace = TRUE)
  ),
  row.names = c(NA,-13L),
  class = c("tbl_df", "tbl", "data.frame", "epiinfo")
)

# save test data
saveRDS(epi, "inst/testdata/drug_baseliner_epi.rds")

# correct results  
## "XYZ6" is not included since it has one inh_res result which is NA
correct <- data.frame(APID = c("XYZ1", "XYZ2", "XYZ3", "XYZ4", "XYZ5"),
                      base_inh = structure(
                        c(1L, 2L, 1L, 2L, 2L),
                        .Label = c("Sensitive", "Resistant"),
                        class = "factor"),
                      stringsAsFactors = FALSE)
# save correct results data
saveRDS(correct, "inst/testdata/drug_baseliner_epi_correct.rds")
