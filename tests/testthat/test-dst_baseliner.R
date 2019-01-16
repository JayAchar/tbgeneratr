context("test-dst_baseliner")
library(tbgeneratr)

## Epiinfo
# load test data

epi_adm <- system.file("testdata", "dst_baseliner_epi_adm.rds", package="tbgeneratr") %>%
  readRDS()
epi_lab <- system.file("testdata", "dst_baseliner_epi_lab.rds", package="tbgeneratr") %>%
  readRDS()

output <- dst_baseliner(epi_adm, epi_lab)

test_that("EpiInfo works", {
  expect_equal(class(epi_adm), class(output))
  expect_equal(length(unique(epi_adm$APID)), length(unique(output$APID)))
  expect_true("base_rif" %in% names(output))
  # rif_res aggregate variable check
  expect_equal(class(output$base_rif), "factor")
  expect_true(output$base_rif[output$APID == "XYZ1"] == "Sensitive")
  expect_true(output$base_rif[output$APID == "XYZ4"] == "Resistant")
  expect_true(output$base_rif[output$APID == "XYZ5"] %>% is.na())
  # inh_res aggregate variable check
  expect_equal(class(output$base_inh), "factor")
  expect_true(output$base_inh[output$APID == "XYZ1"] == "Sensitive")
  expect_true(output$base_inh[output$APID == "XYZ4"] == "Sensitive")
  expect_true(output$base_inh[output$APID == "XYZ5"] %>% is.na())
  # base_sli aggregate variable check
  expect_equal(class(output$base_sli), "factor")
  expect_true(output$base_sli[output$APID == "XYZ1"] == "Sensitive")
  expect_true(output$base_sli[output$APID == "XYZ4"] == "Resistant")
  expect_true(output$base_sli[output$APID == "XYZ5"] %>% is.na())
  # base_fq aggregate variable check
  expect_equal(class(output$base_fq), "factor")
  expect_true(output$base_fq[output$APID == "XYZ1"] == "Sensitive")
  expect_true(output$base_fq[output$APID == "XYZ4"] == "Sensitive")
  expect_true(output$base_fq[output$APID == "XYZ5"] %>% is.na())
  
  # check final baseline dst
  expect_equivalent(output$base_dst, epi_adm$indictor_dst)

})
