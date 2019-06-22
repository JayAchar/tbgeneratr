context("test-dst_baseliner")
library(tbgeneratr)

## Epiinfo
# load test data

adm <- system.file("testdata", "dst_baseliner_epi_adm.rds", package="tbgeneratr") %>%
  readRDS()
lab <- system.file("testdata", "dst_baseliner_epi_lab.rds", package="tbgeneratr") %>%
  readRDS()

output <- dst_baseliner(adm, lab)

test_that("EpiInfo works", {
  expect_equal(class(adm), class(output))
  expect_equal(length(unique(adm$APID)), length(unique(output$APID)))
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
  expect_equivalent(output$base_dst, adm$indictor_dst)

})


## Koch 6 with Grozny lab data

adm <- system.file("testdata", "dst_baseliner_k6_groz_adm.rds", package="tbgeneratr") %>%
  readRDS()
lab <- system.file("testdata", "dst_baseliner_k6_groz_lab.rds", package="tbgeneratr") %>%
  readRDS()

output <- dst_baseliner(adm, lab)

test_that("K6 Grozny works", {
  expect_equal(class(adm), class(output))
  expect_equal(length(unique(adm$registrationnb)), length(unique(output$registrationnb)))
  expect_true("base_rif" %in% names(output))

  # rif_res aggregate variable check
  expect_equal(class(output$base_rif), "factor")
  expect_true(output$base_rif[output$registrationnb == "XYZ1"] == "Sensitive")
  expect_true(output$base_rif[output$registrationnb == "XYZ4"] == "Resistant")
  expect_true(output$base_rif[output$registrationnb == "XYZ5"] %>% is.na())
  # inh_res aggregate variable check
  expect_equal(class(output$base_inh), "factor")
  expect_true(output$base_inh[output$registrationnb == "XYZ1"] == "Sensitive")
  expect_true(output$base_inh[output$registrationnb == "XYZ4"] == "Sensitive")
  expect_true(output$base_inh[output$registrationnb == "XYZ5"] %>% is.na())
  # base_sli aggregate variable check
  expect_equal(class(output$base_sli), "factor")
  expect_true(output$base_sli[output$registrationnb == "XYZ1"] == "Sensitive")
  expect_true(output$base_sli[output$registrationnb == "XYZ4"] == "Resistant")
  expect_true(output$base_sli[output$registrationnb == "XYZ5"] %>% is.na())
  # base_fq aggregate variable check
  expect_equal(class(output$base_fq), "factor")
  expect_true(output$base_fq[output$registrationnb == "XYZ1"] == "Sensitive")
  expect_true(output$base_fq[output$registrationnb == "XYZ4"] == "Sensitive")
  expect_true(output$base_fq[output$registrationnb == "XYZ5"] %>% is.na())
  
  # check final baseline DST
  expect_equivalent(output$base_dst, adm$indictor_dst)
})


## Koch 6 with EpiInfo lab data

adm <- system.file("testdata", "dst_baseliner_k6_groz_adm.rds", package="tbgeneratr") %>%
  readRDS() %>% 
  dplyr::mutate(Ofx = factor(2, levels = 1:2, labels = c("No", "Yes")))
class(adm) <- c(class(adm), "koch6")
lab <- system.file("testdata", "dst_baseliner_epi_lab.rds", package="tbgeneratr") %>%
  readRDS()

output <- dst_baseliner(adm, lab)


test_that("K6 EpiInfo works", {
  expect_equal(class(adm), class(output))
  expect_equal(length(unique(adm$registrationnb)), length(unique(output$registrationnb)))
  expect_true("base_rif" %in% names(output))
  
  # rif_res aggregate variable check
  expect_equal(class(output$base_rif), "factor")
  expect_true(output$base_rif[output$registrationnb == "XYZ1"] == "Sensitive")
  expect_true(output$base_rif[output$registrationnb == "XYZ4"] == "Resistant")
  expect_true(output$base_rif[output$registrationnb == "XYZ5"] %>% is.na())
  # inh_res aggregate variable check
  expect_equal(class(output$base_inh), "factor")
  expect_true(output$base_inh[output$registrationnb == "XYZ1"] == "Sensitive")
  expect_true(output$base_inh[output$registrationnb == "XYZ4"] == "Sensitive")
  expect_true(output$base_inh[output$registrationnb == "XYZ5"] %>% is.na())
  # base_sli aggregate variable check
  expect_equal(class(output$base_sli), "factor")
  expect_true(output$base_sli[output$registrationnb == "XYZ1"] == "Sensitive")
  expect_true(output$base_sli[output$registrationnb == "XYZ4"] == "Resistant")
  expect_true(output$base_sli[output$registrationnb == "XYZ5"] %>% is.na())
  # base_fq aggregate variable check
  expect_equal(class(output$base_fq), "factor")
  expect_true(output$base_fq[output$registrationnb == "XYZ1"] == "Sensitive")
  expect_true(output$base_fq[output$registrationnb == "XYZ4"] == "Sensitive")
  expect_true(output$base_fq[output$registrationnb == "XYZ5"] %>% is.na())
  
  # check final baseline DST
  expect_equivalent(output$base_dst, adm$indictor_dst)
})
