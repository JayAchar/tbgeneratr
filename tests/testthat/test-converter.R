context("test-converter")
library(tbgeneratr)

## Correct conversion dates
# XYZ1 = 3/1/10
# XYZ2 = 18/3/10
# XYZ3 = 18/3/10
# XYZ4= 18/3/10

## Epiinfo
# load test data

epi_adm <- system.file("testdata", "converter_epi_adm.rds", package="tbgeneratr") %>% 
  readRDS() 
epi_lab <- system.file("testdata", "converter_epi_lab.rds", package="tbgeneratr") %>% 
  readRDS() 

# epiinfo - corect
output <- converter(epi_adm, epi_lab, "culture")
out_smear <- converter(epi_adm, epi_lab, "smear")

test_that("EpiInfo correct", {
  expect_equal(class(epi_adm), class(output))
  expect_equal(ncol(epi_adm) + 1, ncol(output))
  expect_true("cc_date" %in% names(output))
  expect_true(lubridate::is.Date(output$cc_date))
  expect_equal(length(unique(epi_adm$APID)), length(unique(output$APID)))
  expect_true(all(output$cc_date > output$STARTTRE, na.rm = TRUE))
  expect_true(all(output$cc_date < output$DATEN, na.rm = TRUE))
  expect_equal(output$cc_date[output$APID == "XYZ1"] %>% as.character(), "2010-01-03")
  expect_equal(output$cc_date[output$APID == "XYZ2"] %>% as.character(), "2010-03-18")
  expect_equal(output$cc_date[output$APID == "XYZ3"] %>% as.character(), "2010-03-18")
  expect_equal(output$cc_date[output$APID == "XYZ4"] %>% as.character(), "2010-03-18")
  expect_identical(output$cc_date, out_smear$smc_date)
  })


## Koch 6 with EpiInfo lab data
# Load test data







## Koch 6 with Koch 6 lab data
# Load test data
k6_adm <- system.file("testdata", "converter_k6_adm.rds", package="tbgeneratr") %>% 
  readRDS() 
k6_lab <- system.file("testdata", "converter_k6_lab.rds", package="tbgeneratr") %>% 
  readRDS() 

# correct
out_cult <- converter(k6_adm, k6_lab, "culture")
out_smear <- converter(k6_adm, k6_lab, "smear")

test_that("K6 with K6 lab correct", {
  expect_equal(class(k6_adm), class(out_cult))
  expect_equal(ncol(k6_adm) + 1, ncol(out_cult))
  expect_true("cc_date" %in% names(out_cult))
  expect_true(lubridate::is.Date(out_cult$cc_date))
  expect_equal(length(unique(k6_adm$registrationnb)), length(unique(out_cult$registrationnb)))
  expect_true(all(out_cult$cc_date > out_cult$Starttre, na.rm = TRUE))
  expect_true(all(out_cult$cc_date < out_cult$dateend, na.rm = TRUE))
  expect_equal(out_cult$cc_date[out_cult$registrationnb == "XYZ1"] %>% as.character(), "2010-01-03")
  expect_equal(out_cult$cc_date[out_cult$registrationnb == "XYZ2"] %>% as.character(), "2010-03-18")
  expect_equal(out_cult$cc_date[out_cult$registrationnb == "XYZ3"] %>% as.character(), "2010-03-18")
  expect_equal(out_cult$cc_date[out_cult$registrationnb == "XYZ4"] %>% as.character(), "2010-03-18")
  expect_identical(out_cult$cc_date, out_smear$smc_date)
})

## Koch 6 with Grozny lab data
# Load test data