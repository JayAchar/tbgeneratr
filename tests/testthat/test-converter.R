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
epi_cult <- converter(epi_adm, epi_lab, "culture")
epi_smear <- converter(epi_adm, epi_lab, "smear")

test_that("EpiInfo correct", {
  expect_equal(class(epi_adm), class(epi_cult))
  expect_equal(ncol(epi_adm) + 1, ncol(epi_cult))
  expect_true("cc_date" %in% names(epi_cult))
  expect_true(lubridate::is.Date(epi_cult$cc_date))
  expect_equal(length(unique(epi_adm$APID)), length(unique(epi_cult$APID)))
  expect_true(all(epi_cult$cc_date > epi_cult$STARTTRE, na.rm = TRUE))
  expect_true(all(epi_cult$cc_date < epi_cult$DATEN, na.rm = TRUE))
  expect_equal(epi_cult$cc_date[epi_cult$APID == "XYZ1"] %>% as.character(), "2010-01-03")
  expect_equal(epi_cult$cc_date[epi_cult$APID == "XYZ2"] %>% as.character(), "2010-03-18")
  expect_equal(epi_cult$cc_date[epi_cult$APID == "XYZ3"] %>% as.character(), "2010-03-18")
  expect_equal(epi_cult$cc_date[epi_cult$APID == "XYZ4"] %>% as.character(), "2012-03-18")
  expect_identical(epi_cult$cc_date, epi_smear$smc_date)
  })


## Koch 6 with EpiInfo lab data
# Load test data
k6_adm <- system.file("testdata", "converter_k6_adm.rds", package="tbgeneratr") %>% 
  readRDS() 
epi_lab <- system.file("testdata", "converter_epi_lab.rds", package="tbgeneratr") %>% 
  readRDS() 

# correct
out_cult <- converter(k6_adm, epi_lab, "culture")
out_smear <- converter(k6_adm, epi_lab, "smear")


test_that("K6 with EpiInfo lab correct", {
  expect_equal(class(k6_adm), class(out_cult))
  expect_equal(ncol(k6_adm) + 1, ncol(out_cult))
  expect_true("cc_date" %in% names(out_cult))
  expect_true(lubridate::is.Date(out_cult$cc_date))
  expect_equal(length(unique(k6_adm$registrationnb)), length(unique(out_cult$registrationnb)))
  expect_true(all(out_cult$cc_date > out_cult$Starttre, na.rm = TRUE))
  expect_true(all(out_cult$cc_date < out_cult$dateend, na.rm = TRUE))
  expect_identical(out_cult$cc_date, out_smear$smc_date)
  expect_identical(out_cult$cc_date, epi_cult$cc_date)
})



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
  expect_identical(out_cult$cc_date, out_smear$smc_date)
  expect_identical(out_cult$cc_date, epi_cult$cc_date)
})

## Koch 6 with Grozny lab data
# Load test data

groz_adm <- system.file("testdata", "converter_groz_adm.rds", package="tbgeneratr") %>% 
  readRDS() 
groz_lab <- system.file("testdata", "converter_groz_lab.rds", package="tbgeneratr") %>% 
  readRDS() 

# correct
out_cult <- converter(groz_adm, groz_lab, "culture")
out_smear <- converter(groz_adm, groz_lab, "smear")

test_that("K6 with Grozny lab correct", {
  expect_equal(class(groz_adm), class(out_cult))
  expect_equal(ncol(groz_adm) + 1, ncol(out_cult))
  expect_true("cc_date" %in% names(out_cult))
  expect_true(lubridate::is.Date(out_cult$cc_date))
  expect_equal(length(unique(groz_adm$registrationnb)), length(unique(out_cult$registrationnb)))
  expect_true(all(out_cult$cc_date > out_cult$Starttre, na.rm = TRUE))
  expect_true(all(out_cult$cc_date < out_cult$dateend, na.rm = TRUE))
  expect_identical(out_cult$cc_date, out_smear$smc_date)
  expect_identical(out_cult$cc_date, epi_cult$cc_date)
})
