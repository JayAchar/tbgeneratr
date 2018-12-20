context("test-converter")
library(tbgeneratr)

## Epiinfo
# load test data

epi_adm <- system.file("testdata", "converter_epi_adm.rds", package="tbgeneratr") %>% 
  readRDS() 
epi_lab <- system.file("testdata", "converter_epi_lab.rds", package="tbgeneratr") %>% 
  readRDS() 

# epiinfo - corect
output <- converter(epi_adm, epi_lab, "culture")

test_that("EpiInfo culture correct", {
  expect_equal(class(epi_adm), class(output))
  expect_equal(ncol(epi_adm) + 1, ncol(output))
  expect_true("cc_date" %in% names(output))
  expect_true(lubridate::is.Date(output$cc_date))
  expect_equal(length(unique(epi_adm$APID)), length(unique(output$APID)))
  expect_true(all(output$cc_date > output$STARTTRE, na.rm = TRUE))
  expect_true(all(output$cc_date < output$DATEN, na.rm = TRUE))
  expect_equal(output$cc_date[output$APID == "XYZ1"], lubridate::ymd("2010/03/01"))
  expect_equal(output$cc_date[output$APID == "XYZ2"], lubridate::ymd("2010/04/05"))
  expect_equal(output$cc_date[output$APID == "XYZ3"], as.Date(NA))
  })






## Koch 6 with EpiInfo lab data
# Load test data







## Koch 6 with Koch 6 lab data
# Load test data




## Koch 6 with Grozny lab data
# Load test data