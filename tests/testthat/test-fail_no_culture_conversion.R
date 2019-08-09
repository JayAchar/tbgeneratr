context("test-fail_no_culture_conversion")
library(tbgeneratr)

# define unit tests code
testing_code <- quote({
  expect_true("data.frame" %in% class(output))
  expect_equal(nrow(output), nrow(input_safe))
  expect_true(all(c("APID", "fail_no_cc", "fail_no_cc_dt") %in% names(output)))
  
  expect_true(output$fail_no_cc[output[[1]] == "XYZ1"] == 0L)
  expect_true(output$fail_no_cc[output[[1]] == "XYZ2"] == 0L)
  expect_true(output$fail_no_cc[output[[1]] == "XYZ3"] == 0L)
  expect_true(output$fail_no_cc[output[[1]] == "XYZ4"] == 0L)
  expect_true(output$fail_no_cc[output[[1]] == "XYZ5"] == 1L)
  expect_true(output$fail_no_cc[output[[1]] == "XYZ6"] == 1L)
  expect_true(output$fail_no_cc[output[[1]] == "XYZ7"] == 0L)
  expect_true(output$fail_no_cc[output[[1]] == "XYZ8"] == 1L)
  expect_true(output$fail_no_cc[output[[1]] == "XYZ9"] == 0L)
  expect_true(output$fail_no_cc[output[[1]] == "XYZ10"] == 1L)
})

error_testing <- quote({
  expect_error(tbgeneratr:::fail_no_culture_conversion(adm_lab = input_full,
                                                       no_cc_days = "zzz"))
  
  expect_message(tbgeneratr:::fail_no_culture_conversion(adm_lab = input_full,
                                                       no_cc_days = 240))
})

## Epiinfo
# load full test data
input_full <- system.file("testdata", "fail_no_culture_conversion_epi.rds", package="tbgeneratr") %>% 
  readRDS()

# remove records causing warnings/errors
input_safe <- input_full %>% 
  dplyr::filter(as.numeric(stringr::str_extract(.$APID, pattern = "[0-9]+$")) < 100)
class(input_safe) <- c("data.frame", "epiinfo")

# evaluate safe test data
output <- tbgeneratr:::fail_no_culture_conversion(adm_lab = input_safe,
                                                  no_cc_days = 240)

# test code
test_that("EpiInfo testing", eval(testing_code))
test_that("EpiInfo errors", eval(error_testing))