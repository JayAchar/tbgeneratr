context("test-fail_drugs")
library(tbgeneratr)

# define unit tests code
testing_code <- quote({
  expect_true("data.frame" %in% class(output))
  expect_equal(ncol(output), 3)
  expect_equal(nrow(output), length(unique(output[[1]])))
  expect_equal(class(output$fail_drugs_dt), "Date")
  expect_true(all(unique(output$fail_drugs) %in% c(0, 1)))
  
  # check dates are generated correctly
  expect_true(all(is.na(output$fail_drugs_dt[output$fail_drugs == 0L])))
  expect_true(all(! is.na(output$fail_drugs_dt[output$fail_drugs == 1L])))
  
  expect_true(output$fail_drugs[output[[1]] == "XYZ1"] == 0L)
  expect_true(output$fail_drugs[output[[1]] == "XYZ2"] == 1L)
  expect_true(output$fail_drugs[output[[1]] == "XYZ3"] == 1L)
  expect_true(output$fail_drugs[output[[1]] == "XYZ4"] == 0L)
  expect_true(output$fail_drugs[output[[1]] == "XYZ5"] == 1L)
  expect_true(output$fail_drugs_dt[output[[1]] == "XYZ5"] == as.Date("2010-04-11"))
  expect_true(output$fail_drugs[output[[1]] == "XYZ6"] == 1L)
  expect_true(output$fail_drugs_dt[output[[1]] == "XYZ6"] == as.Date("2010-02-10"))
  expect_true(output$fail_drugs[output[[1]] == "XYZ7"] == 1L)
  expect_true(output$fail_drugs_dt[output[[1]] == "XYZ7"] == as.Date("2010-01-21"))
 
})

error_testing <- quote({
  
})

## Epiinfo
# load test data
input <- system.file("testdata", "fail_drugs_epi.rds", package="tbgeneratr") %>% 
  readRDS() 

output <- fail_drugs(adm = input$adm,
                    change = input$change)

# test code
test_that("EpiInfo testing", eval(testing_code))
# test_that("EpiInfo errors", eval(error_testing))
