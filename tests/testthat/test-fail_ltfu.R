context("test-fail_ltfu")
library(tbgeneratr)

# define unit tests code
testing_code <- quote({
  expect_true("data.frame" %in% class(output))
  expect_equal(ncol(output), 3)
  expect_equal(nrow(output), length(unique(output[[1]])))
  expect_equal(class(output$fail_ltfu_dt), "Date")
  expect_true(all(unique(output$fail_ltfu) %in% c(0, 1)))
  
  # check dates are generated correctly
  expect_true(all(is.na(output$fail_ltfu_dt[output$fail_ltfu == 0L])))
  expect_true(all(! is.na(output$fail_ltfu_dt[output$fail_ltfu == 1L])))
  
  expect_true(output$fail_ltfu[output[[1]] == "XYZ1"] == 0L)
  expect_true(output$fail_ltfu[output[[1]] == "XYZ2"] == 1L)
  expect_true(output$fail_ltfu_dt[output[[1]] == "XYZ2"] == as.Date("2010-04-01"))
  expect_true(output$fail_ltfu[output[[1]] == "XYZ3"] == 1L)
  expect_true(output$fail_ltfu[output[[1]] == "XYZ4"] == 0L)
  expect_true(output$fail_ltfu[output[[1]] == "XYZ5"] == 0L)
  expect_true(output$fail_ltfu[output[[1]] == "XYZ6"] == 0L)
  expect_true(output$fail_ltfu[output[[1]] == "XYZ7"] == 1L)
  expect_true(output$fail_ltfu_dt[output[[1]] == "XYZ7"] == as.Date("2010-03-01"))
})

error_testing <- quote({
  
})

## Epiinfo
# load test data
input <- system.file("testdata", "fail_ltfu_epi.rds", package="tbgeneratr") %>% 
  readRDS() 

output <- fail_ltfu(adm = input$adm,
                    adhere = input$adhere)

# test code
test_that("EpiInfo testing", eval(testing_code))
# test_that("EpiInfo errors", eval(error_testing))