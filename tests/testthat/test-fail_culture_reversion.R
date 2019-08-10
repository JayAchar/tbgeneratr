context("test-drug_timer")
library(tbgeneratr)

# define unit tests code
testing_code <- quote({
  expect_true("data.frame" %in% class(output))
  expect_equal(ncol(output), 3)
  expect_equal(nrow(output), length(unique(output[[1]])))
  expect_equal(class(input$adm), class(output))
  expect_equal(class(output$fail_reversion_dt), "Date")
  
  expect_true(all(is.na(output$fail_reversion_dt[output$fail_reversion == 0L])))
  expect_true(all(! is.na(output$fail_reversion_dt[output$fail_reversion == 1L])))
  
  expect_true(output$fail_reversion[output[[1]] == "XYZ1"] == 0L)
  expect_true(output$fail_reversion[output[[1]] == "XYZ2"] == 0L)
  expect_true(output$fail_reversion[output[[1]] == "XYZ3"] == 0L)
  expect_true(output$fail_reversion[output[[1]] == "XYZ4"] == 1L)
  expect_true(output$fail_reversion[output[[1]] == "XYZ5"] == 0L)
  # when duplicated sample results - keep positive cultures
  expect_true(output$fail_reversion[output[[1]] == "XYZ6"] == 1L)
  expect_true(output$fail_reversion[output[[1]] == "XYZ7"] == 1L)
  expect_true(output$fail_reversion[output[[1]] == "XYZ8"] == 0L)
  expect_true(output$fail_reversion[output[[1]] == "XYZ9"] == 0L)
  expect_true(output$fail_reversion[output[[1]] == "XYZ10"] == 0L)
  expect_true(output$fail_reversion[output[[1]] == "XYZ11"] == 1L)
  expect_true(output$fail_reversion[output[[1]] == "XYZ12"] == 1L)
  expect_equal(output$fail_reversion_dt[output[[1]] == "XYZ12"], input$adm$STARTTRE[1] + 250)
  
})

error_testing <- quote({
 
})

## Epiinfo
# load test data
input <- system.file("testdata", "fail_culture_reversion_epi.rds", package="tbgeneratr") %>% 
  readRDS() 

output <- fail_culture_reversion(adm = input$adm,
                                 lab = input$lab)

# test code
test_that("EpiInfo testing", eval(testing_code))
# test_that("EpiInfo errors", eval(error_testing))