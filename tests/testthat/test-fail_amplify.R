context("test-fail_amplify")
library(tbgeneratr)

# define unit tests code
testing_code <- quote({
  expect_true("data.frame" %in% class(output))
  expect_equal(ncol(output), 3)
  expect_equal(nrow(output), length(unique(output[[1]])))
  expect_equal(class(input$adm), class(output))
  expect_equal(class(output$fail_amplify_dt), "Date")
  
  # check dates are generated correctly
  expect_true(all(is.na(output$fail_amplify_dt[output$fail_amplify == 0L])))
  expect_true(all(! is.na(output$fail_amplify_dt[output$fail_amplify == 1L])))
  
  expect_true(output$fail_amplify[output[[1]] == "XYZ1"] == 0L)
  expect_true(output$fail_amplify[output[[1]] == "XYZ2"] == 0L)
  expect_true(output$fail_amplify[output[[1]] == "XYZ3"] == 1L)
  expect_true(output$fail_amplify[output[[1]] == "XYZ4"] == 0L)
  expect_true(output$fail_amplify[output[[1]] == "XYZ5"] == 1L)
})

error_testing <- quote({
  
})

## Epiinfo
# load test data
input <- system.file("testdata", "fail_amplify_epi.rds", package="tbgeneratr") %>% 
  readRDS() 

output <- fail_amplify(adm = input$adm,
                       lab = input$lab)

# test code
test_that("EpiInfo testing", eval(testing_code))
# test_that("EpiInfo errors", eval(error_testing))