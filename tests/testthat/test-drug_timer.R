context("test-drug_timer")
library(tbgeneratr)

# define unit tests code
testing_code <- quote({
  expect_true("data.frame" %in% class(output))
  expect_equal(nrow(output), nrow(input$adm))
  expect_equal(ncol(output), ncol(input$adm) + 1)
  expect_true("bdq_days" %in% names(output))
  expect_error(tbgeneratr:::drug_timer(adm = input$adm,
                                              change = input$change,
                                              drug = "bdq"),
                 NA)
  expect_warning(tbgeneratr:::drug_timer(adm = input$adm,
                                       change = input$change,
                                       drug = "bdq"),
               NA)
  expect_message(tbgeneratr:::drug_timer(adm = input$adm,
                                       change = input$change,
                                       drug = "bdq"),
               regexp = "drug_timer(): ",
               all = FALSE, 
               fixed = TRUE)
  expect_message(tbgeneratr:::drug_timer(adm = input$adm,
                                         change = input$change,
                                         drug = "bdq"),
                 regexp = "identified who received this drug.",
                 all = FALSE, 
                 fixed = TRUE)
  expect_true(all(output$bdq_days >= 0 | is.na(output$bdq_days)))
  expect_true(output$bdq_days[output[[1]] == "XYZ1"] == 5L)
  expect_true(output$bdq_days[output[[1]] == "XYZ2"] == 5L) 
  expect_true(output$bdq_days[output[[1]] == "XYZ3"] == 60L)
  expect_equal(output$bdq_days[output[[1]] == "XYZ1"], output$bdq_days[output[[1]] == "XYZ4"])
  expect_true(output$bdq_days[output[[1]] == "XYZ5"] == 360L)
  expect_true(output$bdq_days[output[[1]] == "XYZ8"] == 20L)
  expect_true(output$bdq_days[output[[1]] == "XYZ0"] == 365L)
  expect_true(output$bdq_days[output[[1]] == "XYZ10"] == 365L)
  expect_true(output$bdq_days[output[[1]] == "XYZ11"] == 70L)
  
})

error_testing <- quote({
  # drug argument not present in look up table
  expect_error(tbgeneratr:::drug_timer(adm = input$adm,
                                             change = input$change,
                                             drug = "zzz"))
  # drug arg should be string
  expect_error(tbgeneratr:::drug_timer(adm = input$adm,
                                             change = input$change,
                                             drug = bdq))
})

## Epiinfo
# load test data
input <- system.file("testdata", "drug_timer_epi.rds", package="tbgeneratr") %>% 
  readRDS() 

output <- tbgeneratr:::drug_timer(adm = input$adm,
                                      change = input$change,
                                      drug = "bdq")

# test code
test_that("EpiInfo testing", eval(testing_code))
test_that("EpiInfo errors", eval(error_testing))


## Koch6
# load test data
input <- system.file("testdata", "drug_timer_koch.rds", package="tbgeneratr") %>% 
  readRDS() 

output <- tbgeneratr:::drug_timer(adm = input$adm,
                                      change = input$change,
                                      drug = "bdq")

# test code
test_that("Koch6 testing", eval(testing_code))
test_that("Koch6 errors", eval(error_testing))
