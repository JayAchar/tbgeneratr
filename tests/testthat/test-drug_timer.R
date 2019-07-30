context("test-drug_timer")
library(tbgeneratr)

# define unit tests code
testing_code <- quote({
  expect_true("data.frame" %in% class(output))
  expect_equal(nrow(output), nrow(input$adm))
  expect_equal(ncol(output), ncol(input$adm) + 1)
  expect_true("bdq_days" %in% names(output))
  expect_true(all(output$bdq_days >= 0 | is.na(output$bdq_days)))
  expect_true(output$bdq_days[output[[1]] == "XYZ1"] == 5L)
  expect_true(output$bdq_days[output[[1]] == "XYZ2"] == 5L) 
  expect_true(output$bdq_days[output[[1]] == "XYZ3"] == 60L)
  expect_equal(output$bdq_days[output[[1]] == "XYZ1"], output$bdq_days[output[[1]] == "XYZ4"])
  expect_true(output$bdq_days[output[[1]] == "XYZ5"] == 360L)
  expect_true(output$bdq_days[output[[1]] == "XYZ8"] == 20L)
  expect_true(output$bdq_days[output[[1]] == "XYZ0"] == 365L)
  expect_true(output$bdq_days[output[[1]] == "XYZ10"] == 365L)
})

## Epiinfo
# load test data
input <- system.file("testdata", "drug_timer_epi.rds", package="tbgeneratr") %>% 
  readRDS() 

output <- tbgeneratr:::drug_timer.epiinfo(adm = input$adm,
                                      change = input$change,
                                      drug = "bdq")

# test code
test_that("EpiInfo testing", eval(testing_code))


## Koch6
# load test data
input <- system.file("testdata", "drug_timer_koch.rds", package="tbgeneratr") %>% 
  readRDS() 

output <- tbgeneratr:::drug_timer.koch6(adm = input$adm,
                                      change = input$change,
                                      drug = "bdq")

# test code
test_that("Koch6 testing", eval(testing_code))
