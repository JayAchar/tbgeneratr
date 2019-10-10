context("test-regimen_calculator")
library(tbgeneratr)

# define unit tests code
testing_code <- quote({
  expect_true("data.frame" %in% class(output))
  expect_equal(ncol(output), 4)
  expect_equal(nrow(output), length(unique(output[[1]])))
  expect_true(is.character(output$regimen_0d))
  
  expect_equal(output$regimen_0d[output$APID == "XYZ1"] ,
               output$regimen_0d[output$APID == "XYZ2"])
  
  expect_equal(output$regimen_0d[output$APID == "XYZ1"] ,
               output$regimen_0d[output$APID == "XYZ3"])
  
  pattern <- c("MFX", "BDQ", "LZD", "RIF", "INH")
  expect_true(all(stringr::str_detect(output[output$APID == "XYZ1", c("regimen_0d")], 
                                      pattern = pattern)))
  expect_true(all(stringr::str_detect(output[output$APID == "XYZ4", c("regimen_90d")], 
                                      pattern = pattern)))
  expect_true(all(stringr::str_detect(output[output$APID == "XYZ5", c("regimen_0d")], 
                                      pattern = pattern)))
  expect_true(all(stringr::str_detect(output[output$APID == "XYZ5", c("regimen_90d")], 
                                      pattern = pattern)))
  
  pattern <- c("MFX", "LZD", "RIF", "INH")
  expect_true(all(stringr::str_detect(output[output$APID == "XYZ4", c("regimen_0d")], 
                                      pattern = pattern)))
  expect_true(all(stringr::str_detect(output[output$APID == "XYZ4", c("regimen_30d")], 
                                      pattern = pattern)))
 
  pattern <- c("RIF", "INH")
  expect_true(all(stringr::str_detect(output[output$APID == "XYZ2", c("regimen_30d")], 
                                      pattern = pattern)))
  expect_true(all(stringr::str_detect(output[output$APID == "XYZ2", c("regimen_90d")], 
                                      pattern = pattern)))
  expect_true(all(stringr::str_detect(output[output$APID == "XYZ3", c("regimen_90d")], 
                                      pattern = pattern)))
  
  pattern <- c("MFX", "RIF", "INH")
  expect_true(all(stringr::str_detect(output[output$APID == "XYZ5", c("regimen_30d")], 
                                      pattern = pattern)))
})

error_testing <- quote({
  
})

## Epiinfo
# load test data
input <- system.file("testdata", "regimen_calculator_epi.rds", package="tbgeneratr") %>% 
  readRDS() 

output <- purrr::map(c(0, 30, 90), 
              .f = ~ regimen_calculator(adm = input$adm, 
                                        change = input$change, 
                                        days = .x)) %>% 
  purrr::reduce(dplyr::left_join, by = "APID")


# test code
test_that("EpiInfo testing", eval(testing_code))
# test_that("EpiInfo errors", eval(error_testing))
