context("test-baseliner")
library(tbgeneratr)
library(magrittr)

testing_code <- quote({
  expect_equal(class(input$adm), class(output))
  expect_equal(length(unique(input$adm[[1]])), length(unique(output[[1]])))
  expect_true(all(c("base_smear", "base_culture") %in% names(output)))
  expect_true(length(unique(output[[1]])) == nrow(output))
  
  expect_true(output$base_culture[output[[1]] == "XYZ1"] == "Positive")
  expect_true(output$base_smear[output[[1]] == "XYZ1"] == "Scanty")
  
  expect_true(is.na(output$base_culture[output[[1]] == "XYZ2"]))
  expect_true(is.na(output$base_smear[output[[1]] == "XYZ2"]))
  
  expect_true(output_days$base_culture[output_days[[1]] == "XYZ2"] == "Positive")
  expect_true(output_days$base_smear[output_days[[1]] == "XYZ2"] == "3+")
  
  expect_true(output$base_culture[output[[1]] == "XYZ3"] == "Positive")
  expect_true(output$base_smear[output[[1]] == "XYZ3"] == "1+")
  
  expect_true(output$base_culture[output[[1]] == "XYZ4"] == "Positive")
  expect_true(output$base_smear[output[[1]] == "XYZ4"] == "3+")
  
  expect_true(is.na(output$base_culture[output[[1]] == "XYZ5"]))
  expect_true(is.na(output$base_smear[output[[1]] == "XYZ5"]))
  
  
})

# EpiInfo
input <- system.file("testdata", "baseliner_epi.rds",
                     package = "tbgeneratr") %>% 
  readRDS()

output <- baseliner(
  adm = input$adm,
  lab = input$lab,
  baseline_test = "culture",
  baseline_days = 90
) %>%
  baseliner(lab = input$lab,
            baseline_test = "smear",
            baseline_days = 90)

output_days <- baseliner(
  adm = input$adm,
  lab = input$lab,
  baseline_test = "culture",
  baseline_days = 95
) %>%
  baseliner(lab = input$lab,
            baseline_test = "smear",
            baseline_days = 95)

# test code
test_that("EpiInfo testing", eval(testing_code))



# koch 6
input <- system.file("testdata", "baseliner_k6.rds",
                     package = "tbgeneratr") %>% 
  readRDS()

output <- baseliner(
  adm = input$adm,
  lab = input$lab,
  baseline_test = "culture",
  baseline_days = 90
) %>%
  baseliner(lab = input$lab,
            baseline_test = "smear",
            baseline_days = 90)

output_days <- baseliner(
  adm = input$adm,
  lab = input$lab,
  baseline_test = "culture",
  baseline_days = 95
) %>%
  baseliner(lab = input$lab,
            baseline_test = "smear",
            baseline_days = 95)

# test code
test_that("Koch 6 testing", eval(testing_code))



# Grozny
input <- system.file("testdata", "baseliner_groz.rds",
                     package = "tbgeneratr") %>% 
  readRDS()

output <- baseliner(
  adm = input$adm,
  lab = input$lab,
  baseline_test = "culture",
  baseline_days = 90
) %>%
  baseliner(lab = input$lab,
            baseline_test = "smear",
            baseline_days = 90)

output_days <- baseliner(
  adm = input$adm,
  lab = input$lab,
  baseline_test = "culture",
  baseline_days = 95
) %>%
  baseliner(lab = input$lab,
            baseline_test = "smear",
            baseline_days = 95)

# test code
test_that("Grozny testing", eval(testing_code))
