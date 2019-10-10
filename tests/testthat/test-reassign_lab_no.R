context("test-reassign_lab_no")
library(tbgeneratr)
library(magrittr)

## draft testing rules
testing_code <- quote({
  expect_equal(nrow(input$lab), nrow(output))
  expect_equal(ncol(input$lab), ncol(output))
  expect_equal(class(input$lab), class(output))
  expect_true(output$APID[output$MICRLABN == "10-00001"] == "ABC0001")
  expect_true(output$APID[output$MICRLABN == "10-00003"] == "ABC0002")
  expect_true(output$APID[output$MICRLABN == "10-00005"] == "ABC0003")
  expect_true(is.na(output$APID[output$MICRLABN == "09-00006"]))
  expect_true(output$APID[output$MICRLABN == "10-00007"] == "ABC0005")
  expect_true(output$APID[output$MICRLABN == "10-00008"] == "ABC0006")
  expect_true(output$APID[output$MICRLABN == "10-00009"] == "ABC0007")
  expect_true(is.na(output$APID[output$MICRLABN == "10-00010"]))
  expect_true(output$APID[output$MICRLABN == "09-00013"] == "ABC0009")
  expect_true(is.na(output$APID[output$MICRLABN == "10-00014"]))
})

## Import data
# EpiInfo
input <- system.file("testdata", "reassign_lab_no_epi.rds",
                     package = "tbgeneratr") %>% 
  readRDS()

## Execute function
output <- reassign_lab_no(input$adm, 
                          input$lab)

# test code
test_that("EpiInfo testing", eval(testing_code))
