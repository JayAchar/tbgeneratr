context("test-finalise_outcome")
library(tbgeneratr)

# test epiinfo
epi <- system.file("testdata", "finalise_outcome_epi.rds", 
                                   package = "tbgeneratr") %>% 
  readRDS()

epi_bin_out <- finalise_outcome(epi, bin_outcome = TRUE)

test_that("epiinfo works correctly", {
  # confirm dimensions
  expect_equal(nrow(epi), nrow(epi_bin_out))
  expect_equal(ncol(epi), ncol(epi_bin_out) - 2L)
  expect_true(all(c("outcome_who", "outcome_bin") %in% names(epi_bin_out)))
  
  # check outcome_who variable
  expect_true(class(epi_bin_out$outcome_who) == "factor")
  expect_true(length(levels(epi_bin_out$outcome_who)) == 6)
  expect_true(sum(is.na(epi_bin_out$outcome_who)) == 2)
  
  # check outcome_bin variable
  expect_true(class(epi_bin_out$outcome_bin) == "factor")
  expect_true(length(levels(epi_bin_out$outcome_bin)) == 3)
  expect_true(sum(is.na(epi_bin_out$outcome_bin)) == 2)
  expect_true(sum(epi_bin_out$outcome_bin == "Not evaluated", na.rm = TRUE) == 1)
  expect_true(sum(epi_bin_out$outcome_bin == "Successful", na.rm = TRUE) == 2)

  # check bin_outcome arg
  expect_equal(ncol(finalise_outcome(epi, bin_outcome = FALSE)),
               ncol(epi_bin_out) - 1)
  })


# test koch6
k6 <- system.file("testdata", "finalise_outcome_k6.rds", 
                   package = "tbgeneratr") %>% 
  readRDS()

k6_bin_out <- finalise_outcome(k6, bin_outcome = TRUE)

test_that("koch6 works correctly", {
  # confirm dimensions
  expect_equal(nrow(k6), nrow(k6_bin_out))
  expect_equal(ncol(k6), ncol(k6_bin_out) - 2L)
  expect_true(all(c("outcome_who", "outcome_bin") %in% names(k6_bin_out)))
  
  # check outcome_who variable
  expect_true(class(k6_bin_out$outcome_who) == "factor")
  expect_true(length(levels(k6_bin_out$outcome_who)) == 6)
  expect_true(sum(is.na(k6_bin_out$outcome_who)) == 3)
  
  # check outcome_bin variable
  expect_true(class(k6_bin_out$outcome_bin) == "factor")
  expect_true(length(levels(k6_bin_out$outcome_bin)) == 3)
  expect_true(sum(is.na(k6_bin_out$outcome_bin)) == 3)
  expect_true(sum(k6_bin_out$outcome_bin == "Not evaluated", na.rm = TRUE) == 1)
  expect_true(sum(k6_bin_out$outcome_bin == "Successful", na.rm = TRUE) == 2)
  
  # check bin_outcome arg
  expect_equal(ncol(finalise_outcome(k6, bin_outcome = FALSE)),
               ncol(k6_bin_out) - 1)
})
