context("test-bmi_generator")
library(tbgeneratr)

# Koch 6
system.file("data", "bmi_k6.rda", package = "tbgeneratr") %>% 
  load()

k6 <- bmi_generator(bmi_k6, rm_orig = F)

test_that("Koch 6 works", {
  expect_equal(class(bmi_k6), class(k6))
  expect_equal(nrow(bmi_k6), nrow(k6))
  expect_equal(ncol(bmi_k6) + 1, ncol(k6))
  expect_true("bmi" %in% names(k6))
  expect_true(is.na(k6$bmi[6]))
})


# EpiInfo
system.file("data", "bmi_epi.rda", package = "tbgeneratr") %>% 
  load()

epi <- bmi_generator(bmi_epi, rm_orig = F)

test_that("EpiInfo works", {
  expect_equal(class(bmi_epi), class(epi))
  expect_equal(nrow(bmi_epi), nrow(epi))
  expect_equal(ncol(bmi_epi) + 1, ncol(epi))
  expect_true("bmi" %in% names(epi))
  expect_true(is.na(epi$bmi[6]))
})
