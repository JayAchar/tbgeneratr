context("test-age_generator")
library(tbgeneratr)

## Koch 6
age_gen_koch6 <- system.file("testdata", "age_gen_koch6.rds", 
                             package = "tbgeneratr") %>% 
  readRDS()
age_gen_koch6_neg <- system.file("testdata", "age_gen_koch6_neg.rds", 
                                 package = "tbgeneratr") %>% 
  readRDS()

k6 <- age_generator(age_gen_koch6, rm_orig = F)
k6_narrow <- age_generator(age_gen_koch6, rm_orig = TRUE)
k6_neg <- suppressWarnings(age_generator(age_gen_koch6_neg, rm_orig = F))

test_that("Koch 6 wide ok", {
  expect_equal(class(age_gen_koch6), class(k6))
  expect_equal(nrow(age_gen_koch6), nrow(k6))
  expect_equal(ncol(age_gen_koch6) + 1, ncol(k6))
  expect_true("age_years" %in% names(k6))
  expect_true(is.na(k6$age_years[3]))
  expect_equal(class(k6$age_years), "numeric")
})

test_that("Koch 6 narrow ok", {
  expect_equal(class(age_gen_koch6), class(k6_narrow))
  expect_equal(ncol(age_gen_koch6), ncol(k6_narrow))
  expect_true("Starttre" %in% names(k6_narrow))
})

test_that("Negative Koch 6 age ok", {
  expect_true(is.na(k6_neg$age_years[4]))
})



## Epiinfo

age_gen_epi <- system.file("testdata", "age_gen_epi.rds", 
                           package = "tbgeneratr") %>% 
  readRDS()
age_gen_epi_neg <- system.file("testdata", "age_gen_epi_neg.rds", 
                               package = "tbgeneratr") %>% 
  readRDS()

epi <- age_generator(age_gen_epi, rm_orig = FALSE)
epi_narrow <- age_generator(age_gen_epi, rm_orig = T)
epi_neg <- suppressWarnings(age_generator(age_gen_epi_neg))

test_that("EpiInfo wide ok", {
  expect_equal(class(age_gen_epi), class(epi))
  expect_equal(nrow(age_gen_epi), nrow(epi))
  expect_equal(ncol(age_gen_epi) + 1, ncol(epi))
  expect_true("age_years" %in% names(epi))
  expect_true(is.na(epi$age_years[3]))
  expect_equal(class(epi$age_years), "numeric")
})

test_that("EpiInfo narrow ok", {
  expect_equal(class(age_gen_epi), class(epi_narrow))
  expect_equal(ncol(age_gen_epi), ncol(epi_narrow))
  expect_true("STARTTRE" %in% names(epi_narrow))
})

test_that("Negative EpiInfo age ok", {
  expect_true(is.na(epi_neg$age_years[4]))
})


test_that("Expect errors & warnings", {
  expect_error(age_generator(age_gen_epi, rm_orig = stop))
  expect_warning(age_generator(age_gen_koch6_neg))
  expect_warning(age_generator(age_gen_epi_neg))
})


