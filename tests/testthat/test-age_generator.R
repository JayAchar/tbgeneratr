context("test-age_generator")
library(tbgeneratr)

## Koch 6
age_gen_koch6 <- system.file("testdata", "age_gen_koch6.rds", 
                             package = "tbgeneratr") %>% 
  readRDS()
age_gen_koch6_neg <- system.file("testdata", "age_gen_koch6_neg.rds", 
                                 package = "tbgeneratr") %>% 
  readRDS()

test_that("Koch 6 wide ok", {
  k6 <- age_generator(age_gen_koch6, rm_orig = F)
  
  expect_equal(class(age_gen_koch6), class(k6))
  expect_equal(nrow(age_gen_koch6), nrow(k6))
  expect_equal(ncol(age_gen_koch6) + 1, ncol(k6))
  expect_true("age_years" %in% names(k6))
  expect_true(is.na(k6$age_years[3]))
  expect_equal(class(k6$age_years), "numeric")
})

test_that("Koch 6 narrow ok", {
  k6_narrow <- age_generator(age_gen_koch6, rm_orig = TRUE)
  
  expect_equal(class(age_gen_koch6), class(k6_narrow))
  expect_equal(ncol(age_gen_koch6), ncol(k6_narrow))
  expect_true("Starttre" %in% names(k6_narrow))
})

test_that("Negative Koch 6 age ok", {
  k6_neg <- suppressWarnings(age_generator(age_gen_koch6_neg, rm_orig = F))
  
  expect_true(is.na(k6_neg$age_years[4]))
})

test_that("Categorise Koch 6 age ok", {
  k6_cat <- age_generator(age_gen_koch6, categorise = TRUE, rm_orig = F)
  
  expect_true("age_cat" %in% names(k6_cat))
  expect_equal(k6_cat$age_years[4], 45)
  expect_equal(as.character(k6_cat$age_cat[4]), "15 - <= 45y")

})

test_that("Paediatrics Koch 6 age ok", {
  k6_paeds <- age_generator(age_gen_koch6, categorise = FALSE, paediatric = TRUE, rm_orig = F)
  
  expect_true("age_paeds" %in% names(k6_paeds))
  expect_equal(k6_paeds$age_years[5], 2)
  expect_equal(as.character(k6_paeds$age_paeds[5]), "<= 2y")
  expect_true(k6_paeds$age_paeds[4] %>% is.na())
  
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

test_that("Categorise Epiinfo age ok", {
  epi_cat <- age_generator(age_gen_epi, categorise = TRUE, rm_orig = F)
  
  expect_true("age_cat" %in% names(epi_cat))
  expect_equal(epi_cat$age_years[4], 45)
  expect_equal(as.character(epi_cat$age_cat[4]), "15 - <= 45y")
  
})


test_that("Paediatrics Epiinfo age ok", {
  epi_paeds <- age_generator(age_gen_epi, categorise = FALSE, paediatric = TRUE, rm_orig = F)
  
  expect_true("age_paeds" %in% names(epi_paeds))
  expect_equal(epi_paeds$age_years[5], 2)
  expect_equal(as.character(epi_paeds$age_paeds[5]), "<= 2y")
  expect_true(epi_paeds$age_paeds[4] %>% is.na())
  
})
