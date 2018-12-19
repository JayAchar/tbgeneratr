context("test-start_splittr")
library(tbgeneratr)

## Koch 6
start_splittr_koch6 <- system.file("testdata", "start_splittr_koch6.rds", 
                                   package = "tbgeneratr") %>% 
  readRDS()

koch6 <- start_splittr(start_splittr_koch6)

test_that("Koch 6 works", {
  expect_equal(nrow(start_splittr_koch6), nrow(koch6))
  expect_equal(ncol(start_splittr_koch6) + 3, ncol(koch6))
  expect_equal(class(start_splittr_koch6), class(koch6))
  expect_true(all(c("start_year", "start_month", "start_day") %in% names(koch6)))
  expect_true(is.numeric(koch6$start_year))
  expect_true(is.na(koch6$start_year[10]))
})



## EpiInfo
start_splittr_epi <- system.file("testdata", "start_splittr_epi.rds", 
                                 package = "tbgeneratr") %>% 
  readRDS()

epi <- start_splittr(start_splittr_epi)

test_that("EpiInfo works", {
  expect_equal(nrow(start_splittr_epi), nrow(epi))
  expect_equal(ncol(start_splittr_epi) + 3, ncol(epi))
  expect_equal(class(start_splittr_epi), class(epi))
  expect_true(all(c("start_year", "start_month", "start_day") %in% names(epi)))
  expect_true(is.numeric(epi$start_year))
  expect_true(is.na(epi$start_year[10]))
})

