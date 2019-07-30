context("test-drug_timer")
library(tbgeneratr)

## Epiinfo
# load test data
epi <- system.file("testdata", "drug_timer_epi.rds", package="tbgeneratr") %>% 
  readRDS() 

out <- tbgeneratr:::drug_timer.epiinfo(adm = epi$adm,
                                      change = epi$change,
                                      drug = "bdq")

test_that("EpiInfo correct", {
  expect_equal(nrow(out), nrow(epi$adm))
  expect_equal(ncol(out), ncol(epi$adm) + 1)
  expect_true("bdq_days" %in% names(out))
  expect_true(all(out$bdq_days >= 0))
  expect_true(out$bdq_days[out$APID == "XYZ1"] == 5L)
  expect_true(out$bdq_days[out$APID == "XYZ2"] == 5L) 
  expect_true(out$bdq_days[out$APID == "XYZ3"] == 60L)
})


## Koch6
# load test data
k <- system.file("testdata", "drug_timer_koch.rds", package="tbgeneratr") %>% 
  readRDS() 

out <- tbgeneratr:::drug_timer.koch6(adm = k$adm,
                                      change = k$change,
                                      drug = "bdq")

test_that("Koch 6 correct", {
  expect_equal(nrow(out), nrow(k$adm))
  expect_equal(ncol(out), ncol(k$adm) + 1)
  expect_true("bdq_days" %in% names(out))
  expect_true(all(out$bdq_days >= 0))
  expect_true(out$bdq_days[out$registrationnb == "XYZ1"] == 5L)
  expect_true(out$bdq_days[out$registrationnb == "XYZ2"] == 5L) 
  expect_true(out$bdq_days[out$registrationnb == "XYZ3"] == 60L)
})
