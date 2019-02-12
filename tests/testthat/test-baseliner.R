context("test-baseliner")
library(tbgeneratr)

# EpiInfo
baseline_epi_adm <- system.file("testdata", "baseline_epi_adm.rds", 
                                package = "tbgeneratr") %>% 
  readRDS()
baseline_epi_lab <- system.file("testdata", "baseline_epi_lab.rds", 
                                package = "tbgeneratr") %>% 
  readRDS()

## apply baseliner.epiinfo
epi <- baseliner(adm = baseline_epi_adm,
                 lab = baseline_epi_lab,
                 baseline_test = "culture",
                 baseline_days = 90) %>% 
       baseliner(lab = baseline_epi_lab,
                 baseline_test = "smear",
                 baseline_days = 90)

test_that("EpiInfo culture works", {
  expect_equal(class(baseline_epi_adm), class(epi))
  expect_equal(length(unique(baseline_epi_adm$APID)), length(unique(epi$APID)))
  expect_true(all(c("base_smear", "base_culture") %in% names(epi)))
})

