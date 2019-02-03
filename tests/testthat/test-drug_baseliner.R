context("test-drug_baseliner")
library(tbgeneratr)

# EpiInfo
epi <- system.file("testdata", "drug_baseliner_epi.rds", 
                                package = "tbgeneratr") %>% 
  readRDS()

correct <- system.file("testdata", "drug_baseliner_epi_correct.rds",
                       package = "tbgeneratr") %>% 
  readRDS()

result <- drug_baseliner(x = epi, drug = "inh_res")

test_that("EpiInfo works", {
  expect_equal(ncol(result), 2)
  expect_true(all(c("APID", "base_inh") %in% names(result)))
  expect_equal(class(result$base_inh), "factor")
  expect_equal(length(unique(result$APID)), nrow(result))
  expect_true(result$base_inh[result$APID == "XYZ1"] == "Sensitive")
  expect_equivalent(correct$base_inh, result$base_inh)
})

  ## adjust days args
result <- drug_baseliner(x = epi, drug = "inh_res", days = 60)

test_that("Days arg works", {
  expect_true(all(c("APID", "base_inh") %in% names(result)))
  expect_equal(length(unique(result$APID)), nrow(result))
  expect_true(result$base_inh[result$APID == "XYZ6"] == "Resistant")
})
