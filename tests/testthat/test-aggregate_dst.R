context("test-aggregate_dst")

raw <- system.file("testdata", "aggregate_dst.rds", package = "tbgeneratr") %>% 
  readRDS()

# Rifampicin output
outrif <- aggregate_dst(raw, drug_class = "rif")
correct_rif <- factor(c(1L, 1L, 1L, 2L, 1L, NA_real_, 1L, 2L),
                          levels = 1:2,
                          labels = c("Sensitive", "Resistant"))
  
test_that("rif works", {
  expect_equal(nrow(raw), nrow(outrif))
  expect_equal(class(outrif), "data.frame")
  expect_true(names(outrif) == "rif_res")
  expect_equal(class(outrif$rif_res), "factor")
  expect_true(all(outrif$rif_res == correct_rif, na.rm = TRUE))
})

# SLI output

outsli <- aggregate_dst(raw, drug_class = "cm|km")

correct_sli <- factor(c(1L, 2L, 1L, 2L, 1L, NA_real_, NA_real_, NA_real_),
                      levels = 1:2,
                      labels = c("Sensitive", "Resistant"))

test_that("sli works", {
  expect_equal(nrow(raw), nrow(outsli))
  expect_equal(class(outsli), "data.frame")
  expect_true(names(outsli) == "sli_res")
  expect_equal(class(outsli$sli_res), "factor")
  expect_true(all(outsli$sli_res == correct_sli, na.rm = TRUE))
})
