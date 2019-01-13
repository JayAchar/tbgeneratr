context("test-drug_baseliner")
library(tbgeneratr)

epi <- system.file("testdata", "drug_baseliner_epi.rds", 
                                package = "tbgeneratr") %>% 
  readRDS()

result <- drug_baseliner(x = epi, drug = dst_p_cm)

test_that("EpiInfo works", {
  expect_equal(ncol(result), 2)
  expect_true(all(c("APID", "base_cm") %in% names(result)))
  expect_equal(class(result$base_cm), "factor")
  expect_equal(length(unique(result$APID)), nrow(result))
  expect_true(result$base_cm[result$APID == "XYZ1"] == "Sensitive")
})
