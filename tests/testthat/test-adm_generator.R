context("test-adm_generator")

k6_raw <- system.file("testdata", "adm_generator_koch6.rds",
                      package = "tbgeneratr") %>%
  readRDS()

k6 <- adm_generator(k6_raw)
k6_extended <- adm_generator(k6_raw, categorise = TRUE,
                             paediatric = TRUE)

test_that("koch6", {
  expect_equal(nrow(k6_raw), nrow(k6))
  expect_true(all(c("start_year", "start_month",
                "start_day", "age_years", "bmi") %in% names(k6)))
  expect_true(all(c("age_cat", "age_paeds") %in% names(k6_extended)))
})



epi_raw <- system.file("testdata", "adm_generator_epiinfo.rds",
                      package = "tbgeneratr") %>%
  readRDS()
epi <- adm_generator(epi_raw)
epi_extended <- adm_generator(epi_raw, categorise = TRUE,
                             paediatric = TRUE)

test_that("epiinfo", {
  expect_equal(nrow(epi_raw), nrow(epi))
  expect_true(all(c("start_year", "start_month",
                    "start_day", "age_years", "bmi") %in% names(epi)))
  expect_true(all(c("age_cat", "age_paeds") %in% names(epi_extended)))
})
