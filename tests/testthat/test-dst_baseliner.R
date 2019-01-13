context("test-dst_baseliner")
library(tbgeneratr)

## Epiinfo
# load test data
# 
# epi_adm <- system.file("testdata", "baseline_dst_epi_adm.rds", package="tbgeneratr") %>% 
#   readRDS() 
# epi_lab <- system.file("testdata", "baseline_dst_epi_lab.rds", package="tbgeneratr") %>% 
#   readRDS() 
# 
# test_that("EpiInfo works", {
#   output <- dst_baseliner(epi_adm, epi_lab)
#   
#   expect_equal(class(epi_adm), class(output))
#   expect_equal(length(unique(epi_adm$APID)), length(unique(output$APID)))
#   expect_equal(sum(is.na(output$samp_date)), 0)
#   expect_true("rif_res" %in% names(output))
#   # rif_res aggregate variable check
#   expect_equal(class(output$rif_res), "factor")
#   expect_true(output$rif_res[output$APID == "XYZ1"] == "Sensitive")
#   expect_true(output$rif_res[output$APID == "XYZ4"] == "Resistant")
#   expect_true(output$rif_res[output$APID == "XYZ5"] %>% is.na())
#   # inh_res aggregate variable check
#   expect_equal(class(output$inh_res), "factor")
#   expect_true(output$inh_res[output$APID == "XYZ1"] == "Sensitive")
#   expect_true(output$inh_res[output$APID == "XYZ4"] == "Sensitive")
#   expect_true(output$inh_res[output$APID == "XYZ5"] %>% is.na())
#   # sli_res aggregate variable check
#   expect_equal(class(output$sli_res), "factor")
#   expect_true(output$sli_res[output$APID == "XYZ1"] == "Sensitive")
#   expect_true(output$sli_res[output$APID == "XYZ4"] == "Resistant")
#   expect_true(output$sli_res[output$APID == "XYZ5"] %>% is.na())
#   # fq_res aggregate variable check
#   expect_equal(class(output$fq_res), "factor")
#   expect_true(output$fq_res[output$APID == "XYZ1"] == "Sensitive")
#   expect_true(output$fq_res[output$APID == "XYZ4"] == "Sensitive")
#   expect_true(output$fq_res[output$APID == "XYZ5"] %>% is.na())
#   
# })
