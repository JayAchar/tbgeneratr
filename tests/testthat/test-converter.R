context("test-converter")
library(tbgeneratr)

# load test data
raw <- system.file("testdata", "culture_conv.rds", package="tbgeneratr") %>% 
          readRDS() 


# epiinfo - corect
correct <- converter(raw, "culture", "epiinfo", "kk", "adm") 

    test_that("culture conversion correct", {
        expect_equal(ncol(correct), 2)
        expect_equal(nrow(correct), length(unique(raw$id)))
        expect_true(all(c("id", "cc_date") %in% names(correct)))
        expect_match(class(correct$cc_date), "Date")
        expect_equal(length(unique(correct$id)), nrow(correct))
        expect_equal(length(unique(raw$id)), length(unique(correct$id)))
        expect_equal(correct$cc_date[correct$id == "a"], as.Date("2017-02-15"))
        expect_equal(correct$cc_date[correct$id == "b"], as.Date("2008-02-21"))
        expect_true(is.na(correct$cc_date[correct$id == "c"]))
        expect_equal(correct$cc_date[correct$id == "d"], as.Date("2008-03-01"))
        expect_equal(correct$cc_date[correct$id == "e"], as.Date("2008-03-01"))
    })

