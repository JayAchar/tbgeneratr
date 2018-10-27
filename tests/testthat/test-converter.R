context("test-converter")
library(tbgeneratr)

# load test data
raw <- system.file("testdata", "culture_conv.rds", package="tbgeneratr") %>% 
    readRDS() 


# epiinfo - corect
correct <- converter(raw, "culture", "epiinfo", "kk", "adm") 

    test_that("culture conversion dates correct", {
        expect_equal(ncol(correct), 2)
        expect_equal(nrow(correct), 3)
        expect_true(all(c("id", "cc_date") %in% names(correct)))
        expect_match(class(correct$cc_date), "Date")
        expect_equal(length(unique(correct$id)), nrow(correct))
        expect_equal(length(unique(raw$id)), length(unique(correct$id)))
        expect_equal(correct$cc_date[1], as.Date("2017-02-15"))
        expect_equal(correct$cc_date[2], as.Date("2008-02-21"))
    })
