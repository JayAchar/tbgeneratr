context("test-converter")
library(tbgeneratr)

# generate test data
# epiinfo - corect
out <- structure(list(id = c("a", "a", "a", "b", "b", "b", "b", "b", 
       "b", "b", "b"), samp_date = structure(c(17140, 17212, 17254, 
         13843, 13895, 13901, 13920, 13930, 13957, 13991, 14020), class = "Date"), 
        culture = structure(c(2L, 1L, 1L, 2L, 1L, 2L, 2L, 1L, 1L, 
        1L, 1L), .Label = c("Negative", "Positive"), class = c("ordered", 
        "factor")), STARTTRE = structure(c(17184, 17184, 17184, 13908, 
        13908, 13908, 13908, 13908, 13908, 13908, 13908), class = "Date"), 
        DATEN = structure(c(17256, 17256, 17256, 14214, 14214, 14214, 
        14214, 14214, 14214, 14214, 14214), class = "Date"), smear = structure(c(NA_integer_, 
        NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_, 
        NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_
        ), .Label = character(0), class = "factor")), row.names = c(NA, -11L), class = c("tbl_df", 
        "tbl", "data.frame"))

correct <- converter(out, "culture", "epiinfo", "kk", "adm") 

    
    test_that("culture conversion dates correct", {
        expect_equal(ncol(correct), 2)
        expect_equal(nrow(correct), 2)
        expect_true(all(c("id", "cc_date") %in% names(correct)))
        expect_match(class(correct$cc_date), "Date")
        expect_equal(length(unique(correct$id)), nrow(correct))
    })
