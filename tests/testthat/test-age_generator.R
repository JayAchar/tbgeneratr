context("test-age_generator")
library(tbgeneratr)

# generate test data
# correctly formatted data frmae - Koch 6
k6_data <- structure(list(id = 1:10, 
               dateofbirth = structure(c(12541, 430, 
                                 10006, 10930, 9207, 4977, 8491, 4592, 13531, 7829), class = "Date"), 
               Starttre = structure(c(14217, 3797, 12418, 14796, 11785, 
                             8730, 9980, 6407, 13893, 10832), class = "Date")), 
                            class = "data.frame", row.names = c(NA, -10L))

k1 <- age_generator(k6_data, "koch_6", rm_orig = TRUE)


# add NA to date of birth
k6_data2 <- k6_data
k6_data2[10, 2] <- NA
k2 <- age_generator(k6_data2, "koch_6", rm_orig = TRUE)


# Starttre as string
k6_data3 <- k6_data
k6_data3$Starttre <- as.character(k6_data3$Starttre)

# future date in Starttre
k6_data4 <- k6_data
k6_data4[1, 3] <- Sys.Date() + 5
k4 <- age_generator(k6_data4, "koch_6", rm_orig = TRUE)

# age > Starttre
k6_data5 <- k6_data
k6_data5[1, 2] <- k6_data5$Starttre[1] + 1
k5 <- age_generator(k6_data5, "koch_6", rm_orig = TRUE)
 
# check rm_orig argument
k6_data6 <- k6_data
k6 <- age_generator(k6_data6, "koch_6", rm_orig = FALSE)


test_that("structure is ok", {
  expect_match(class(k1), "data.frame")
  expect_equal(ncol(k1), 3)
  expect_equal(nrow(k1), 10)
  expect_true(all(c("id", "Starttre", "age") %in% names(k1)))
  expect_match(class(k1$Starttre), "Date")
  expect_match(class(k1$age), "numeric")
})

test_that("content is ok", {
  expect_equal(length(unique(k1$id)), 10)
  expect_equal(round(k1$age[1], 2), 4.59)
  expect_equal(round(median(k1$age), 2), 6.83)
  expect_true(all(k1$age > 0))
})

test_that("DoB NA", {
    expect_equal(length(unique(k2$id)), 10)
    expect_equal(round(k2$age[1], 2), 4.59)
    expect_equal(round(median(k2$age, na.rm = TRUE), 2), 6.60)
    expect_equal(sum(is.na(k6_data2$dateofbirth)), sum(is.na(k2$age)))
})

test_that("Expect errors", {
    expect_error(age_generator(x = list(0), "koch_6"))
    expect_error(age_generator(k6_data3, "koch_6", rm_orig = TRUE),
                 "not equal to")  # start date as character variable
#    expect_message(age_generator(k6_data4, "koch_6", rm_orig = TRUE))
#  expect_error(age_generator(k6_data5, "koch_6, rm_orig = TRUE))    
})

test_that("test rm_orig arg", {
  expect_equal(ncol(k6), 4)  
  expect_true("age" %in% names(k6))    
})
