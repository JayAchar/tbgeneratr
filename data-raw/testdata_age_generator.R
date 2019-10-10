## age_generator test data
# Koch_6
age_gen_koch6 <- data.frame(dateofbirth = lubridate::dmy("1/1/2015",
                                              "5/3/2012",
                                              "7/6/1967",
                                              "1/1/1950",
                                              "1/1/1990"),
                 Starttre = lubridate::dmy("3/1/2016",
                                           "7/3/2018",
                                           NA_character_,
                                           "1/1/1995",
                                           "1/1/1992"),
                 stringsAsFactors = FALSE)
class(age_gen_koch6) <- c(class(age_gen_koch6), "koch6")
saveRDS(age_gen_koch6, file = "inst/testdata/age_gen_koch6.rds")

  # generate negative age
age_gen_koch6_neg <- data.frame(dateofbirth = lubridate::dmy("1/1/2015",
                                                         "5/3/2012",
                                                         "7/6/1967", 
                                                         "1/1/2016"),
                            Starttre = lubridate::dmy("3/1/2016",
                                                      "7/3/2018",
                                                      NA_character_,
                                                      "1/1/2012"),
                            stringsAsFactors = FALSE)
class(age_gen_koch6_neg) <- c(class(age_gen_koch6_neg), "koch6")
saveRDS(age_gen_koch6_neg, file = "inst/testdata/age_gen_koch6_neg.rds")

# EpiInfo
age_gen_epi <- data.frame(BIRTDATE = lubridate::dmy("1/1/2015",
                                              "5/3/2012",
                                              "7/6/1967",
                                              "1/1/1950",
                                              "1/1/1990"),
                 STARTTRE = lubridate::dmy("3/1/2016",
                                           "7/3/2018",
                                           NA_character_,
                                           "1/1/1995",
                                           "1/1/1992"),
                 stringsAsFactors = FALSE)
class(age_gen_epi) <- c(class(age_gen_epi), "epiinfo")
saveRDS(age_gen_epi, file = "inst/testdata/age_gen_epi.rds")

  # generate negative age
age_gen_epi_neg <- data.frame(BIRTDATE = lubridate::dmy("1/1/2015",
                                                    "5/3/2012",
                                                    "7/6/1967", 
                                                    "1/1/2016"),
                          STARTTRE = lubridate::dmy("3/1/2016",
                                                    "7/3/2018",
                                                    NA_character_,
                                                    "1/1/2012"),
                          stringsAsFactors = FALSE)
class(age_gen_epi_neg) <- c(class(age_gen_epi_neg), "epiinfo")
saveRDS(age_gen_koch6, file = "inst/testdata/age_gen_epi_neg.rds", version = 2)
