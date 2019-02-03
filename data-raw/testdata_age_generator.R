## age_generator test data
# Koch_6
age_gen_koch6 <- data.frame(dateofbirth = lubridate::dmy("1/1/2015",
                                              "5/3/2012",
                                              "7/6/1967"),
                 Starttre = lubridate::dmy("3/1/2016",
                                           "7/3/2018",
                                           NA_character_),
                 stringsAsFactors = FALSE)
class(age_gen_koch6) <- c(class(age_gen_koch6), "koch6")

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


# EpiInfo
age_gen_epi <- data.frame(BIRTDATE = lubridate::dmy("1/1/2015",
                                              "5/3/2012",
                                              "7/6/1967"),
                 STARTTRE = lubridate::dmy("3/1/2016",
                                           "7/3/2018",
                                           NA_character_),
                 stringsAsFactors = FALSE)
class(age_gen_epi) <- c(class(age_gen_epi), "epiinfo")

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
