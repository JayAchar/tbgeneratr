# raw data for adm_generator tests
# koch 6
k6 <- data.frame(Starttre = rep(lubridate::dmy("1/1/2018"), 5),
                 dateofbirth = rep(lubridate::dmy("1/2/2015"), 5),
                 weight = rep(5L, 5),
                 height = rep(1.23, 5),
                 outcome = factor(c(2, 4, 7, 8, 0),
                                  levels = c(0:8),
                             labels = c("On treatment",
                                        "Cured",
                                        "Completed",
                                        "Death",
                                        "Fail",
                                        "LTFU",
                                        "Transfer out",
                                        "Other",
                                        "Transfer back to SCC")))

class(k6) <- c("data.frame", "koch6")

saveRDS(k6, file = "inst/testdata/adm_generator_koch6.rds", , version = 2)

# epiinfo
epiinfo <- data.frame(STARTTRE = rep(lubridate::dmy("1/1/2018"), 5),
                 BIRTDATE = rep(lubridate::dmy("1/2/2015"), 5),
                 WEIGHT = rep(5L, 5),
                 HEIGHT = rep(1.23, 5),
                 outcome = factor(c(2, 4, 6, 8, 0),
                                  levels = 0:8,
                                  labels = c("On treatment",
                                             "Cured",
                                             "Completed",
                                             "Death",
                                             "Fail",
                                             "LTFU",
                                             "Transfer out",
                                             "Fail & amplify",
                                             "Transfer to Cat 4")))

class(epiinfo) <- c("data.frame", "epiinfo")

saveRDS(epiinfo, file = "inst/testdata/adm_generator_epiinfo.rds", version = 2)
