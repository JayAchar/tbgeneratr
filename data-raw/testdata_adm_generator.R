# raw data for adm_generator tests
# koch 6
k6 <- data.frame(Starttre = rep(lubridate::dmy("1/1/2018"), 5),
                 dateofbirth = rep(lubridate::dmy("1/2/2015"), 5),
                 weight = rep(5L, 5),
                 height = rep(1.23, 5))
class(k6) <- c(class(k6), "koch6")

saveRDS(k6, file = "inst/testdata/adm_generator_koch6.rds")

# epiinfo
epiinfo <- data.frame(STARTTRE = rep(lubridate::dmy("1/1/2018"), 5),
                 BIRTDATE = rep(lubridate::dmy("1/2/2015"), 5),
                 WEIGHT = rep(5L, 5),
                 HEIGHT = rep(1.23, 5))
class(epiinfo) <- c(class(epiinfo), "epiinfo")

saveRDS(epiinfo, file = "inst/testdata/adm_generator_epiinfo.rds")