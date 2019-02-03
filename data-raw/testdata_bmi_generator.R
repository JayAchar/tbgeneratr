set.seed(100)
## Koch 6 normal data
bmi_k6 <- data.frame(weight = c(sample(c(1:100), 5, replace = T), NA_real_),
                     height = round(runif(6, 0.5, 2.5), 2))
class(bmi_k6) <- c(class(bmi_k6), "koch6")


## EpiInfo normal data
bmi_epi <- data.frame(WEIGHT = c(sample(c(1:100), 5, replace = T), NA_real_),
                     HEIGHT = round(runif(6, 0.5, 2.5), 2))
class(bmi_epi) <- c(class(bmi_epi), "epiinfo")
