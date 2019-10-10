# test data for fail_ltfu()
library(magrittr)

new_data <- function(unique_id_int,
                     tx_months, 
                     bdq,
                     lzd, 
                     lfx) {
  
  assertthat::assert_that(is.numeric(tx_months),
                          length(bdq) == length(tx_months))
  
  start_date <- lubridate::dmy("1/1/2010")
  
  # gen APID vector
  id <- paste0("XYZ", unique_id_int)
  
  # output adm dataframe
  adm <- data.frame(APID = id,
                    STARTTRE = start_date,
                    stringsAsFactors = FALSE)
  
  # define adherence data
  tx_month <- tx_months
  
  id_lab <- rep(id, length(tx_month))
  
  adhere <- data.frame(APID = id_lab,
                       tx_month = tx_month,
                       adhere_pct_BDQ = bdq,
                       adhere_pct_LZD = lzd,
                       adhere_pct_LFX = lfx,
                       adhere_pct_H = NA_integer_,
                       adhere_pct_R = NA_integer_,
                       adhere_pct_E = NA_integer_,
                       adhere_pct_Z = NA_integer_,
                       adhere_pct_S = NA_integer_,
                       adhere_pct_KA = NA_integer_,
                       adhere_pct_CAP = NA_integer_, 
                       adhere_pct_OFL = NA_integer_, 
                       adhere_pct_ETH = NA_integer_, 
                       adhere_pct_CYC = NA_integer_, 
                       adhere_pct_PAS = NA_integer_, 
                       adhere_pct_AMX = NA_integer_, 
                       adhere_pct_CLA = NA_integer_, 
                       adhere_pct_CLO = NA_integer_,
                       adhere_pct_MFX = NA_integer_, 
                       adhere_pct_PTO = NA_integer_,
                       adhere_pct_IMP = NA_integer_,
                       adhere_pct_DLM = NA_integer_,
                    stringsAsFactors = FALSE)
  
  list(adm = adm,
       adhere = adhere)
}

records <- list(
  # 100% adherence
  list(1, 1:3, c(100, 100, 100), c(100, 100, 100), c(100, 100, 100)),
  # ltfu at month 3
  list(2, 1:3, c(100, 0, 0), c(100, 0, 0), c(100, 0, 0)),
  # ltfu at month 3 with > 0% adherence in some months
  list(3, 1:3, c(100, 80, 0), c(100, 0, 70), c(100, 0, 0)),
  # ltfu except for missing months of adherence
  list(4, c(1, 2, 5), c(100, 0, 0), c(100, 0, 0), c(100, 0, 0)),
  # 0% adherence in first month after subject 4 last month == 0% 
  list(5, 1:3, c(0, 100, 100), c(0, 100, 100), c(0, 100, 100)),
  # all NA
  list(6, 1:3, rep(NA_integer_, 3), rep(NA_integer_, 3), rep(NA_integer_, 3)), 
  # two episodes of ltfu - choose earlier
  list(7, 1:5, c(0, 0, 100, 0, 0), c(0, 0, 100, 0, 0), c(0, 0, 100, 0, 0))
  )

## construct formula to generate data frame
num <- length(records[[1]])
args <- paste0(".x[[", 1:num, "]]", collapse = ", ")
form <- as.formula(paste0("~ new_data(", args, ")"))

epi <- purrr::map(records, .f = form) %>% 
  purrr::pmap(dplyr::bind_rows)

# apply class
class(epi$adm) <- c("data.frame", "epiinfo")
class(epi$adhere) <- c("data.frame", "epiinfo")

# save koch6 data
saveRDS(epi, "inst/testdata/fail_ltfu_epi.rds", version = 2)






  
