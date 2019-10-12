#' @inherit fail_drugs
#' @importFrom assertthat assert_that
#' @importFrom dplyr left_join group_by mutate arrange lag filter slice 
#' @importFrom stats complete.cases
#' @importFrom stringr str_which

fail_drugs.epiinfo <- function(adm, 
                               change,
                               stop_days = 30) {
  
  # check args
  assertthat::assert_that(is.data.frame(adm),
                          is.data.frame(change),
                          is.numeric(stop_days),
                          length(stop_days) == 1L, 
                          length(unique(adm$APID)) == nrow(adm))
  
  # select variables from admission data
  a <- adm[, c("APID", "STARTTRE", "DATEN")]
  
  # merge adm and change data
  data <- dplyr::left_join(a, 
                           change, 
                           by = "APID")

  
  # remove rows with no date or no start/end date
  data <- data[complete.cases(data[c("STARTTRE", "change_dt")]), ]
  
  # remove rows with no "STOP" information
  data$stops <- rowSums(data[ , stringr::str_which(names(data), 
                                                   pattern = "_change$")] == "Stop", 
                        na.rm = TRUE)
  data <- data[data$stops > 0, ]
  
  # detect when two drugs or more are stopped on the same day
  single_day <- data[data$stops >= 2, ]

  # detect when two drugs stopped within arg(stop_days of each other)
data <- data %>% 
  dplyr::group_by(.data$APID) %>% 
  # calculate days between drug "stops"
  dplyr::mutate(int_days = as.integer(.data$change_dt - dplyr::lag(.data$change_dt, n = 1))) %>% 
  # filter intervals less than arg(stop_days)
  dplyr::filter(.data$int_days <= stop_days) %>% 
  # row bind single_day
  dplyr::bind_rows(single_day) %>% 
  # keep earliest 2nd stop per patient
  dplyr::arrange(.data$APID, .data$change_dt) %>% 
  dplyr::slice(1)
  
  # format output
  data <- data[, c("APID", "int_days", "change_dt")]
  
  # rename and recode 
  names(data) <- c("APID", "fail_drugs", "fail_drugs_dt")
  
  data$fail_drugs <- 1L

  # merge with admission data
  out <- dplyr::left_join(x = a[, "APID", drop = FALSE], 
                          y = data, 
                          by = "APID")
  
  # convert all NA to 0 in fail_drugs
  out$fail_drugs[is.na(out$fail_drugs)] <- 0L

  out
}

