#' Aggregate DST
#'
#' Generate laboratory sample DST results by prioritising resistant
#' results over sensitive results where more than one DST test has 
#' been performed
#' @param x data frame of laboratory DST data
#' @param drug_class string used to define data frame variables to use
#' to calculate aggregate DST results
#'
#' @return data frame with one factorised column of DST results with 
#' possible results include "Sensitive" or "Resistant"
#' @importFrom dplyr select matches
#' @importFrom purrr map_df pmap_dbl
#' @importFrom stringr str_detect
#'

aggregate_dst <- function(x, drug_class) {
  
  res <- x %>% 
    select(matches(drug_class)) %>% 
    map_df(.f = as.numeric) %>% 
    pmap_dbl(.f = pmax, na.rm = TRUE)
  
  # name new variable
  fq_str_detect <- c("ofx|lfx|mfx")
  sli_str_detect <- c("km|cm|am")
  
  if (any(str_detect(drug_class, fq_str_detect))) {
    var_name <- "fq_res"
  } else if (any(str_detect(drug_class, sli_str_detect))) {
    var_name <- "sli_res"
  } else {
    drug_stem <- str_remove(drug_class, "^_")
    var_name <- paste0(drug_stem, "_res")
  }

  # rename new variable
  df <- data.frame(new_var = factor(res, levels = 1:2,
                               labels = c("Sensitive", "Resistant")))
  names(df) <- var_name
  
  df

}


