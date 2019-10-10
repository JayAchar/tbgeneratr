## Generate internal data
library(magrittr)
# drug_look_up

new_drug <- function(drug_string, 
                     ideal_drug_name,
                     change_drug_name,
                     epi_drug,
                     k6_drug) {
  
  data.frame(drug_string = drug_string,
             ideal_drug_name = ideal_drug_name,
             change_drug_name = change_drug_name,
             epi_drug = epi_drug,
             k6_drug = k6_drug,
             stringsAsFactors = FALSE)
  
}


drug_list <- list(
  # Bdq
  list("bdq", "bdq", "bdq_change", "BDQDBDQ", "Bdq"),
  # Dlm
  list(c("dlm", "dld"), "dlm", "dlm_change", "DLMDDLM", "Dld"),
  # Mfx
  list("mfx", "mfx", "mfx_change", "MXDMX", "Mfx"),
  # Cfz
  list(c("cfz", "clo"), "cfz", "cfz_change", "CLODCLO", "Cfz"),
  # Lfx
  list("lfx", "lfx", "lfx_change", "LXDLX", "Lfx"),
  # Lzd
  list(c("lzd", "lzn"), "lzd", "lzd_change", "LZDDLZD", "Lzd"),
  # Imp
  list("imp", "imp", "imp_change", "IMPDIMP", "ImpCln"),
  # Rif
  list(c("rif", "rmp"), "rif", "rif_change", "RDR", ""),
  # Inh
  list("inh", "inh", "inh_change", "HDH", ""),
  # Pza
  list("pza", "pza", "pza_change", "ZDZ", ""),
  # Eth
  list("eth", "eth", "eth_change", "EDE", ""),
  # Str
  list("str", "str", "str_change", "SDS", ""),
  # Km
  list("km", "km", "kan_change", "KADKA", ""),
  # Cm
  list("cm", "cm", "cap_change", "CAPDCAP", ""),
  # Ofx
  list("ofx", "ofx", "ofx_change", "OFLDOFL", ""),
  # Cyc
  list("cyc", "cyc", "cyc_change", "CYCLDCYCL", ""),
  # Pto
  list("pto", "pto", "pto_change", "PTDPT", ""),
  # Eto
  list("eto", "eto", "eto_change", "ETHDETH", ""),
  # PAS
  list("pas", "pas", "pas_change", "PASDPAS", ""),
  # Amx
  list("amx", "amx", "amx_change", "AMXDAMX", "")
)


## construct formula to generate data frame
  num <- length(drug_list[[1]])
  args <- paste0(".x[[", 1:num, "]]", collapse = ", ")
  form <- as.formula(paste0("~ new_drug(", args, ")"))

  # generate data frame
drug_look_up <- purrr::map(drug_list, .f = form) %>%
  dplyr::bind_rows()

# save as internal data
usethis::use_data(drug_look_up, internal = TRUE, overwrite = TRUE)
