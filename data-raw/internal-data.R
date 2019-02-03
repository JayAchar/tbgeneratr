## Generate internal data

# drug_look_up
drug_look_up <- data.frame(drug_string = c("bdq",
                                           "dlm", "dld",
                                           "mfx",
                                           "cfz", "clo",
                                           "lfx",
                                           "lzd", "lzn",
                                           "imp"),
                           change_drug_name = c("bdq_change",
                                                rep("dlm_change", 2),
                                                "mfx_change",
                                                rep("cfz_change", 2),
                                                "lfx_change",
                                                rep("lzd_change", 2),
                                                "imp_change"),
                           epi_drug = c("BDQDBDQ", 
                                        rep("DLMDDLM", 2),
                                        "MXDMX", 
                                        rep("CLODCLO", 2), 
                                        "LXDLX", 
                                        rep("LZDDLZD", 2),
                                        "IMPDIMP"),
                           stringsAsFactors = FALSE)

usethis::use_data(drug_look_up, internal = TRUE, overwrite = TRUE)
