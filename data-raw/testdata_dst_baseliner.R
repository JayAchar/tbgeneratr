# Generate testdata for dst_baseliner()

## EpiInfo admission and lab
### Admission data
epi_adm <-
  data.frame(APID = paste0("XYZ", 1:8), stringsAsFactors = F) %>%
  dplyr::mutate(
    STARTTRE = lubridate::dmy(c(rep("1/1/10", 8))),
    indictor_dst = factor(c(
      "DSTB",
      "Inh-mono",
      "Inh-mono",
      "pre-XDRTB (SLI)",
      NA_character_,
      "RRTB",
      "XDRTB",
      "DSTB"
    ))
  )

class(epi_adm) <- c(class(epi_adm), "epiinfo")
saveRDS(epi_adm, "inst/testdata/dst_baseliner_epi_adm.rds")
                

### Laboratory data


epi_lab <-
  structure(
    list(
      APID = c(
        "XYZ1",
        "XYZ2",
        "XYZ3",
        "XYZ4",
        "XYZ4",
        "XYZ5",
        "XYZ6",
        "XYZ6",
        "XYZ7",
        "XYZ8"
      ),
      samp_date = structure(
        c(14608,
          14608, 14608, 14608, NA, 14608, 14558, 14608, 14608, 14608),
        class = "Date"
      ),
      hain_inh = structure(
        c(1L, 2L, 1L, NA, 1L, NA, NA, NA, 2L, NA),
        .Label = c("Sensitive",
                   "Resistant"),
        class = "factor"
      ),
      hain_rif = structure(
        c(1L,
          1L, 1L, 2L, 1L, NA, NA, NA, 2L, NA),
        .Label = c("Sensitive", "Resistant"),
        class = "factor"
      ),
      xpert_res = structure(
        c(2L, 2L, 1L,
          1L, 1L, NA, 2L, 2L, NA, 2L),
        .Label = c("Negative", "Positive"),
        class = "factor"
      ),
      xpert_rif = structure(
        c(NA, 1L, 1L, 1L, 1L, NA, 1L, 2L, NA, 1L),
        .Label = c("Not detected",
                   "Detected"),
        class = "factor"
      ),
      dst_p_rif = structure(
        c(1L,
          1L, 1L, 1L, 1L, NA, NA, NA, 1L, NA),
        .Label = c("Sensitive", "Resistant"),
        class = "factor"
      ),
      dst_p_inh = structure(
        c(1L, 1L, 2L,
          1L, 1L, NA, NA, NA, 2L, NA),
        .Label = c("Sensitive", "Resistant"),
        class = "factor"
      ),
      dst_p_pza = structure(
        c(1L, 1L, 1L, 1L, 1L, NA, NA, NA, 2L, NA),
        .Label = c("Sensitive",
                   "Resistant"),
        class = "factor"
      ),
      dst_p_eth = structure(
        c(1L,
          1L, 1L, 1L, 1L, NA, NA, NA, NA, NA),
        .Label = c("Sensitive", "Resistant"),
        class = "factor"
      ),
      dst_p_cm = structure(
        c(1L, 1L, 1L,
          2L, 1L, NA, NA, NA, 1L, NA),
        .Label = c("Sensitive", "Resistant"),
        class = "factor"
      ),
      dst_p_mfx = structure(
        c(NA, 1L, 1L, 1L, 2L, NA, NA, NA, NA, NA),
        .Label = c("Sensitive",
                   "Resistant"),
        class = "factor"
      ),
      dst_p_km = structure(
        c(1L,
          2L, 1L, 1L, 1L, NA, NA, NA, 2L, NA),
        .Label = c("Sensitive", "Resistant"),
        class = "factor"
      ),
      dst_p_ofx = structure(
        c(1L, 1L, 2L,
          1L, 1L, NA, NA, NA, 2L, NA),
        .Label = c("Sensitive", "Resistant"),
        class = "factor"
      )
    ),
    row.names = c(NA,-10L),
    class = c("tbl_df", "tbl", "data.frame")
  ) %>%
  dplyr::mutate(MICRLABN = paste0("09-000", 1:nrow(.))) %>%
  dplyr::select(APID, MICRLABN, samp_date, dplyr::everything())

class(epi_lab) <- c(class(epi_lab), "epiinfo")
saveRDS(epi_lab, "inst/testdata/dst_baseliner_epi_lab.rds")


# ================================================
# Koch 6 with Grozny lab
### Admission data
k6_groz_adm <- epi_adm %>% 
  rename(registrationnb = APID,
         Starttre = STARTTRE) %>% 
  mutate(dstnumber = as.character(1:length(unique(.$registrationnb)))) %>% 
  select(registrationnb, dstnumber, dplyr::everything())


class(k6_groz_adm) <- c(class(k6_groz_adm), "koch6")
saveRDS(k6_groz_adm, "inst/testdata/dst_baseliner_k6_groz_adm.rds")


### Laboratory data
k6_groz_lab <- epi_lab %>% 
  rename(registrationnb = APID,
         dst_p_am = dst_p_km,
         dst_p_lfx = dst_p_ofx) %>% 
  mutate(dst_p_mfxhigh = dst_p_mfx) %>% 
  left_join(k6_groz_adm, by = "registrationnb") %>% 
  select(-registrationnb, -MICRLABN, -Starttre) %>% 
  select(dstnumber, dplyr::everything())

class(k6_groz_lab) <- c(class(k6_groz_lab), "grozny")
saveRDS(k6_groz_lab, "inst/testdata/dst_baseliner_k6_groz_lab.rds")
