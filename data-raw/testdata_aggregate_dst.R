## test data for aggregate_dst

df <- structure(
  list(
    APID = c("XYZ1", "XYZ2", "XYZ3", "XYZ4", "XYZ4", "XYZ5", "XYZ6", "XYZ6"),
    hain_rif = structure(
      c(1L, 1L, 1L, 2L,
        1L, NA, NA, NA),
      .Label = c("Sensitive", "Resistant"),
      class = "factor"
    ),
    xpert_rif = structure(
      c(NA, 1L, 1L, 1L, 1L, NA, 1L, 2L),
      .Label = c("Not detected",
                 "Detected"),
      class = "factor"
    ),
    dst_p_rif = structure(
      c(1L,
        1L, 1L, 1L, 1L, NA, NA, NA),
      .Label = c("Sensitive", "Resistant"),
      class = "factor"
    ),
    dst_p_cm = structure(
      c(1L, 1L, 1L,
        2L, 1L, NA, NA, NA),
      .Label = c("Sensitive", "Resistant"),
      class = "factor"
    ),
    dst_p_km = structure(
      c(1L, 2L, 1L, 1L, 1L, NA, NA, NA),
      .Label = c("Sensitive",
                 "Resistant"),
      class = "factor"
    )
  ),
  row.names = c(NA,-8L),
  class = "data.frame"
)
saveRDS(df, "inst/testdata/aggregate_dst.rds")