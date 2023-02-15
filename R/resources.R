resources <- list()
resources$choices <- list()
resources$choices$examples <- c("My scdf", substr(data(package = "scan")$results[,3], 0, nchar(data(package = "scan")$results[,3]) - 12))

resources$choices$fn_stats <- c(
  "describe", "overlap", "hplm", "plm", "tau_u", "cdc",
  "corrected_tau", "outlier", "pand", "pem", "pet", "pnd",
  "rand_test", "trend", "autocorr"
)

resources$choices$fn_plot <- c("plotSC" = "plot.scdf", "scplot" = "scplot")

tmp_filename <- "tmp_active_case.rds"
saveRDS(NULL,tmp_filename)
