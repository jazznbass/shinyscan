resources <- list()
resources$choices <- list()
resources$choices$examples <- c("My scdf", substr(data(package = "scan")$results[,3], 0, nchar(data(package = "scan")$results[,3]) - 12))

resources$choices$fn_stats <- c(
  "describe", "smd", "overlap", "trend", "autocorr", "cdc", "plm", "hplm",
  "tau_u", "corrected_tau", "pnd", "pem", "pet", "pand", "nap",
  "rand_test", "outlier"
)

resources$choices$fn_plot <- c("plot" = "plot.scdf", "scplot" = "scplot")

tmp_filename <-  "temp-scdf.rds" #file.path(tempdir(), "my-scdf.rds")
saveRDS(NULL,tmp_filename)


# define js function for opening urls in new tab/window
js_code <- "
shinyjs.openURL = function(url) {
  window.open(url,'_blank');
}
"

