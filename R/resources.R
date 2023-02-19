library(scan)
library(scplot)
library(shiny)
library(shinyjs)

res <- list()

res$choices <- list()
res$choices$examples <- c("(none)", substr(data(package = "scan")$results[,3], 0, nchar(data(package = "scan")$results[,3]) - 12))

res$choices$fn_stats <- c(
  "describe", "smd", "overlap", "trend", "autocorr", "cdc", "plm", "hplm",
  "tau_u", "corrected_tau", "pnd", "pem", "pet", "pand", "nap",
  "rand_test", "outlier"
)

res$choices$fn_plot <- c("scplot" = "scplot", "plot" = "plot.scdf")

res$placeholder$transform <- "e.g.
values = scale(values),
across_cases(values2 = scale(values)
"

res$placeholder$plot_arguments <- 'e.g. for scplot:
add_statline("trend", color = "red", width = 2)
add_theme("simple")

e.g. for plot:
style = "sienna"
lines = list("loreg", f = 0.2, lty = "solid", col = "black", lwd = 3)
'

res$error_msg$invalid_case <- "Sorry!
The last case you tried to add didn't have a valid case definition."

res$error_msg$plot <- "Sorry!
The plot arguments are not valid."


res$msg$startup <-
"Welcome to 'shiny scan'!

You can:

1. create a new case (click 'Add')
2. load a dataset (click 'Load file' to import an rds, csv, or excel file)
3. choose an example scdf (choosse from 'Load example')

'exampleABC' is a good place to start.

The basic procedure is:

1. *Choose or create* a single case file in the 'scdf' tab.
2. Optionally *refine* the case in the 'Transform' tab (select cases, recombine phases, etc.)
3. *Analyse* the data in the 'Stats' tab.
4. Create a *plot* in the 'Plot' tab.

Look at the 'Help' tab for more information.

Have fun!
"

res$msg$no_case_scdf <-
"No case has been defined yet.
You can:
1. create a new case (click 'Add')
2. load a dataset (click 'Load file' to import an rds, csv, or excel file)
3. choose an example scdf (choosse from 'Load example')
"

res$msg$no_case <-
"There is no case defined yet.
Please define a case on the 'scdf' tab first.
"

res$help_page <- "
#### Welcome to ***shiny scan***!

*Shiny-scan* is a graphical surface for the *scan* (Single-Case Data Analysis). *scan* is an R package.

The basic procedure is:

1. Choose/ create a single case file in the **scdf tab**.
2. Optionally refine the case in the **Transform tab** (select cases, recombine phases, etc.)
3. Analyse the data in the **Stats tab**.
4. Create a plot in the **Plot tab**.

Analysis and plots are based on the scdf after any changes from the **Transform tab**.

You have two plot engines to choose from. scplot is much more powerful.

Here are helpful links:

[Help pages for scan](https://jazznbass.github.io/scan/)

[Online book for single case analysis with scan](https://jazznbass.github.io/scan-Book/)

[Help pages for scplot](https://jazznbass.github.io/scplot/)

Have fun!
"

# define js function for opening urls in new tab/window
res$java$window.open <- "
shinyjs.openURL = function(url) {
  window.open(url,'_blank');
}
"

### little help-functions

trim <- function(x) {
  gsub("\n", ", ", trimws(x))
  x <- gsub("\n", ", ", trimws(x))
  gsub(",,", ",", trimws(x))
}

quoted <- function(x) {
  out <- paste0("\"", x, "\"")
  out[is.na(x)] <- NA
  out
}

n2br <- function(x) gsub("\n", "<br>", x)
