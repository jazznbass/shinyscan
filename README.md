# Shiny scan

*Shiny scan* is a shiny application that is developed as a graphical user interface for the scan package. *scan* stands for **S**ingle-**C**ase Data **An**alysis. 

## How to install *Shiny scan*?

1. Install R from CRAN  (https://cloud.r-project.org/)
2. Install R-Studio (https://posit.co/download/rstudio-desktop/)
3. Windows users only: install Rtools (https://cran.r-project.org/bin/windows/Rtools/)

From within R/R-Studio use the following commands:

4. Install devtools: `install.packages("devtools")`
5. Install the latest version of scan: `devtools::install_github("jazznbass/scan")`
6. Install scplot: `devtools::install_github("jazznbass/scplot")`
7. Install shinyscan: `devtools::install_github("jazznbass/shinyscan")`
8. Start the app: `shinyscan::shinyscan()`

Or try all at once with:

``` .r
install.packages("devtools")
devtools::install_github("jazznbass/scan")
devtools::install_github("jazznbass/scplot")
devtools::install_github("jazznbass/shinyscan")
shinyscan::shinyscan()
````

If you get errors, look if a specific package is missing/ not found and try to install the missing package with `install.packages("the-missing-one")`.

## Finding help

- The "home" of *scan* is a good place to start from (https://jazznbass.github.io/scan/)
- For *scplot* look at (https://jazznbass.github.io/scplot/)
- You find in depth information an *scan* in the book [Analyzing single-case data with R and scan](https://jazznbass.github.io/scan-Book/)



