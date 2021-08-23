
# STAT302PACKAGE

<!-- badges: start -->
[![R-CMD-check](https://github.com/RolinaC/STAT302PACKAGE/workflows/R-CMD-check/badge.svg)](https://github.com/RolinaC/STAT302PACKAGE/actions)
[![codecov](https://codecov.io/gh/RolinaC/STAT302PACKAGE/branch/master/graph/badge.svg?token=MC5J5B34H3)](https://codecov.io/gh/RolinaC/STAT302PACKAGE)
<!-- badges: end -->

The goal of STAT302PACKAGE is to demonstrate how to build a package

## Installation

You can install the released version of STAT302PACKAGE from GitHub using:

``` r
devtools::install.packages("STAT302PACKAGE")
```

To view vignettes, run the following code:
```{r}
devtools::install_github("RolinaC/STAT302PACKAGE", build_vignette = TRUE, build_opts = c())
library(Demo)
# Use this to view the vignette in the Demo HTML help
help(package = "STAT302PACKAGE", help_type = "html")
# Use this to view the vignette as an isolated HTML file
utils::browseVignettes(package = "STAT302PACKAGE")
```
