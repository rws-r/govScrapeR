
# govScrapeR

<!-- badges: start -->
<!-- badges: end -->

govScrapeR is a simple set of scripts to grab constantly changing government data, and present it a workable format for data analysis. It began as a desire to capture and categorize White House press releases and executive orders—as they were coming fast and furious immediately after the 2025 term began. It morphed, however, primarily into a package designed to capture DOGE data from doge.gov/savings, and compare it with source data on FPDS. 

govScrapeR interacts with three different APIs: doge.gov, fpds.gov, and usaspending.gov. By capturing data from the later two, comparisons can be made against DOGE data. This package also includes a couple utility functions to match and analyze DOGE data. However, these are meant to be basic utilities to foster easier analysis by the researcher. 

This project is intended to be open, and to foster greater transparency of government actions. Feel free to fork, port, or suggest improvements.

## Installation

You can install the development version of govScrapeR from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("rws-r/govScrapeR")
```


