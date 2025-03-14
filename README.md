
# govScrapeR

<!-- badges: start -->
<!-- badges: end -->

govScrapeR is a simple set of scripts to grab constantly changing government data, and present it a workable format for data analysis. It began as a desire to capture and categorize White House press releases and executive orders—as they were coming fast and furious immediately after the 2025 term began. I decided to make it public after DOGE released their website on savings, and questions about their own transparency began to emerge. 

Currently, govScrapeR offers two primary functions. The most recent, and useful is related to doge.gov/savings. Because doge.gov has published their findings online, but not as a dataset, govScrapeR scrapes the dynamically-created web data and returns it in table form. DOGE pulls their data from fpds.gov, so I have also included functionality that queries the ATOM feed at fpds.gov based on the PIID numbers supplied by DOGE. This can be used for comparison and fact-checking.

The other functionality captures data from whitehouse.gov/news. If desired, it also integrates with OpenAI to summarize and categorize posts from the website. I am including this, but it's still in somewhat rough shape. 

This project is intended to be open, and to foster greater transparency of government actions. Feel free to fork, expand, port, or suggest improvements

## Installation

You can install the development version of govScrapeR from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("rws-r/govScrapeR")
```


