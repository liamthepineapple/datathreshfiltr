
<!-- README.md is generated from README.Rmd. Please edit that file -->

# datathreshfiltr

<!-- badges: start -->
<!-- badges: end -->

The goal of datathreshfiltr is to filter the output metabolomic peak
area files from PeakMeister (an automated data preprocessing software
for capillary electrophoresis mass spectrometry data) below a specified
threshold and to replace all those values along with any NPD (no peak
detected) or (\<LOD) outputs with NA. Data with low peak areas are
typically indicative of false detections. The threshold can be set to
any value you choose although we suggest filtering out peaks below 15000
peak area.This filters your .cvs file and creates an object in the
environemnt which can be further exported into whatever filter you so
choose.

## Installation

You can install the development version of datathreshfiltr like so:

``` r
install.packages("devtools")
install.packages("usethis")
library(devtools)
library(usethis)

install_github("liamthepineapple/datathreshfiltr")
library(datathreshfiltr)
```

## Example

This is a basic example which shows you how to use the package

``` r
library(datathreshfiltr)
yourdatafile <- datathreshfiltr("your_file.csv", threshold = 15000)
## basic example code
```
