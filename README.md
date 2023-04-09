
<!-- README.md is generated from README.Rmd. Please edit that file -->

# NetworkUtils

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

Empowered by shiny and visNetwork, this is a font-end development to
interactively view and modify any graph based network structure.

## Installation

You can install the development version of ShinySpider from
[GitHub](https://github.com/) with:

``` r
remotes::install_github("FzliangFrank/networkUtils")
```

## Example

``` r
run_simpleNetworkUtilApp()
```

[Simple Network Util
App](https://frank-the-tank.shinyapps.io/networkutils/?_ga=2.2760560.1153125338.1680970415-1261124081.1680970415)
is avaiable on shiny.io

## Trivia

### To-do List

- [ ] `mod_visNetworkRead` should scope much more data type.
- [x] specialized shiny app
- [x] When scoping namespace within `visNetwork` own function, session
  id do not get appended automatically. This might get fixed in the
  future. For production reasons, freeze current visNetwork package in
  namespace
