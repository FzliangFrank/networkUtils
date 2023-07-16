# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
usethis::use_package("visNetwork")
usethis::use_package("dplyr")
usethis::use_package("igraph", "depends")
usethis::use_package("networkD3")
usethis::use_package("htmlwidgets")
usethis::use_package("shinyWidgets")
usethis::use_package("shinyjs")
usethis::use_package("bs4Dash")
usethis::use_package("DiagrammeRsvg")
usethis::use_package("openxlsx")
usethis::use_package("readr")
usethis::use_package("purrr")
usethis::use_package("waiter")
usethis::use_package("sf")
usethis::use_package("RColorBrewer")
usethis::use_package("spatstat")
usethis::use_package("shinyjqui")
usethis::use_package("tidygraph") # I have to give up to tidy evalutation
# usethis::use_package_doc()
usethis::use_import_from("igraph", "V")
usethis::use_import_from("igraph", "E")
usethis::use_import_from("bs4Dash", "box")
## Amend DESCRIPTION with dependencies read from package code parsing
## install.package('attachment') # if needed.
attachment::att_amend_desc()

## Add modules ----
## Create a module infrastructure in R/
golem::add_module(name = "visNetworkRead", with_test = TRUE) # Query node or edge in network
golem::add_module(name = "visNetworkWrite", with_test = TRUE) # Write a node or Edge
golem::add_module(name = "fileUploader", with_test = TRUE) # Upload file with confidence
## Add helper functions ----
## Creates fct_* and utils_*
golem::add_fct("spiderOptions", with_test = TRUE)
golem::add_fct("sumAttributes.http")
golem::add_fct("modify_graph", with_test = TRUE) # api for modifying graph based on
# selected list
# this is useful when develop against an API instead of develop against
# static excel sheet or R obejct
golem::add_fct("plotPPPdensity") # for rendering geometries (pretty)
golem::add_utils("callmebyName", with_test = TRUE)
golem::add_utils("igraph_to_svg") #
golem::add_utils("blurry_range")
golem::add_utils("add_vertex_sf") # for comparability of sf
golem::add_utils("search_idx")

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file("script")
golem::add_js_handler("handlers")
golem::add_css_file("custom")
golem::add_sass_file("custom")

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw(name = "my_dataset", open = FALSE)

## Tests ----
## Add one line by test you want to create
usethis::use_test("app")

# Documentation

## Vignette ----
usethis::use_vignette("ShinySpider")
devtools::build_vignettes()

## Code Coverage----
## Set the code coverage service ("codecov" or "coveralls")
usethis::use_coverage()

# Create a summary readme for the testthat subdirectory
covrpage::covrpage()

## CI ----
## Use this part of the script if you need to set up a CI
## service for your application
##
## (You'll need GitHub there)
usethis::use_github()

# GitHub Actions
usethis::use_github_action()
# Chose one of the three
# See https://usethis.r-lib.org/reference/use_github_action.html
usethis::use_github_action_check_release()
usethis::use_github_action_check_standard()
usethis::use_github_action_check_full()
# Add action for PR
usethis::use_github_action_pr_commands()

# Travis CI
usethis::use_travis()
usethis::use_travis_badge()

# AppVeyor
usethis::use_appveyor()
usethis::use_appveyor_badge()

# Circle CI
usethis::use_circleci()
usethis::use_circleci_badge()

# Jenkins
usethis::use_jenkins()

# GitLab CI
usethis::use_gitlab_ci()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")
