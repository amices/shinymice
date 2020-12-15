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
## Add one line by package you want to add as dependency
usethis::use_package("mice")
usethis::use_package("ggplot2")
usethis::use_package("dplyr")
usethis::use_package("plotly")
usethis::use_package("psych")
usethis::use_package("haven")
usethis::use_package("shinyjs")
usethis::use_package("waiter")
usethis::use_pipe()

## Add modules ----
## Create a module infrastructure in R/
golem::add_module(name = "00_home") # Name of the module
golem::add_module(name = "01_data") # Name of the module
golem::add_module(name = "02_model") # Name of the module
golem::add_module(name = "03_convergence") # Name of the module
golem::add_module(name = "04_imputations") # Name of the module
golem::add_module(name = "05_save") # Name of the module

## Add helper functions ----
## Creates ftc_* and utils_*
#golem::add_fct("helpers")
golem::add_utils("ui")
golem::add_utils("server")
# golem::add_utils("00_home")
golem::add_utils("01_data")
# golem::add_utils("02_model")
# golem::add_utils("03_convergence")
# golem::add_utils("04_imputations")
# golem::add_utils("05_save")

# #golem::add_utils("ui", module = "missingness")
# golem::add_utils("server", module = "missingness")
# # golem::add_utils("ui", module = "imputationmodel")
# golem::add_utils("server", module = "imputationmodel")
# #golem::add_utils("ui", module = "imputeddata")
# golem::add_utils("server", module = "imputeddata")

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file("script")
golem::add_js_handler("handlers")
golem::add_css_file("custom")

## Add internal datasets ----
## If you have data in your package
# usethis::use_data_raw(name = "my_dataset", open = FALSE)
# load("plotmouse.RData")
usethis::use_data(mouse)

## Tests ----
## Add one line by test you want to create
usethis::use_test("plot_md_pattern")
usethis::use_test("plot_flux")
usethis::use_test("plot_pred_matrix")

# Documentation

## Vignette ----
usethis::use_vignette("shinymice")
usethis::use_vignette("ggmice")
devtools::build_vignettes()

## Code coverage ----
## (You'll need GitHub there)
usethis::use_github()
usethis::use_travis()
usethis::use_appveyor()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")
