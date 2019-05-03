rm(list=ls());if(!("rstudioapi" %in% installed.packages()[,"Package"])){install.packages("rstudioapi")};require("rstudioapi");
setwd(dirname(rstudioapi::getActiveDocumentContext()$path));setwd("./")
getwd()
rstudioapi::isAvailable("0.99.149")


devtools::install_github("r-lib/devtools")
require(c("devtools", "roxygen2", "testthat", "knitr","UpSetR", "survey"))

library(roxygen2)
library(devtools)
library(survey)

has_devel()
print(has_devel())

.onLoad <- function(libname, pkgname){
  packageStartupMessage("Welcome to a package that lets you visualise intersecting sets")
}

print(rtools_path() )
has_rtools()
build()
devtools::load_all()
devtools::document()
devtools::test()
devtools::build_vignettes()


