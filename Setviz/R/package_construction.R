
rm(list=ls());if(!("rstudioapi" %in% installed.packages()[,"Package"])){install.packages("rstudioapi")};require("rstudioapi");
setwd(dirname(rstudioapi::getActiveDocumentContext()$path));setwd("./../..")

install.packages(c("devtools", "roxygen2", "testthat", "knitr","UpSetR"))

install.packages("rstudioapi")
rstudioapi::isAvailable("0.99.149")
devtools::install_github("r-lib/devtools")

library(roxygen2)
library(devtools)
has_devel()
print(has_devel())

.onLoad <- function(libname, pkgname){
  packageStartupMessage("Welcome to a package that lets you visualise intersecting sets")
}

print(rtools_path() )
has_rtools()
build()
devtools::load_all()
check()
test()
Sys.setenv(PATH = paste("c:\\Rtools\\bin\\", Sys.getenv("PATH"), sep=";"))
Sys.setenv(BINPREF = "C:\\Rtools\\mingw_$(WIN\\bin\\")


