rm(list=ls());if(!("rstudioapi" %in% installed.packages()[,"Package"])){install.packages("rstudioapi")};require("rstudioapi");
setwd(dirname(rstudioapi::getActiveDocumentContext()$path));setwd("./../..")
require("testthat")
source("./R/120 - dependencies.R")
source("./R/unit_tests/test_utilities.R")

is_in_test_dir<-(getwd() %>% gsub("\\/$","",.) %>% strsplit("/") %>% .[[1]] %>% last)=="unit_tests"
if(!is_in_test_dir){setwd("./internal/R/unit_tests/")}

getwd()

example<-load.example("example1",F)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

data <- example$data
tf <- example$tf
questionnaire <- example$questionnaire
design <- svydesign(~0, data = data)
