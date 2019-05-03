rm(list=ls());if(!("rstudioapi" %in% installed.packages()[,"Package"])){install.packages("rstudioapi")};require("rstudioapi");
setwd(dirname(rstudioapi::getActiveDocumentContext()$path));setwd("./../..")
require("testthat")
source("./R/120 - dependencies.R")
source("./tests/testthat/test_utilities.R")

getwd()

data <- read.csv(file = "./tests/test_data/example1/data.csv")
weight_variable <- "weight_nat"
varnames <- c("MCNA_FoodSec1",	"health_score1",	"wash3_score1",	"MCNA_education_score1",	"protection_scoreSFHH2",	"shelter_score1",	"live_score1")

devtools::load_all()
devtools::test()
