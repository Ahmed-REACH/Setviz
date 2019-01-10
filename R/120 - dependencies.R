# easier dependency loading:
source("./R/base_functions/utilities - dependency_loading.R")
# folders to load scripts from
source_folders<-c("./R/hypegrammaR",
                  "./R/koboreadeR",
                  "./R/base_functions")
# source them all:
sapply(source_folders,.source_dir)
# external packages to install/load:
.dependencies<-c("dplyr","purrr","questionr","data.table","tidyr","survey","magrittr","rstudioapi","igraph","knitr","rlist","ggplot2","ggthemes","extrafont","UpSetR")



# load external packages:
.install_dependencies(.dependencies)
.load_dependencies(.dependencies)


