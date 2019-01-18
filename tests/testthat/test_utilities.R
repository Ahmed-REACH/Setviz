# where are the examples stored?
# setwd("./../..")

example.data.path<-function(examplename){
  return(paste0("./tests/test_data/",examplename,"/"))
}


# set available examples' metadata:

example_names<-c("example1","example2")
# Metadata for examples.
# Data frame with one row per

example_metadata<-data.frame(
  name=example_names,
  path=sapply(example_names,example.data.path) %>% unname,
  choice.label.column.to.use=c("Label::English","Label::English"),
  stringsAsFactors = F
)



read.example.csv<-function(filename,examplename){
  read.csv(paste0(example.data.path(examplename),filename))
}



load.example<-function(name,global_space=F){


  ex<-example_metadata[which(example_names==name),,drop=F] %>% as.list

  exfile<-function(file){
    read.example.csv(file,ex$name)
  }
  exfilepath<-function(file){
    paste0(ex$path,file)
  }

  ex$data<- data
  # ex$questionnaire<-load_questionnaire(ex$data,
  #
  #                                       questions.file = exfilepath("kobo questions.csv"),
  #                                       choices.file = exfilepath("kobo choices.csv"),
  #                                       choices.label.column.to.use = ex$choice.label.column.to.use)

  ex$names_good <- c("Food",	"Health",	"WASH",	"Education",	"Protection",	"Shelter",	"Livelihoods")
  ex$names_good_bad_sign <- c("MCNA_FoodSec1",	"health_score1",	"wash3_score1",	"MCNA_education_score1",	"protection_&score5V2b",	"shelter_score1",	"live_score1")
  ex$names_bad <- c("Food",	"Health",	"cross_score_cat&",	"Education",	"Protection",	"Shelter",	"Livelihoods")
  ex$weight_var <- "weight_nat"
  ex$random_var_num <- "cwg_income"
  ex$random_var_cat <- "hh_type"
  ex$random_var_NA_heave <- "child_spouse"


  return(ex)
}
