
combine_varnames_to_evaluable_expressions_mutually_exclusive<-function(varnames){
  newvarnames<-lapply(1:length(varnames),
                      function(x){
                        included <- as.data.frame(combn(varnames,x),stringsAsFactors = FALSE)
                        excluded <- as.data.frame(lapply(as.data.frame(included,stringsAsFactors = FALSE),function(x){varnames[!(varnames %in% x)]}))
                        included_expression <- included %>% apply(2,paste,collapse="&")
                        excluded_expression<- excluded %>% apply(2,
                                                                 paste,
                                                                 collapse="&!") %>%
                          (function(x){if(length(x)!=0 & !all(x=="")){paste0("&!",x)}else{x}})
                        complete_expression <- paste0(included_expression,excluded_expression)
                      }) %>% unlist
  newvarnames
}


combine_varnames_to_evaluable_expressions_cummulative<-function(varnames){
  newvarnames<-lapply(1:length(varnames),function(x){
    combn(varnames,x) %>% apply(2,paste,collapse="&")
  }) %>% unlist
  newvarnames
}
