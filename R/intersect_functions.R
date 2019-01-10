#'Expand the binary indicators to set intersections
#'
#'@param data a dataframe containing all the 1,0 indicators in varnames
#'@param varnames a vector containing the names of variables to be used in the intersection
#'@return The dataframe containing the intersections of all variables and the names of those combinations of varnames with '&'
#'@examples
#'@export
expand_to_set_intersections<-function(data,varnames){
  ### sanitise inputs
  if(!is.data.frame(data))stop("input must be a data frame") #ensure first input is a dataframe
  if(any(grep("&", varnames)))stop("can't have the '&' sign in your variable names, it will mess everything up!")
  culprits <- varnames[!(varnames %in% names(data))]
  #ensure all the variable names are in the dataframe
  if(sum(varnames %in% names(data)) < length(varnames))stop(paste0("all the variable names must be found in the data: ", culprits, " is/are not"))
  if(!(sum(sapply(data[varnames], is.numeric)) == length(varnames)|
     sum(sapply(data[varnames], is.logical)) == length(varnames)))stop("all the variables must be numeric or logical") #ensure all columns are coercible to numbers

  ### creates a vector for the names of new variables using all combinations of varnames linked with '&'
  newvarnames<-lapply(1:length(varnames),function(x){
    combn(varnames,x) %>% apply(2,paste,collapse="&")
  }) %>% unlist
  # coverts the columns in the data corresponding to varnames to T/F columns
  data <- lapply(data[,varnames],as.logical)
  attach(data)
  # creates setintersections, a dataframe of newvarnames with T/F in each column
  setintersections <- lapply(newvarnames,function(x){
    eval(parse(text = x))
  })
  detach(data)
  setintersections<-as.data.frame(setintersections)
  names(setintersections)<- newvarnames
  # for msna tool, you might want to use a non-special-character placeholder for "&":
  # names(setintersections)<-gsub("&","._.a.n.d._.",newvarnames)
  return(setintersections)
}

#'Create a plot from the percentages in each set
#'
#'@param set_percentages a names vector with the percentages for each combination
#'@param nsets number of intersections to look at, the default being 12
#'@param label the label to be added to the plot
#'@return A plot object
#'@examples
#'@export
set_intersection_plot<-function(set_percentages, nintersects = 12, label = NULL){
  set_percentages <- set_percentages*100 %>% round
  label <- as.character(label)
  upset_object <- upset(fromExpression(set_percentages),
        order.by = "freq", nintersects = nintersects,
        mainbar.y.label = label
        #, mainbar.y.max = 50
  )
  return(upset_object)
}

#'Create a vector containing the weighted percentages in each set
#'
#'@param data a dataframe containing all the sets for each record, coercible to 1,0
#'@param nsets number of sets to look at
#'@param nsets number of intersections to look at, the default being 12
#'@param label the label to be added to the plot
#'@return A vector of the aggregated percent for each intersection
#'@examples
#'@export

add_set_intersection_to_df <- function(data, varnames, exclude_unique = T){
### Sanitise inputs
  if(!is.data.frame(data))stop("input must be a data frame") #ensure first input is a dataframe
  ###this function will change to reflect calculating the design from the sampling frame with map_to_design
  if(any(grep("&", varnames)))stop("can't have the '&' sign in your variable names, it will mess everything up!")
  culprits <- varnames[!(varnames %in% names(data))]
  #ensure all the variable names are in the dataframe
  if(sum(varnames %in% names(data)) < length(varnames))stop(paste0("all the variable names must be found in the data: ", culprits, " is/are not"))

#### Use the expand_composite_indicators function to return the intersected sets,
  intersected_sets<- expand_to_set_intersections(data,varnames)
  newvarnames <- names(intersected_sets) #and save the names in a new vector

#### Take away the single indicators
  if(exclude_unique){
    intersected_sets <- intersected_sets[,-(1:length(varnames))]
    newvarnames <- newvarnames[-(1:length(varnames))]}

#### Append the new composite indicators to the dataset, fixing the names
  final_names <- c(names(data), newvarnames)
  data <- cbind(data, intersected_sets, stringsAsFactors = F)
  names(data) <- final_names

  results <- list()
  results$data <- data
  results$newvarnames <- newvarnames
return(results)
}


#'Create a vector containing the weighted percentages in each set
#'
#'@param data a dataframe containing all the intersected sets
#'@param intersected_names the names of the intersected sets
#'@param weight_variable the variables in the dataset containing the weights
#'@return A vector of the aggregated percent for each intersection
#'@examples
#'@export

svymean_intersected_sets <- function(data, intersected_names, weight_variable){
  # this function will change to reflect calculating the design from the sampling frame with map_to_design
### Sanitise inputs
  if(any(grep("&", varnames)))stop("can't have the '&' sign in your variable names, it will mess everything up!")
  culprits <- varnames[!(varnames %in% names(data))]
  #ensure all the variable names are in the dataframe
  if(sum(varnames %in% names(data)) < length(varnames))stop(paste0("all the variable names must be found in the data: ", culprits, " is/are not"))
  #check weighting variable is in the function
  # if the weights are not calculated by weights_of....
  if(!weight_variable %in% names(data))stop("weighting variable missing or not in dataframe")

#### Create the design object with the weights if applicable
  design <- survey::svydesign(~1, weights = data[[weight_variable]], data = data) #later will become map_to_design

#### Calculate the average % using svymean and save in a named vector
  aggregated.results <- svymean(data[,newvarnames], design, na.rm = T)
  aggregated.results.named <- aggregated.results %>% unlist %>% as.data.frame(., stringsAsFactors =F, na.rm = T)
  aggregated.results <- aggregated.results.named[,1]
  names(aggregated.results) <- rownames(aggregated.results.named)
#### Remove NAs from resulting vector
  aggregated.results <- aggregated.results[!is.na(aggregated.results)]
  return(aggregated.results)
}


#'Create a plot from a dataset and variable names combining the make_set_percentages and set_percentage_plot functions
#'
#'@param set_percentages a names vector with the percentages for each combination
#'@param nsets number of sets to look at
#'@param nsets number of intersections to look at, the default being 12
#'@param label the label to be added to the plot
#'@return A plot object
#'@examples
#'@export
make_set_percentages_plot <- function(data, varnames, weight_variable, nintersects = 12, exclude_unique = T, label){
  intersections_df <- expand_to_set_intersections(data, varnames)
  expanded_df <- add_set_intersection_to_df(data, varnames, exclude_unique = T)
  case_load_percent <- svymean_intersected_sets(expanded_df$data, expanded_df$newvarnames)
  plot <- set_intersection_plot(case_load_percent, nintersects, label)
  return(plot)
  }

#
#   # aggregated.results must be a vector with the names given as set1, set2, set1&set2 ... etc (as given by expand_composite_indicators_to_set_intersections()):
#   # then we can do:
#   save_the_plot <- function(aggregated.results){
#   plot <- set_intersection_plot(aggregated.results)
#   savePlot(paste0(unique(data$hh_type),"intersections"))
#   }
