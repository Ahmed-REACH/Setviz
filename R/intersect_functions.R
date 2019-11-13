#' Expand the binary indicators to set intersections
#'
#' @param data a dataframe containing all the 1,0 indicators in varnames
#' @param varnames a vector containing the names of variables to be used in the intersection
#' @param mutually_exclusive_sets if FALSE, intersecting sets will be cummulative, i.e. "A & B & C" will be also counted in "A & B"; if FALSE, records that have "A & B" will not be counted in "A".
#' @return The dataframe containing the intersections of all variables and the names of those combinations of varnames with '&'
#'
#' @importFrom magrittr %>%
#'
#' @export
expand_to_set_intersections <- function(data, varnames, mutually_exclusive_sets = FALSE) {
  ### sanitise inputs
  if(!is.data.frame(data))stop("input must be a data frame") #ensure first input is a dataframe
  if(any(grep("&", varnames)))stop("can't have the '&' sign in your variable names, it will mess everything up!")
  culprits <- varnames[!(varnames %in% names(data))]
  #ensure all the variable names are in the dataframe
  if(sum(varnames %in% names(data)) < length(varnames))stop(paste0("all the variable names must be found in the data: ", culprits, " is/are not"))
  if(!(sum(sapply(data[varnames], is.numeric)) == length(varnames)|
     sum(sapply(data[varnames], is.logical)) == length(varnames)))stop("all the variables must be numeric or logical") #ensure all columns are coercible to numbers

  ### creates a vector for the names of new variables using all combinations of varnames linked with '&'
  newvarnames <-  combine_varnames_to_evaluable_expressions_cummulative(varnames)
  if(!mutually_exclusive_sets){
  newvars_expression <- newvarnames
  }else{
    newvars_expression<-combine_varnames_to_evaluable_expressions_mutually_exclusive(varnames)
  }


  # coverts the columns in the data corresponding to varnames to T/F columns
  data <- lapply(data[,varnames],as.logical)
  attach(data)
  # creates setintersections, a dataframe of newvarnames with T/F in each column
  setintersections <- lapply(newvars_expression,function(x){
    eval(parse(text = x))
  })
  detach(data)
  setintersections<-as.data.frame(setintersections)
  names(setintersections)<- newvarnames
  # for msna tool, you might want to use a non-special-character placeholder for "&":
  # names(setintersections)<-gsub("&","._.a.n.d._.",newvarnames)
  return(setintersections)
}



#' Create a plot from the percentages in each set
#'
#' @param set_percentages a names vector with the percentages for each combination
#' @param nintersects number of intersections to look at, the default being 12
#' @param nsets number of unique sets making up the intersection
#' @param label the label to be added to the plot
#' @param round_to_1_percent if FALSE, will ignore sets with < 1%. If TRUE, will round up intersections >0 to at least 1% and include them.
#' @return A plot object
#'
#' @importFrom UpSetR upset fromExpression
#'
#' @export
set_intersection_plot<-function(set_percentages, nintersects = 12, nsets = NULL, label = NULL, round_to_1_percent = TRUE){
  set_percentages <- set_percentages*100
  # round up to 1% (unless 0); otherwise UpSetR will ignore those categories (because upsetr thinks these are counts..)
  if(round_to_1_percent){set_percentages[set_percentages<1 & set_percentages!=0] <- 1}
    set_percentages <- set_percentages %>% round
  label <- as.character(label)
  upset_object <- upset(fromExpression(set_percentages),
        order.by = "freq",
        nintersects = nintersects,
        nsets = nsets,
        mainbar.y.label = label
  )
  return(upset_object)
}

#' Create a vector containing the weighted percentages in each set
#'
#' @param data a dataframe containing all the sets for each record, coercible to 1,0
#' @param varnames  a vector containing the names of variables to be used in the intersection
#' @param exclude_unique whether the set intersections should include singular sets (i.e. that one variable). Note that if this is set to True, the total set size on the left will be wrong
#' @return A vector of the aggregated percent for each intersection
#'
#' @export

add_set_intersection_to_df <- function(data, varnames, exclude_unique = T, mutually_exclusive_sets = FALSE){
### Sanitise inputs
  if(!is.data.frame(data))stop("input must be a data frame") #ensure first input is a dataframe
  ###this function will change to reflect calculating the design from the sampling frame with map_to_design
  if(any(grep("&", varnames))) stop("can't have the '&' sign in your variable names, it will mess everything up!")
  if(any(grep("!", varnames))) stop("can't have the '!' sign in your variable names, it will mess everything up!")

  culprits <- varnames[!(varnames %in% names(data))]
  #ensure all the variable names are in the dataframe
  if(sum(varnames %in% names(data)) < length(varnames))stop(paste0("all the variable names must be found in the data: ", culprits, " is/are not"))

#### Use the expand_composite_indicators function to return the intersected sets,
  intersected_sets<- expand_to_set_intersections(data,
                                                 varnames,
                                                 mutually_exclusive_sets = mutually_exclusive_sets)
  newvarnames <- names(intersected_sets) #and save the names in a new vector

#### Take away the single indicators
  if(exclude_unique){
    intersected_sets <- intersected_sets[,-(1:length(varnames))]
    newvarnames <- newvarnames[-(1:length(varnames))]}

#### delete any duplicate variable names
  ## this is important especially if unique sets are included, where we know that they will be the same names

  data <- data[,!(names(data) %in% newvarnames)]

#### Append the new composite indicators to the dataset, fixing the names
  final_names <- c(names(data), newvarnames)
  data <- cbind(data, intersected_sets, stringsAsFactors = F)
  names(data) <- final_names

  results <- list()
  results$data <- data
  results$newvarnames <- newvarnames
return(results)
}


#' Create a vector containing the weighted percentages in each set
#'
#' @param data a dataframe containing all the intersected sets
#' @param intersected_names the names of the intersected sets: as combinations of variable names combined with '&'
#' @param weight_variable character string: the name of the variable in the dataset containing the weights
#' @return A plot of the intersection
#'
#' @importFrom survey svymean svydesign
#' @importFrom magrittr %>%
#'
#' @export

svymean_intersected_sets <- function(data, intersected_names, weight_variable = NULL, weighting_function = NULL){
  # this function will change to reflect calculating the design from the sampling frame with map_to_design
### Sanitise inputs
  culprits <- intersected_names[!(intersected_names %in% names(data))]
  #ensure all the variable names are in the dataframe
  if(sum(intersected_names %in% names(data)) < length(intersected_names))stop(paste0("all the variable names must be found in the data: ", culprits, " is/are not"))

#### Create the design object with the weights if applicable
  if(!is.null(weight_variable)){
    #check weighting variable is in the function
    # if the weights are not calculated by weights_of....
    if(!weight_variable %in% names(data))stop("weighting variable missing or not in dataframe")
    design <- svydesign(~1, weights = data[[weight_variable]], data = data) #later will become map_to_design
  } else if (!is.null(weighting_function)) {
    design <- svydesign(~1, weights = weighting_function(data), data = data)
  } else {
    design <- svydesign(~1, weights = NULL, data = data)
  }

#### Calculate the average % using svymean and save in a named vector
  aggregated.results <- svymean(data[,intersected_names], design, na.rm = T)
  aggregated.results.named <- aggregated.results %>% unlist %>% as.data.frame(., stringsAsFactors =F, na.rm = T)
  aggregated.results <- aggregated.results.named[,1]
  names(aggregated.results) <- rownames(aggregated.results.named)
#### Remove NAs from resulting vector
  aggregated.results <- aggregated.results[!is.na(aggregated.results)]
  return(aggregated.results)
}


#' Create a plot from a dataset and variable names combining the make_set_percentages and set_percentage_plot functions
#'
#' @param data  a dataframe containing all the 1,0 indicators in varnames
#' @param varnames  a vector containing the names of variables to be used in the intersection
#' @param weight_variable a character string: the name of the variable in the dataset containing the weights, defaults to NULL
#' @param nintersects number of intersections to look at, the default being 12
#' @param exclude_unique whether the set intersections should include singular sets (i.e. that one variable). Note that if this is set to True, the total set size on the left will be wrong
#' @param label the label to be added to the plot
#' @param round_to_1_percent if FALSE, will ignore sets with < 1%. If TRUE (default), will round up intersections >0 to at least 1% and include them.
#' @return An UpSetR plot object with the different sets
#' @export
plot_set_percentages <- function(data, varnames, weight_variable = NULL, weighting_function = NULL, nintersects = 12, exclude_unique = T, mutually_exclusive_sets = FALSE ,label = NULL,round_to_1_percent = TRUE){
  expanded_df <- add_set_intersection_to_df(data, varnames,
                                            exclude_unique = exclude_unique,
                                            mutually_exclusive_sets = mutually_exclusive_sets)
  case_load_percent <- svymean_intersected_sets(expanded_df$data, expanded_df$newvarnames, weight_variable)
  # nsets <- length(varnames)
  nsets<-length(case_load_percent)
  set_intersection_plot(case_load_percent, nintersects = nintersects, nsets = nsets, label,round_to_1_percent = round_to_1_percent)
  # on.exit()
}


