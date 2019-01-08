# is_in_test_dir<-(getwd() %>% gsub("\\/$","",.) %>% strsplit("/") %>% .[[1]] %>% last)=="unit_tests"
# if(!is_in_test_dir){setwd("./internal/R/unit_tests/")}

context("Unit tests for set intersection")
print(getwd())
print(list.files())
source("test_utilities.R")

#
# test_that("confidence_intervals_mean inputs correct",{
#   ###This needs to be tested with a dependent var thats select one, one that's select multiple, one that's numeric etc
#   expect_is(expand_to_set_intersections(tf$numeric[1], design = design), "data.frame") #numerical var
#   expect_is(expand_to_set_intersections(tf$numeric_NA_heavy[1], design = design), "data.frame") #numerical var
#   expect_is(expand_to_set_intersections(tf$logical[1], design = design), "data.frame")
#   expect_warning(expand_to_set_intersections(tf$numeric[1], tf$select_one[2] , design = design))
#   expect_error(expand_to_set_intersections(example$data, bad_vector))
#   expect_error(expand_to_set_intersections(tf$select_one[1], design = design))
#   expect_error(expand_to_set_intersections(tf$NAs[1], design = design))
#   expect_error(expand_to_set_intersections(tf$fake[1], design = design)) #nonexistent.var
#   expect_error(expand_to_set_intersections(tf$select_multiple[1], design = design)) # select multiple
# })

test_that("expand_to_set_intersections inputs correct",{

expect_error(expand_to_set_intersections(example$data, bad_vector))
expect_is(expand_to_set_intersections(example$data, good_vector), "data.frame")
  })

