library(testthat)
library(Setviz)

example<-load.example("example1",F)
data <- example$data
tf <- example$tf
good_vector <- example$names_good
bad_vector <- example$names_bad
# design <- svydesign(~0, data = data)

test_check("Setviz")
