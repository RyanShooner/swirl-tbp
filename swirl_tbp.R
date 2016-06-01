################################################################
# load and update swirl functions for use in swirl-tbp 
################################################################

swirl_cat <-function(...) {
 #cat(...)
}

library(swirl)
source("parse_content.R")
source("resume.R")
source("tokens.R")
source("testResponse.R")
source("instructionSet.R")

e=environment(getFromNamespace("parse_content.yaml", "swirl"))
environment(parse_content.yaml) = e
environment(resume.default) = e
environment(testResponse.default) = e
environment(rpt) = e
environment(swirl) = e
environment(waitUser.default) = e

assignInNamespace("parse_content.yaml", parse_content.yaml, "swirl") 
assignInNamespace("resume.default", resume.default, "swirl") 
assignInNamespace("testResponse.default", testResponse.default, "swirl") 
assignInNamespace("waitUser.default", waitUser.default, "swirl") 
assignInNamespace("swirl", swirl, "swirl") 

# Returns TRUE if the variable 'var' has the value 'val' with an error
# tolerance of epsilon. Error tolerance is needed to check some calculations. For example
# identical(X,Y) returns FALSE for X = 1-pnorm(3) and Y = pnorm(3, lower.tail = FALSE)
# since a user could use either, an error tolerance (e.g., eps = 1e-10) is needed
# Note that the 'var' should be the name of a variable in quotes
# do we want to add this to answerTests2.R of swirl package?
var_has_value <- function(var, val, eps = 0) {
  var <- str_trim(var)
  if(exists(var, globalenv())){
    var <- get(var, globalenv())
    return (identical(abs(val-var)<=eps, TRUE))
  } else {
    swirl_out(paste0("Error: ", var, " does not exist. Make sure to store your answer in ", var, "."))
    return(FALSE)
  }
}

environment(var_has_value) = e
