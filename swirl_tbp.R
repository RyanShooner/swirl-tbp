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

## would go in answerTests2.R
var_has_value <- function(val, var_name) {
  var_name <- str_trim(var_name)
  if(exists(var_name, globalenv())){
    var <- get(var_name, globalenv())
    return (identical(val, var))
  } else {
    swirl_out(paste0("Error: ", var_name, " does not exist. Make sure to store your answer in ", var_name, "."))
    return(FALSE)
  }
}

environment(var_has_value) = e
