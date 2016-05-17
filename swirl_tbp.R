################################################################
# load and update swirl functions for use in swirl-tbp 
################################################################

library(swirl)
source("parse_content.R")
source("resume.R")
source("tokens.R")
source("testResponse.R")

e=environment(getFromNamespace("parse_content.yaml", "swirl"))
environment(parse_content.yaml) = e
environment(resume.default) = e
environment(testResponse.default) = e
environment(rpt) = e
environment(swirl) = e

assignInNamespace("parse_content.yaml", parse_content.yaml, "swirl") 
assignInNamespace("resume.default", resume.default, "swirl") 
assignInNamespace("testResponse.default", testResponse.default, "swirl") 
assignInNamespace("swirl", swirl, "swirl") 

