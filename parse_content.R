##########################################################
# Added followign to newrow function:
#   'Token' - R code that will generate tokens
#   'NumTimes' - the number of times to repeat the question
#   'TimesRepated' - the number of times the question has
#	already been repeated 
##########################################################

parse_content.yaml <- function(file, e){
  cat("\n\nin parse_content...\n\n")
  # GD: add Token to newrow
  newrow <- function(element){
    temp <- data.frame(Class=NA, NumTimes = 1, TimesRepeated = 0,Token = NA, Output=NA, 
		       CorrectAnswer=NA, AnswerChoices=NA, AnswerTests=NA,
                       Hint=NA, Figure=NA, FigureType=NA,
                       VideoLink=NA, Script=NA)
    for(nm in names(element)){
      # Only replace NA with value if value is not NULL, i.e. instructor
      # provided a nonempty value
      if(!is.null(element[[nm]])) {
        temp[,nm] <- element[[nm]]
      }
    }
    temp
  }
  cat("loading file...\n")
  raw_yaml <- yaml.load_file(file)
  cat("done...\n")
  temp <- lapply(raw_yaml[-1], newrow)
  cat("done lapply\n")
  df <- NULL
  for(row in temp){
    df <- rbind(df, row)
  }
  save(df, file = "df.RData")
  meta <- raw_yaml[[1]]
  cat("===returning from parse_content.yaml===\n")
  lesson(df, lesson_name=meta$Lesson, course_name=meta$Course,
         author=meta$Author, type=meta$Type, organization=meta$Organization,
         version=meta$Version, partner=meta$Partner)
}

