library(yaml)

##############################################################
# Assignment: 
#   (1) modify the newrow function to handle a Token row.
#       once you do this, calling the parse_content.yaml 
#       function should correctly call a data.frame where
#       each row is a different class and each column is
#       a different characteristic of that class, which includes
#       'Token'
#   (2) Write a function that takes a single argument, which
#       is the data.frame returned by parse_content.yaml. This
#       function uses the functions you have developed previously
#       to get the tokens from Token (if present), replaces
#       the token values in all other columns, and returns
#       the updated data.frame. Note: a concise way to do this
#       is to first write a function that takes a single row
#       as an argument, and updates the tokens. Then you can
#       use the apply function to apply this to each row of 
#       the parse_content data.frame
##############################################################

tokens.replace <- function(x,y){
  
  for (i in 1:length(y)){
    x = lapply(x, function(x) sub(paste0("<",y[[i]][1],">"), y[[i]][2], x))}

  return(x)
}

tokens.create <- function(.token.str) { #token.str will be code to input, output.to will be the value to apply token.replace to
  eval(parse(text = .token.str))   #pulls the code out of the character string and executes it using magic
  a = ls.str()       # creates a vector of tokens in the function environment
  x = list()                      #creates blank list
  for (i in 1:length(a)){
    x[[i]] = c(a[i], get(a[i]))
  }
  return(x)
}

newrow <- function(element){
  temp <- data.frame(Class=NA, Token=NA, Output=NA, CorrectAnswer=NA,
                     AnswerChoices=NA, AnswerTests=NA,
                     Hint=NA, Figure=NA, FigureType=NA,
                     VideoLink=NA, Script=NA)
  
  for(nm in names(element)){
    # Only replace NA with value if value is not NULL, i.e. instructor
    # provided a nonempty value
    if(!is.null(element[[nm]])) {
      temp[,nm] <- element[[nm]]
    }
  }
  return(temp)
}

parse_content.yaml <- function(file, e){
  raw_yaml <- yaml.load_file(file)
  temp <- lapply(raw_yaml[-1], newrow)
  df <- NULL
  for(row in temp){
    df <- rbind(df, row)
  }
  tokens <- tokens.create(df$Token)
  df <- tokens.replace(df, tokens)
  #save(df, file = "yaml.RData")  #Optional, if you want an output file
  meta <- raw_yaml[[1]]

  return(df)
  
  # original swirl function returns a 'lesson'  
  #  lesson(df, lesson_name=meta$Lesson, course_name=meta$Course,
  #         author=meta$Author, type=meta$Type, organization=meta$Organization,
  #         version=meta$Version, partner=meta$Partner)
  
  
}

#Extras
{
# change these to reflect the full path of the yaml file
file.lesson = "C:/Users/shado/Google Drive/School/R Independent Study/swirl_templates/lesson.yaml"
file.template = "C:/Users/shado/Google Drive/School/R Independent Study/swirl_templates/template.yaml"


#library(yaml)
#raw_data = yaml.load_file(file.template)

## convert 5th class to a 'new row' 
#newrow(raw_data[[5]])

#df.lesson = parse_content.yaml(file.lesson)

# Token causes an error; newrow must be fixed
df.template = parse_content.yaml(file.template)
}