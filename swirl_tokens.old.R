########################################################
# Written by Kyle A. Marrotte
# With tons of assistance by Dr. Garret M. Dancik
# Released under a Creative Commons Attribution-ShareAlike 4.0 
# International license, because education should always be free.
########################################################
library(yaml)
library(swirl)
library(swirlify)

file.template = "./lesson.yaml" #Will later be replaced

tokens.replace <- function(x,y){ #Old tokens.replace function, takes tokens.create and applies it to df
  for (i in 1:length(y)){
    x = lapply(x, function(x) sub(paste0("<",y[[i]][1],">"), y[[i]][2], x))
    }  #lapply required to preserve data.frame
  return(x)
}

tokens.create <- function(.token.str) { #creates tokens based on code from 'token' class
  eval(parse(text = .token.str))   #pulls the code out of the character string and executes it using magic
  a = ls.str()       # creates a vector of tokens in the function environment
  x = list()                      #creates blank list
  for (i in 1:length(a)){
    x[[i]] = c(a[i], get(a[i])) #fills list x with tokens and executed code
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
  
  for(i in 1:length(df)){
    if(!is.na(df$Token[i])){
        tokens <- tokens.create(df$Token[i]) #pulls token input from df and saves it as tokens
        df[i,] <- tokens.replace(df[i,], tokens)#applies tokens to the orignal data frame
    }
    }
  #save(df, file = "yaml.RData")  #Optional, if you want an output file
  meta <- raw_yaml[[1]]
  
  return(df)
}

df.template = parse_content.yaml(file.template) #shortcut to test everything more easily

assignInNamespace("parse_content.yaml", parse_content.yaml, "swirl")