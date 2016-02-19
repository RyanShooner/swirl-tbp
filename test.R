source("swirl_tokens.R")

df = parse_content.yaml("template.yaml", NULL, TRUE)

row = df[4,]
cat("with tokens, row is: \n")
print(row)

if (is.na(row$Token)) {
	stop ("selected row does not have token!\n")
}

tokens <- tokens.create(as.character(row$Token), NULL)  #create the tokens
row <- tokens.replace(row, tokens)  #then replace the tokens
cat("with values, row is: \n")
print(row)

