library(tidyverse)
library(forcats)
library(stringr)

# Importing Data
data <- read_csv("PEAData.csv")

# Removing unnecessary features
data <- data %>%
  select(-c(Education, Major, Institution))
data

# Converting the columns except reverse scored ones to factors
levels <- c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")

data_opinion <- data[,3:ncol(data)] %>%
  map_df(~factor(.,levels = levels))

# Converting the columns of reverse scored ones to factors(i.e. change the level ordering of them)
rev_order <- c("REC7", "SA6", "PC6", "SS5", "ER5", "ES5", "RES5", "CON5")

for (rev_col in rev_order) {
  data_opinion[[rev_col]] <- fct_rev(data_opinion[[rev_col]])
}

# Converting personal info to factors
data_personal <- data[,1:2] %>%
  map_df(~factor(.))

# Putting all together
data[,1:2] <- data_personal
data[,3:ncol(data)] <- data_opinion
data

# Encoded Data
data_enc <- data %>%
  map_df(~as.numeric(.))
data_enc
