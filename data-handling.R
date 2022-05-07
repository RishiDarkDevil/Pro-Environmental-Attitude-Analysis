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
rev_order <- c("SS1", "PC2", "PC3")

for (rev_col in rev_order) {
  data_opinion[[rev_col]] <- fct_rev(data_opinion[[rev_col]])
}

data_opinion <- data_opinion %>%
  select(matches("^REC."), matches("^SA."), matches("^PC."), matches("^SS."), matches("^ER."), matches("^ES."), matches("^RES."), matches("^CON."))
data_opinion

# Converting personal info to factors
data_personal <- data[,1:2] %>%
  map_df(~factor(.))

# Putting all together
data <- as_tibble(cbind(data_personal, data_opinion))
data

# Encoded Data
data_enc <- data %>%
  map_df(~as.numeric(.))
data_enc

# Net Score Data for each category
data_net_score <- data_enc %>%
  transmute(
    Sex = Sex,
    Family = Family,
    REC = (REC1+REC2+REC3+REC4+REC5+REC6+REC7)*100/35,
    SA = (SA1+SA2+SA3+SA4+SA5+SA6)*100/30,
    PC = (PC1+PC2+PC3+PC4+PC5+PC6)*100/30,
    SS = (SS1+SS2+SS3+SS4+SS5)*100/25,
    ER = (ER1+ER2+ER3+ER4+ER5)*100/25,
    ES = (ES1+ES2+ES3+ES4+ES5)*100/25,
    RES = (RES1+RES2+RES3+RES4+RES5)*100/25,
    CON = (CON1+CON2+CON3+CON4+CON5)*100/25
  )
data_net_score

# Total Score
data_tot_score <- data_enc %>%
  mutate(Score = (REC1+REC2+REC3+REC4+REC5+REC6+REC7+SA1+SA2+SA3+SA4+SA5+SA6+PC1+PC2+PC3+PC4+PC5+PC6+SS1+SS2+SS3+SS4+SS5+ER1+ER2+ER3+ER4+ER5+ES1+ES2+ES3+ES4+ES5+RES1+RES2+RES3+RES4+RES5+CON1+CON2+CON3+CON4+CON5)*100/220) %>%
  select(Sex, Family, Score)
data_tot_score
