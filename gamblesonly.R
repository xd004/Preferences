#install.packages('qualtRics')
library(qualtRics)
library(tidyverse)
source("./vpcR.R")
library(lme4)
library(kableExtra)

# 40 - 81, gamble 1
# 1 - 39, 4000, 4100, 4200, gamble 2

data <- df %>% 
  select(`ResponseId`, starts_with("Q")) %>% 
  select(-c(2,3,4,5,6,7,8,9,"Q25","Q27","Q28","Q26_1...119","Q197","Q272_11","Q273_11","Q274_11","Q84_11","Q185_11","Q186_11"))%>%
  pivot_longer(
    cols = -ResponseId,
    names_to = "question",
    values_to = "response"
  )%>%
  mutate(num = parse_number(question))%>%
  mutate(block = case_when(
    between(num, 40, 81) ~ "1",
    between(num, 1, 4200) ~ "2"
  ))%>%
  mutate(participant = `ResponseId`)


#install.packages('brms')
library(brms)

fit1_brms <- brm(
  response ~ 1 + (1|participant) + (1|block) + (1|participant:block),
  data = data, 
  cores = 4, # May need to change depending on your computer
  iter = 2000
)

summary(fit1_brms)
VarCorr(fit1_brms)