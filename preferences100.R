#install.packages('qualtRics')
library(qualtRics)
library(tidyverse)
source("./vpcR.R")
library(lme4)

df <- qualtRics::read_survey("./gamblesver2100.csv")

# 40 - 81, gamble 1
# 1 - 39, 4000, 4100, 4200, gamble 2
# 149 - 190, face 1
# 403 - 444, face 2
data <- df %>% 
  select(`ResponseId`, starts_with("Q")) %>% 
  select(-c(2,3,4,5,6,7,8,9,"Q25","Q26_1...203","Q27","Q28","Q197","Q272_11","Q273_11","Q274_11","Q84_11","Q185_11","Q186_11"))%>%
  pivot_longer(
    cols = -ResponseId,
    names_to = "question",
    values_to = "response"
  )%>%
  mutate(num = parse_number(question))%>%
  mutate(type = case_when(
    between(num, 40, 81) ~ "gamble",
    between(num, 149, 190) ~ "face",
    between(num, 403, 444) ~ "face",
    between(num, 1, 4200) ~ "gamble",
  ))%>%
  mutate(block = case_when(
    between(num, 40, 81) ~ "1",
    between(num, 149, 190) ~ "1",
    between(num, 403, 444) ~ "2",
    between(num, 1, 4200) ~ "2",
  ))%>%
  mutate(participant = `ResponseId`)

fit1 <- lme4::lmer(
  response ~ 1 + (1|participant) + (1|type) + (1|participant:block) + (1|participant:type),
  data = data,
  control = lmerControl(optimizer = 'bobyqa'),
  REML = TRUE
)

summary(fit1)

# vcov is the VC (variance component), sdcor is square root of vcov, grp is the specific cluster/random effect
vars <- data.frame(lme4::VarCorr(fit1))

# this calculates VPC (variance partitioning coefficient) which divides each vcov by the total variance = proportion of total variance
vars$vpc <- vars$vcov/sum(vars$vcov)

randeff <- lme4::ranef(fit1)

vpc1 <- compute_vpc(
  data,
  glmer = FALSE,
  dv = "response",
  idyo_ivs = c("participant", "participant:type"),
  shared_ivs = c("type"),
  additional_ivs = c("participant:block")
)

vpc1

