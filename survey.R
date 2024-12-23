rm(list = ls())
graphics.off()
install.packages("knitr")
library(tidyverse)
library(rio)
library(janitor)
library(finalfit)
library(knitr)

getwd()

data <- rio::import(file = "./mars-project-module-1.xlsx") %>% 
  clean_names(.)

knowledge.yes <- data %>% 
  select(10,13) %>% 
  rename(clinical_trial = 1,
         institutional_review_board = 2) %>% 
  mutate(score1 = case_when(clinical_trial == "Yes" ~ 2,
                            T ~ 0)) %>% 
  mutate(score2 = case_when(institutional_review_board == "Yes" ~ 2,
                            T ~ 0))

knowledge.no <- data %>% 
  select(12,14) %>% 
  rename(new_ttt_under_investigation = 1,
         confidential_info = 2) %>% 
  mutate(score3 = case_when(new_ttt_under_investigation == "No" ~ 2,
                            T ~ 0)) %>% 
  mutate(score4 = case_when(confidential_info == "No" ~ 2,
                            T ~ 0))


knowledge.total <- tibble(knowledge.yes,knowledge.no) %>% 
  mutate(total_score_knowledge = score1 + score2 + score3 + score4,
         total_percentage_knowledge = ( total_score_knowledge / 8) * 100,
         bloom_level_knowledge = cut(total_percentage_knowledge,breaks = c(-1,60,80,101),labels = c("Low","Moderate","High")))

knowledge.prob.tables <- table(knowledge.total$bloom_level_knowledge) %>% 
  prop.table(.) * 100

test <- knowledge.total %>% 
  tibble(data$age,.) %>% 
  rename(age = "data$age")

test %>% 
  select(bloom_level_knowledge,age) %>% 
  filter(bloom_level_knowledge %in% c("High","Moderate")) %>% 
  finalfit(explanatory = "age",
           dependent = "bloom_level_knowledge") %>% 
  knitr::kable(.)

test %>% 
  select(bloom_level_knowledge,age) %>% 
  finalfit(explanatory = "age",
           dependent = "bloom_level_knowledge") %>% 
  knitr::kable(.)
