rm(list = ls())
graphics.off()
install.packages("tidyverse")
install.packages("rio")
install.packages("janitor")
install.packages("finalfit")
install.packages("knitr")
library(tidyverse)
library(rio)
library(janitor)
library(finalfit)
library(knitr)

getwd()

data <- rio::import(file = "./mars-project-module-1.xlsx") %>% 
  clean_names(.)

demographic.df <- data %>% 
  select(1:6) %>% 
  mutate(across(.cols = 2:6,
                .fns = as_factor))

knowledge.yes <- data %>% 
  select(10,13) %>% 
  mutate(across(.cols = everything(),
                .fns = ~ case_when(. == "Yes" ~ 2,
                                   T ~ 0),
                .names = "score_{col}"))

knowledge.no <- data %>% 
  select(12,14) %>% 
  mutate(across(.cols = everything(),
                .fns = ~ case_when(. == "No" ~ 2,
                                   T ~ 0),
                .names = "score_{col}"))


knowledge.total <- tibble(knowledge.yes,knowledge.no) %>%
  select(contains("score")) %>% 
  mutate(total_score_knowledge = rowSums(across(everything())),
         total_percentage_knowledge = ( total_score_knowledge / 8) * 100,
         bloom_level_knowledge = cut(total_percentage_knowledge,breaks = c(-1,60,80,101),labels = c("Low","Moderate","High")))

knowledge.prob.tables <- table(knowledge.total$bloom_level_knowledge) %>% 
  prop.table(.) * 100


attitude.yes <- data %>% 
  select(16,17,18,19,21,22,23) %>% 
  mutate(across(.cols = 1:7,
                .fns = ~ case_when(. == "Yes" ~ 2,
                                   T ~ 0),
                .names = "score_{col}"))

attitude.scores <- attitude.yes %>% 
  select(contains("score")) %>% 
  mutate(total_score_attitude =  rowSums(across(1:7)),
         total_percentage_attitude = round((total_score_attitude / 14) * 100,1),
         bloom_level_attitude = cut(total_percentage_attitude,breaks = c(-1,60,80,101),labels = c("Low","Moderate","High")))

attitude.prop.table <- table(attitude.scores$bloom_level_attitude) %>% 
  prop.table(.) * 100
