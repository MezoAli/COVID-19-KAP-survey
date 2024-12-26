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
  mutate(score1 = case_when(pull(.,1) == "Yes" ~ 2,
                            T ~ 0)) %>% 
  mutate(score2 = case_when(pull(.,2) == "Yes" ~ 2,
                            T ~ 0))

knowledge.no <- data %>% 
  select(12,14) %>% 
  mutate(score3 = case_when(pull(.,1) == "No" ~ 2,
                            T ~ 0)) %>% 
  mutate(score4 = case_when(pull(.,2) == "No" ~ 2,
                            T ~ 0))


knowledge.total <- tibble(knowledge.yes,knowledge.no) %>% 
  mutate(total_score_knowledge = score1 + score2 + score3 + score4,
         total_percentage_knowledge = ( total_score_knowledge / 8) * 100,
         bloom_level_knowledge = cut(total_percentage_knowledge,breaks = c(-1,60,80,101),labels = c("Low","Moderate","High")))

knowledge.prob.tables <- table(knowledge.total$bloom_level_knowledge) %>% 
  prop.table(.) * 100
