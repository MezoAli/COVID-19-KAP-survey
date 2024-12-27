rm(list = ls())
graphics.off()
install.packages("tidyverse")
install.packages("rio")
install.packages("janitor")
install.packages("finalfit")
install.packages("knitr")
install.packages("nortest")
library(tidyverse)
library(rio)
library(janitor)
library(finalfit)
library(knitr)
library(nortest)

getwd()

data <- rio::import(file = "./mars-project-module-1.xlsx") %>% 
  clean_names(.) %>% 
  mutate(across(.cols = everything(),
                .fns = ~ str_to_lower(.))) %>% 
  mutate(across(.cols = 10:30,
                .fns = ~ case_when(. == "no or not sure" ~ "no",
                                   T ~ .)))

demographic.df <- data %>% 
  select(1:6) %>% 
  mutate(across(.cols = 2:6,
                .fns = as_factor))

knowledge.yes <- data %>% 
  select(10,13) %>% 
  mutate(across(.cols = everything(),
                .fns = ~ case_when(. == "yes" ~ 2,
                                   T ~ 0),
                .names = "score_{col}"))

knowledge.no <- data %>% 
  select(12,14) %>% 
  mutate(across(.cols = everything(),
                .fns = ~ case_when(. == "no" ~ 2,
                                   T ~ 0),
                .names = "score_{col}"))


knowledge.scores <- tibble(knowledge.yes,knowledge.no) %>%
  select(contains("score")) %>% 
  mutate(total_score_knowledge = rowSums(across(everything())),
         total_percentage_knowledge = ( total_score_knowledge / 8) * 100,
         bloom_level_knowledge = cut(total_percentage_knowledge,breaks = c(-1,60,80,101),labels = c("low","moderate","high")))

knowledge.prob.tables <- table(knowledge.scores$bloom_level_knowledge) %>% 
  prop.table(.) * 100


attitude.yes <- data %>% 
  select(16,17,18,19,21,22,23) %>% 
  mutate(across(.cols = 1:7,
                .fns = ~ case_when(. == "yes" ~ 2,
                                   T ~ 0),
                .names = "score_{col}"))

attitude.scores <- attitude.yes %>% 
  select(contains("score")) %>% 
  mutate(total_score_attitude =  rowSums(across(1:7)),
         total_percentage_attitude = round((total_score_attitude / 14) * 100,1),
         bloom_level_attitude = cut(total_percentage_attitude,breaks = c(-1,60,80,101),labels = c("low","moderate","high")))

attitude.prop.table <- table(attitude.scores$bloom_level_attitude) %>% 
  prop.table(.) * 100


perception.yes <- data %>% 
  select(24,25,26,29,30) %>% 
  mutate(across(.cols = everything(),
                .fns = ~ case_when(. == "yes" ~ 2,
                                   T ~ 0),
                .names = "score_{col}"))


perception.no <- data %>% 
  select(27,28) %>% 
  mutate(across(.cols = everything(),
                .fns = ~ case_when(. == "no" ~ 2,
                                   T ~ 0),
                .names = "score_{col}"))


perception.scores <- tibble(perception.yes,perception.no) %>% 
  select(contains("score")) %>% 
  mutate(total_score_perception =  rowSums(across(1:7)),
         total_percentage_perception = round((total_score_perception / 14) * 100,1),
         bloom_level_perception = cut(total_percentage_perception,breaks = c(-1,60,80,101),labels = c("low","moderate","high")))

perception.prop.table <- table(perception.scores$bloom_level_perception) %>% 
  prop.table(.) * 100


# correlation matrix

total.scores <- tibble(knowledge.scores,attitude.scores,perception.scores) %>% 
  select(total_percentage_knowledge,total_percentage_attitude,total_percentage_perception)

shapiro.test(total.scores$total_percentage_knowledge)
shapiro.test(total.scores$total_percentage_attitude)
shapiro.test(total.scores$total_percentage_perception)

correlation.matrix <- cor(total.scores,method = "spearman")

## regression analysis to predict the outcomes based on basic info

# create data frames that contain the basic info plus the bloom level score

knowledge.df <- tibble(demographic.df,knowledge_score = knowledge.scores$bloom_level_knowledge)
attitude.df <- tibble(demographic.df,attitude_score = attitude.scores$bloom_level_attitude)
perception.df <- tibble(demographic.df,perception_score = perception.scores$bloom_level_perception)


# create explanatory vector that contain names of basic info
explanatory <- demographic.df %>% 
  colnames(.)

# finalfit knowledge high:moderate

knowledge_high_moderate <- knowledge.df %>% 
  mutate(age = as.numeric(age)) %>%
  filter(knowledge_score %in% c("high","moderate")) %>% 
  finalfit(dependent = "knowledge_score",
           explanatory = explanatory)

# show it in a better format

knowledge_high_moderate %>%  knitr::kable(.)

# export the file as csv file
rio::export(x = knowledge_high_moderate,
            file = "regression_finalfit_knowledge_high_moderate.csv")

# regression analysis knowledge high:moderate (just to make sure of finalfit results)

model_knowledge_high_moderate <- knowledge.df %>% 
  mutate(age = as.numeric(age)) %>% 
  filter(knowledge_score %in% c("high","moderate")) %>%
  glm(knowledge_score ~ age + sex + marital_status + education_level + job + health_condition, data = .,family = binomial())

summary(model_knowledge_high_moderate)

## create a function that takes the data frame name, score_variable_name, level1 and level2
# df ==> knowledge.df,attitude.df,perception.df
# score ==> knowledge_score,attitude_score,perception_score
# level1 and level 2 ==> "high" / "moderate" / "low"

finalfit_results <- function(df, score, level1, level2) {
  explanatory <- demographic.df %>% 
    colnames(.)
  # Ensure score is numeric
  df <- df %>% mutate(age = as.numeric(age))
  
  # Filter rows based on the levels of the score
  filtered_df <- df %>%
    filter(get(score) %in% c(level1, level2)) 
  
  # Run finalfit on filtered data
  finalfit_result <- filtered_df %>%
    finalfit(dependent = score,
             explanatory = explanatory) 
  
  # Return the table with knitr::kable to see the results in the console
  print(knitr::kable(finalfit_result))
  
  # Export the results as csv file
  rio::export(x = finalfit_result,
              file = paste0("finalfit_",score,"_",level1,"_",level2,".csv"))
}

# running an example like for attitude.df , attitude_score , "high" , "moderate"
finalfit_results(attitude.df,"attitude_score","high","low")
finalfit_results(knowledge.df,"knowledge_score","high","low")
finalfit_results(perception.df,"perception_score","high","low")


