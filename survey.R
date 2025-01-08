rm(list = ls())
graphics.off()
# used for data wrangling and prepare data for analysis
install.packages("tidyverse")
# used to import and export files in r (extremely fast package)
install.packages("rio")
# we use only one function from janitor => clean_names to standarize variables names
# in the data frame (ex making all of them lowerCase,remove any spaces,etc...)
install.packages("janitor")
# used to run the logistic regression and get the odds ratio for interpretion
install.packages("finalfit")
# used to make tables in the console of good shape
install.packages("knitr")
#
install.packages("nortest")
#
install.packages("ggcorrplot")
# we use car to use one function vif to calcalute the variance inflation factor 
# to detect collinerity in our model
install.packages("car")
# used function from it "alpha" to calcalute the crohbach"s alpha value
install.packages("psych")
library(tidyverse)
library(rio)
library(janitor)
library(finalfit)
library(knitr)
library(nortest)
library(ggcorrplot)
library(car)
library(psych)

getwd()

# importing the whole data, converting all rows into lowerCase, and turn the no 
# or not sure into just no to prevent overlapping as some cloumns have just no
# and other columns has no or not sure
data <- rio::import(file = "./mars-project-module-1.xlsx") %>% 
  clean_names(.) %>% 
  mutate(across(.cols = everything(),
                .fns = ~ str_to_lower(.))) %>% 
  mutate(across(.cols = 10:30,
                .fns = ~ case_when(. == "no or not sure" ~ "no",
                                   T ~ .)))

# get all knowlede question, convert yes and no into numeric to calculate
# cronbach alpha value
knowledge.questions <- data %>% 
  select(10,12,13,14)  %>% 
  mutate(across(everything(),
                .fns = ~ case_when(. == "yes" ~ 1,
                                   . == "no" ~ 0,
                                   T ~ 0)))

cronbach_knowledge_questions_results <- alpha(knowledge.questions)

summary(cronbach_knowledge_questions_results)
# raw_alpha => 0.41


# get all attitude question, convert yes and no into numeric to calculate
# cronbach alpha value
attitude.questions <- data %>% 
  select(16,17,18,19,21,22,23) %>% 
  mutate(across(everything(),
                .fns = ~ case_when(. == "yes" ~ 1,
                                  . == "no" ~ 0,
                                   T ~ 0)))


cronbach_attitude_questions_results <- alpha(attitude.questions)

summary(cronbach_attitude_questions_results)
# raw_alpha => 0.85


# get all perception question, convert yes and no into numeric to calculate
# cronbach alpha value
perception.questions <- data %>% 
  select(24:30) %>% 
  mutate(across(everything(),
                .fns = ~ case_when(. == "yes" ~ 1,
                                   . == "no" ~ 0,
                                   T ~ 0)))

cronbach_perception_questions_results <- alpha(perception.questions)

summary(cronbach_perception_questions_results)
# raw_alpha => 0.42


# get all question and calculate the cronbach alpha for all question together
cronbach_all_questions <- data %>% 
  select(7,8,9,10,12,13,14,16,17,18,19,21:30) %>% 
  mutate(across(everything(),
                .fns = ~ case_when(. == "yes" ~ 1,
                                   . == "no" ~ 0,
                                   T ~ 0)))


cronbach_all_questions_results <- alpha(cronbach_all_questions)

summary(cronbach_all_questions_results)
# raw_alpha => 0.7

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

domins_levels_percentages <- function(table,title){
  table %>% as.data.frame(.) %>%
    rename(level = 1,
           percentage = 2) %>% 
    ggplot(.,aes(x = level,
                 y = percentage,
                 fill = level)) +
    geom_col() +
    geom_text(aes(label = percentage), vjust = -0.5) +
    ggtitle(title) +
    theme(plot.title = element_text(family = "bold",hjust = 0.5))
}

domins_levels_percentages(knowledge.prob.tables,"Knowledge Percentages")


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

domins_levels_percentages(attitude.prop.table,"Attitude Percentages")


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

domins_levels_percentages(perception.prop.table,"Perception Percentages")


# correlation matrix

total.scores <- tibble(knowledge.scores,attitude.scores,perception.scores) %>% 
  select(total_percentage_knowledge,total_percentage_attitude,total_percentage_perception)

shapiro.test(total.scores$total_percentage_knowledge)
shapiro.test(total.scores$total_percentage_attitude)
shapiro.test(total.scores$total_percentage_perception)

correlation.matrix <- cor(total.scores,method = "spearman")

correlation.matrix.plot <- ggcorrplot(correlation.matrix,method = "square", lab = TRUE,title = "Correlation Matrix")

ggsave(filename = "correlation_matrix.png",
       plot = correlation.matrix.plot,
       width = 29,
       height = 21,
       units = "cm",
       dpi = 600)

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
vif(model_knowledge_high_moderate)

## create a function that takes the data frame name, score_variable_name, level1 and level2
# df ==> knowledge.df,attitude.df,perception.df
# score ==> knowledge_score,attitude_score,perception_score
# level1 and level 2 ==> "high" / "moderate" / "low"

finalfit_results_all_explanatory <- function(df, score, level1, level2) {
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
finalfit_results_all_explanatory(knowledge.df,"knowledge_score","high","low")
finalfit_results_all_explanatory(attitude.df,"attitude_score","high","low")
finalfit_results_all_explanatory(perception.df,"perception_score","high","low")

# this model failed due to co-linerity

finalfit_results_all_explanatory(attitude.df,"attitude_score","high","moderate")

model_attitude_high_moderate <- attitude.df %>% 
  mutate(age = as.numeric(age)) %>% 
  filter(attitude_score %in% c("high","moderate")) %>%
  glm(attitude_score ~ age + sex + marital_status + education_level + job + health_condition, data = .,family = binomial())

summary(model_attitude_high_moderate)
vif(model_attitude_high_moderate)

# exexlude the job coulmn to remove co-linerity but the problem still present maybe due to small sample size (8 observations)
# and run the glm function to show the results as finalfit didn't give any results

model_attitude_high_moderate_without_job <- attitude.df %>% 
  mutate(age = as.numeric(age)) %>% 
  select(-job) %>% 
  filter(attitude_score %in% c("high","moderate")) %>%
  glm(attitude_score ~ age + sex + marital_status + education_level + health_condition, data = .,family = binomial())

summary(model_attitude_high_moderate_without_job)

# Variance Inflation Factor shows that there is severe co-linerity

vif(model_attitude_high_moderate_without_job)

# so we gonna create another function that predict the dependent variable vs only one explantory variable
## create a function that takes the data frame name, explanatory, score_variable_name, level1 and level2
# df ==> knowledge.df,attitude.df,perception.df
# explanatory ==> age,sex,marital_status,education_level,job,health_condition
# score ==> knowledge_score,attitude_score,perception_score
# level1 and level 2 ==> "high" / "moderate" / "low"

finalfit_results_single_explanatory <- function(df,explanatory, score, level1, level2) {
  
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
  
  #Export the results as csv file
  rio::export(x = finalfit_result,
              file = paste0("finalfit_","explanatory_",score,"_",level1,"_",level2,".csv"))
}

finalfit_results_single_explanatory(attitude.df,"job","attitude_score","high","moderate")
