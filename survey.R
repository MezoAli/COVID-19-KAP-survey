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
# to display the heat map
install.packages("ggcorrplot")
# we use car to use one function vif to calcalute the variance inflation factor 
# to detect collinerity in our model
install.packages("car")
# used function from it "alpha" to calcalute the crohbach"s alpha value
install.packages("psych")
# used to get the function Cstat to calculate the c-statstics or area under the ROC cureve
install.packages("DescTools")
# used to get the hoslem.test to detect the goodness of fit of the model
install.packages("ResourceSelection")
library(tidyverse)
library(rio)
library(janitor)
library(finalfit)
library(knitr)
library(nortest)
library(ggcorrplot)
library(car)
library(psych)
library(Hmisc)
library(gmodels)
library(DescTools)
library(ResourceSelection)
getwd()
### Data Wrangling


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

# function describe from package Hmisc to summarize and tabulate the data
# to get to know the data and from the data provided it's better to execlude the
# health condition as most of the values is on nothing and other categories
# have very small number of observation
describe(data)

# function from gmodels package that display the tables and proportions together
# like table and prop.table together
CrossTable(data$health_condition)


# show the number in each category and select high number category as reference
# for robust analysis
table(demographic.df$sex)
table(demographic.df$marital_status)
table(demographic.df$education_level)
table(demographic.df$job)

# get the demographic variables which will be used later for prediction
demographic.df <- data %>% 
  select(1:5) %>% 
  mutate(across(.cols = 2:5,
                .fns = as_factor)) %>% 
  mutate(sex = relevel(sex,ref = "female"),
         marital_status = relevel(marital_status,ref = "married"),
         education_level = relevel(education_level,ref = "bachelor"),
         job = relevel(job,ref = "healthcare professional"))


# to make sure that the assigned category is the reference one
contrasts(demographic.df$sex)
contrasts(demographic.df$marital_status)
contrasts(demographic.df$education_level)
contrasts(demographic.df$job)


# get the knowledge question that ideal answer is yes and 
# create new columns that have the grade for particular question
knowledge.yes <- data %>% 
  select(10,13) %>% 
  mutate(across(.cols = everything(),
                .fns = ~ case_when(. == "yes" ~ 2,
                                   T ~ 0),
                .names = "score_{col}"))


# get the knowledge question that ideal answer is no and 
# create new columns that have the grade for particular question
knowledge.no <- data %>% 
  select(12,14) %>% 
  mutate(across(.cols = everything(),
                .fns = ~ case_when(. == "no" ~ 2,
                                   T ~ 0),
                .names = "score_{col}"))


# merge the knowledge coulmns, get the columns that contain score and
# calculate total scores, percentages and bloom level
knowledge.scores <- tibble(knowledge.yes,knowledge.no) %>%
  select(contains("score")) %>% 
  mutate(total_score_knowledge = rowSums(across(everything())),
         total_percentage_knowledge = ( total_score_knowledge / 8) * 100,
         bloom_level_knowledge = cut(total_percentage_knowledge,breaks = c(-1,60,80,101),labels = c("low","moderate","high")))


# to get the percentage of each category in the knowledge section
knowledge.prob.tables <- table(knowledge.scores$bloom_level_knowledge) %>% 
  prop.table(.) * 100


# function to plot the percentage of each category in selected domin
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


# get the attitude question that ideal answer is yes and 
# create new columns that have the grade for particular question
attitude.yes <- data %>% 
  select(16,17,18,19,21,22,23) %>% 
  mutate(across(.cols = 1:7,
                .fns = ~ case_when(. == "yes" ~ 2,
                                   T ~ 0),
                .names = "score_{col}"))


# get the scores for attitude but we didn't merge anything here as the attitude
# domin has no indeal answers of no
attitude.scores <- attitude.yes %>% 
  select(contains("score")) %>% 
  mutate(total_score_attitude =  rowSums(across(1:7)),
         total_percentage_attitude = round((total_score_attitude / 14) * 100,1),
         bloom_level_attitude = cut(total_percentage_attitude,breaks = c(-1,60,80,101),labels = c("low","moderate","high")))

attitude.prop.table <- table(attitude.scores$bloom_level_attitude) %>% 
  prop.table(.) * 100

domins_levels_percentages(attitude.prop.table,"Attitude Percentages")


# get the perception questions that ideal answer is yes and 
# create new columns that have the grade for particular question
perception.yes <- data %>% 
  select(24,25,26,29,30) %>% 
  mutate(across(.cols = everything(),
                .fns = ~ case_when(. == "yes" ~ 2,
                                   T ~ 0),
                .names = "score_{col}"))


# get the perception questions that ideal answer is no and 
# create new columns that have the grade for particular question
perception.no <- data %>% 
  select(27,28) %>% 
  mutate(across(.cols = everything(),
                .fns = ~ case_when(. == "no" ~ 2,
                                   T ~ 0),
                .names = "score_{col}"))

# merge the perception coulmns, get the columns that contain score and
# calculate total scores, percentages and bloom level
perception.scores <- tibble(perception.yes,perception.no) %>% 
  select(contains("score")) %>% 
  mutate(total_score_perception =  rowSums(across(1:7)),
         total_percentage_perception = round((total_score_perception / 14) * 100,1),
         bloom_level_perception = cut(total_percentage_perception,breaks = c(-1,60,80,101),labels = c("low","moderate","high")))

perception.prop.table <- table(perception.scores$bloom_level_perception) %>% 
  prop.table(.) * 100

domins_levels_percentages(perception.prop.table,"Perception Percentages")


### correlation matrix

# create data frame that contains only scores (numeric values) to get the
# correlation between domins
total.scores <- tibble(knowledge.scores,attitude.scores,perception.scores) %>% 
  select(total_percentage_knowledge,total_percentage_attitude,total_percentage_perception)


# shapiro test used to test if the data follows normal distribution or not
# null hypothesis => the data follow normal distribution
# so we are interested in p-values to know wether the data is normally distributed or not
shapiro.test(total.scores$total_percentage_knowledge)
shapiro.test(total.scores$total_percentage_attitude)
shapiro.test(total.scores$total_percentage_perception)


# (Histogram ) another measure to see the distribution of data but not very useful in that case
hist(total.scores$total_percentage_knowledge)
hist(total.scores$total_percentage_attitude)
hist(total.scores$total_percentage_perception)


# (Density plot ) yet another measure to see the distribution of data
plot(density(total.scores$total_percentage_knowledge))
plot(density(total.scores$total_percentage_attitude))
plot(density(total.scores$total_percentage_perception))


# from the results obtained from shapiro test, Histogram and Density plot we
# can concolude that the data is NOT normally dist. and so we go for spearman rank's
# correlation insted of Pearson's correlation
correlation.matrix <- cor(total.scores,method = "spearman")


# heat map to show the relation between different domins
correlation.matrix.plot <- ggcorrplot(correlation.matrix,method = "square", lab = TRUE,title = "Correlation Matrix")

# used to export the graph to be included in the final outcome
ggsave(filename = "correlation_matrix.png",
       plot = correlation.matrix.plot,
       width = 29,
       height = 21,
       units = "cm",
       dpi = 600)

### regression analysis to predict the outcomes based on basic info

# create data frames that contain the basic info plus the bloom level score 
# for each domin

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
  mutate(knowledge_score = droplevels(knowledge_score)) %>% 
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
  mutate(knowledge_score = droplevels(knowledge_score)) %>%
  mutate(knowledge_score = relevel(knowledge_score,ref = "moderate")) %>% 
  glm(knowledge_score ~ age + sex + marital_status + education_level + job, data = .,family = binomial())


# generate null model to be later compared to the proposed model 
# to get the MCFadden or pseudo R^2 value
null_model_knowledge_high_moderate <- knowledge.df %>% 
  mutate(age = as.numeric(age)) %>% 
  filter(knowledge_score %in% c("high","moderate")) %>%
  mutate(knowledge_score = droplevels(knowledge_score)) %>%
  mutate(knowledge_score = relevel(knowledge_score,ref = "moderate")) %>% 
  glm(knowledge_score ~ 1,family = binomial,data = .)


# to show the log odds of the coefficents, standard errors, p-values for each one, null and residual deviance and AIC
summary(model_knowledge_high_moderate)

# to show and odds ratio for each coeficeient
exp(model_knowledge_high_moderate$coefficients)

# to show the effect of each predictors on the deviance, it's p-value
anova(model_knowledge_high_moderate,method = "Chisq")

# to detect any collinerity between variables that could cause problem the model
vif(model_knowledge_high_moderate)

# calculate the McFadden r2 value => to measure predictive power of the model
# it's value is almost 17 which idicate good predictive power but not so good
r2_knowledge_high_moderate <- 1 - logLik(model_knowledge_high_moderate) / logLik(null_model_knowledge_high_moderate)
print(r2_knowledge_high_moderate)


# calculate the c-statsics value => to also measure predictive power of the model
# it's value is almost 0.8 which is almost excellent predictive power
Cstat(model_knowledge_high_moderate)

# create the Hosmer Lemeshow test to detect the goddness of fit
# yields p-value of 0.5165 so fail to reject the null hypothesis (The observed data fits the model well)
# so accept the accept it and the data fits the model well
hoslem.test(x = model_knowledge_high_moderate$y,
            y = fitted(model_knowledge_high_moderate),
            g = 10)

# all above steps whould be followed to get the best results and select models based
# on the best results and AIC


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

# this model failed due to very small sample size (only 8 observations) and
# some predictors here have extremely collinerity

finalfit_results_all_explanatory(attitude.df,"attitude_score","high","moderate")

model_attitude_high_moderate <- attitude.df %>% 
  mutate(age = as.numeric(age)) %>% 
  filter(attitude_score %in% c("high","moderate")) %>%
  glm(attitude_score ~ age + sex + marital_status + education_level + job , data = .,family = binomial())

# this also shown here in the summary
summary(model_attitude_high_moderate)
vif(model_attitude_high_moderate)

# exexlude the job coulmn to remove co-linerity but the problem still present maybe due to small sample size (8 observations)
# and run the glm function to show the results as finalfit didn't give any results

model_attitude_high_moderate_without_job <- attitude.df %>% 
  mutate(age = as.numeric(age)) %>% 
  select(-job) %>% 
  filter(attitude_score %in% c("high","moderate")) %>%
  glm(attitude_score ~ age + sex + marital_status + education_level, data = .,family = binomial())

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

# as the results listed very little observation leads to serious problems
finalfit_results_single_explanatory(attitude.df,"job","attitude_score","high","moderate")


