
test <- knowledge.scores %>% 
  tibble(data$age,data$sex,data$marital_status,.) %>% 
  rename(age = "data$age",
         sex = "data$sex",
         marital_status = "data$marital_status")

test %>% 
  select(bloom_level_knowledge,age,marital_status,sex) %>% 
  filter(bloom_level_knowledge %in% c("High","Moderate")) %>% 
  finalfit(explanatory = c("age","sex","marital_status"),
           dependent = "bloom_level_knowledge") %>% 
  knitr::kable(.)

test1 <- test %>% 
  select(bloom_level_knowledge,age) %>% 
  finalfit(explanatory = "age",
           dependent = "bloom_level_knowledge") %>% 
  knitr::kable(.)

# to get questions in the console
data %>% 
  colnames(.)

data %>% 
  select(health_condition) %>% 
  n_distinct()


test.df <- data %>% 
  mutate(across(.cols = 10:30,
                .fns = ~ case_when(. == "No or not sure" ~ "No",
                                 T ~ .)))

test.df %>% 
  filter(if_any(.cols = 10:30,
                .fns = is.character))


nor <- rnorm(100)
shapiro.test(nor)


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
  finalfit_result %>% knitr::kable()
  
  # Export the results as csv file
  rio::export(x = finalfit_result,
              file = paste0("finalfit_",score,"_",level1,"_",level2,".csv"))
}

# Example usage:  # Specify explanatory variables
finalfit_results(knowledge.df, "knowledge_score", "high", "moderate") 
  
  
  
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
domins_levels_percentages(attitude.prop.table,"Attitude Percentage")
  