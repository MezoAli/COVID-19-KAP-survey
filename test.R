
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
