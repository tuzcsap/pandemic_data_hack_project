library(tidyverse)

unemp1 <- read_delim("/Users/mikhail/Desktop/Pandemic Data Hack/unemployed_1/unemployed_1_data.csv", delim=";")
#sample <- read_delim("/Users/mikhail/Desktop/Pandemic Data Hack/unemployed_1/unemployed_1_sample.csv", delim=";")

unemp1 %>% count(hc_singleparent)

unemp1 %>% 
  filter(hc_didnotwork) %>% 
  count(hc_singleparent)

unemp1 %>%
  filter(hc_singleparent) %>% 
  count(region, sort = TRUE)

unemp1 <- unemp1 %>% 
  #mutate(education_filtered = str_replace_all(education,"Высшее", "Высшее")) %>% 
  mutate(education_filtered = ifelse(str_detect(education, "Высшее"), "Высшее образование", education))

# образование для одиночек
unemp1 %>%
  filter(hc_singleparent) %>% 
  #filter(age==1) %>% 
  count(education_filtered, sort = TRUE)

# образование для всех
unemp1 %>% 
  count(education_filtered, sort = TRUE)

# возрастные группы с начальным образованием
unemp1 %>%
  filter(hc_singleparent) %>% 
  filter(education_filtered=="Начальное общее образование") %>% 
  count(age, sort = TRUE)

# образование по полу у одиночек
unemp1 %>%
  filter(gender=="Мужской") %>% 
  filter(hc_singleparent) %>% 
  count(education_filtered, sort = TRUE)

unemp1 %>%
  filter(gender=="Женский") %>% 
  filter(hc_singleparent) %>% 
  count(education_filtered, sort = TRUE)


# расторжение
unemp1 %>%
  filter(hc_singleparent) %>% 
  count(dismissal_reason, sort = TRUE)

# профессия по последнему месту работы
unemp1 %>%
  filter(hc_singleparent) %>% 
  count(profession_last_work, sort = TRUE)

# профессия по образованию
unemp1 %>%
  filter(hc_singleparent) %>% 
  count(profession_last_educ, sort = TRUE)

# стаж в этом году
unemp1 %>%
  filter(hc_singleparent) %>% 
  count(experience, sort = TRUE)

unemp1 %>%
  filter(hc_singleparent) %>% 
  count(district, sort = TRUE)
# ВОЗРАСТ
unemp1 %>% 
  filter(hc_singleparent) %>% 
  count(age, sort = TRUE)

# среди всех
unemp1 %>% 
  count(age, sort = TRUE)


### ПОЛ
unemp1 %>% 
  filter(hc_singleparent) %>% 
  count(gender, sort = TRUE)

# среди всех
unemp1 %>% 
  count(gender, sort = TRUE)


###



