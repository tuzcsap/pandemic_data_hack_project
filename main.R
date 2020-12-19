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

# образование -> объединить высшие
unemp1 %>%
  filter(hc_singleparent) %>% 
  count(education, sort = TRUE)

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

unemp1 %>% 
  filter(hc_singleparent) %>% 
  count(age, sort = TRUE)

# среди всех
unemp1 %>% 
  count(age, sort = TRUE)

unemp1 %>% 
  filter(hc_singleparent) %>% 
  count(gender, sort = TRUE)

# среди всех
unemp1 %>% 
  count(gender, sort = TRUE)

###

unemp1 %>%
  filter(hc_largefam) %>% 
  count(region, sort = TRUE)

unemp1 %>%
  filter(hc_largefam) %>% 
  count(gender, sort = TRUE)

