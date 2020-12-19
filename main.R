library(tidyverse)

unemp1 <- read_delim("/Users/mikhail/Desktop/Pandemic Data Hack/unemployed_1/unemployed_1_data.csv", delim=";")
#sample <- read_delim("/Users/mikhail/Desktop/Pandemic Data Hack/unemployed_1/unemployed_1_sample.csv", delim=";")

unemp1 %>% count(hc_singleparent)

# одинокие, не работали, их образование
unemp1 %>% 
  filter(hc_didnotwork) %>% 
  filter(hc_singleparent) %>% 
  count(education_filtered, sort = TRUE)

unemp1 %>% 
  filter(hc_predpens) %>% 
  count(hc_singleparent)
  

## Регион
#одинокие
unemp1 %>%
  filter(hc_singleparent) %>% 
  count(region, sort = TRUE)
# all
unemp1 %>%
  count(region, sort = TRUE)

# Округа
unemp1 %>%
  filter(hc_singleparent) %>% 
  count(district, sort = TRUE)
# all
unemp1 %>%
  count(district, sort = TRUE)


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
# и для всех
unemp1 %>%
  count(profession_last_work, sort = TRUE)

unemp1 %>%
  filter(hc_singleparent) %>% 
  filter(gender=="Мужской") %>% 
  count(profession_last_work, sort = TRUE)

unemp1 %>%
  filter(hc_singleparent) %>% 
  filter(gender=="Женский") %>% 
  count(profession_last_work, sort = TRUE)

# зп по профессии
# unemp1 %>%
#   filter(hc_singleparent) %>% 
#   filter(gender=="Женский") %>% 
#   #filter(!is.na(salary_average)) %>% 
#   group_by(profession_last_work) %>% 
#   count(salary_average, count = TRUE)
#   #summarise(mean(salary_average, na.rm = TRUE))


# профессия по образованию
unemp1 %>%
  filter(hc_singleparent) %>% 
  count(profession_last_educ, sort = TRUE)

# пиздец
unemp1 %>%
  filter(hc_singleparent) %>% 
  filter(profession_last_educ=="Программист") %>% 
  group_by(gender) %>% 
  count(gender) %>% 
  ungroup() %>% 
  mutate(ratio = n / sum(n))

unemp1 %>%
  filter(hc_singleparent) %>% 
  filter(profession_last_educ=="Программист") %>% 
  fi

# стаж в этом году
unemp1 %>%
  filter(hc_singleparent) %>% 
  count(experience, sort = TRUE) %>% 
  
# нашли ли работу люди без опыта за 12 месяцев
# all
unemp1 %>%
  count(experience, sort = TRUE)
# интересна доля одиночек с разным стажем
unemp1 %>%
  filter(hc_singleparent) %>%
  group_by(experience) %>% 
  count(experience) %>% 
  ungroup() %>% 
  mutate(ratio = (n / sum(n)))

# то же для всех:
unemp1 %>%
  group_by(experience) %>% 
  count(experience) %>% 
  ungroup() %>% 
  mutate(ratio = (n / sum(n)))
  
#  одиночек без опыта за год по регионам
unemp1 %>%
  filter(hc_singleparent) %>% 
  filter(experience == "0.0") %>% 
  count(region, sort = TRUE)

# all
unemp1 %>%
  filter(experience == "0.0") %>% 
  count(region, sort = TRUE)

# ЗП за 3 месяца!
unemp1 %>%
  filter(hc_singleparent) %>% 
  count(salary_average, sort = TRUE)

#all
unemp1 %>%
  count(salary_average, sort = TRUE)

unemp1 %>%
  filter(hc_singleparent) %>% 
  filter(region == "г. Москва") %>% 
  count(salary_average, sort = TRUE)


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

# доля
unemp1 %>%
  filter(hc_singleparent) %>% 
  group_by(gender) %>% 
  count(gender) %>% 
  ungroup() %>% 
  mutate(ratio = n / sum(n))

# общее
unemp1 %>%
  group_by(gender) %>% 
  count(gender) %>% 
  ungroup() %>% 
  mutate(ratio = n / sum(n))


# среди всех
unemp1 %>% 
  count(gender, sort = TRUE)

# МЕСЯЦ УВОЛЬНЕНИЯ 
unemp1 %>% 
  filter(hc_singleparent) %>% 
  count(month_dismissal)
###
# когда увольняли(сь)
unemp1 %>% 
  filter(hc_singleparent) %>%
  count(month_dismissal) %>% 
  ggplot(aes(month_dismissal, n))+
  geom_col()
# когда обратились в СЗН
unemp1 %>% 
  filter(hc_singleparent) %>%
  count(month_unemployed) %>% 
  ggplot(aes(month_unemployed, n))+
  geom_col()

# устроились
unemp1 %>% 
  filter(hc_singleparent) %>%
  count(month_employment) %>% 
  ggplot(aes(month_employment, n))+
  geom_col()

# снятие с учета как безработного
unemp1 %>% 
  filter(hc_singleparent) %>%
  count(month_close) %>% 
  ggplot(aes(month_close, n))+
  geom_col()

# тип трудоустройства одиночек и всех
unemp1 %>% 
  filter(hc_singleparent) %>%
  count(employment_type, sort = TRUE)

unemp1 %>% 
  filter(hc_singleparent) %>%
  count(employment_mode, sort = TRUE)

#######################################
## новые группы по возрасту
#######################################

unemp1 <- unemp1 %>% 
  mutate(age_new_group = case_when(age == 1 ~ "16-17",
                                   age %in% c(2, 3) ~ "18-27",
                                   age %in% 4:8 ~ "28-52",
                                   age %in% 9:15 ~ "53-60",
                                   age == 16 ~ "60+"))



