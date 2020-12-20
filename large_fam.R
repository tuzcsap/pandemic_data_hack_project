### многодетные
library(tidyverse)

unemp1 <- read_delim("/Users/mikhail/Desktop/Pandemic Data Hack/unemployed_1/unemployed_1_data.csv", delim=";")
#sample <- read_delim("/Users/mikhail/Desktop/Pandemic Data Hack/unemployed_1/unemployed_1_sample.csv", delim=";")

unemp1 %>% count(hc_largefam)

# многодетные, не работали, их образование
unemp1 %>% 
  filter(hc_didnotwork) %>% 
  filter(hc_largefam) %>% 
  count(education_filtered, sort = TRUE)

# предпенсионеры
unemp1 %>% 
  filter(hc_predpens) %>% 
  count(hc_largefam)


## Регион
#одинокие
unemp1 %>%
  filter(hc_largefam) %>% 
  count(region, sort = TRUE)
# all
unemp1 %>%
  count(region, sort = TRUE)

# Округа
unemp1 %>%
  filter(hc_largefam) %>% 
  count(district, sort = TRUE)
# all
unemp1 %>%
  count(district, sort = TRUE)

## фильтр: высшее в одно значение
unemp1 <- unemp1 %>% 
  #mutate(education_filtered = str_replace_all(education,"Высшее", "Высшее")) %>% 
  mutate(education_filtered = ifelse(str_detect(education, "Высшее"), "Высшее образование", education))

# образование для многодетных
unemp1 %>%
  filter(hc_largefam) %>% 
  filter(age==1) %>% 
  count(education_filtered, sort = TRUE)

# образование многодетных 16-17 лет
unemp1 %>%
  filter(hc_largefam) %>% 
  filter(age==1) %>% 
  View()

# образование для всех
unemp1 %>% 
  count(education_filtered, sort = TRUE)

# возрастные группы с начальным образованием
unemp1 %>%
  filter(hc_largefam) %>% 
  filter(education_filtered=="Начальное общее образование") %>% 
  View()
  count(age, sort = TRUE)

# образование по полу у многодетных
unemp1 %>%
  filter(gender=="Мужской") %>% 
  filter(hc_largefam) %>% 
  count(education_filtered, sort = TRUE)

unemp1 %>%
  filter(gender=="Женский") %>% 
  filter(hc_largefam) %>% 
  count(education_filtered, sort = TRUE)

# по полу
unemp1 %>%
  filter(hc_largefam) %>% 
  count(gender)
# доля
unemp1 %>%
  filter(hc_largefam) %>% 
  group_by(gender) %>% 
  count(gender) %>% 
  ungroup() %>% 
  mutate(ratio = n / sum(n))

# расторжение
unemp1 %>%
  filter(hc_largefam) %>% 
  count(dismissal_reason, sort = TRUE)

unem

# профессия по последнему месту работы
unemp1 %>%
  filter(hc_largefam) %>% 
  count(profession_last_work, sort = TRUE)
# и для всех
unemp1 %>%
  count(profession_last_work, sort = TRUE)

unemp1 %>%
  filter(hc_largefam) %>% 
  filter(gender=="Мужской") %>% 
  count(profession_last_work, sort = TRUE)

# котельные
unemp1 %>%
  filter(hc_largefam) %>% 
  filter(gender=="Женский") %>% 
  filter(profession_last_work == "Машинист котельной установки") %>% 
  count(region, sort = TRUE)

unemp1 %>%
  filter(hc_largefam) %>% 
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
  filter(hc_largefam) %>% 
  count(profession_last_educ, sort = TRUE)

# что тут произошло?
unemp1 %>%
  filter(hc_largefam) %>% 
  filter(profession_last_educ=="Программист") %>% 
  group_by(gender) %>% 
  count(gender) %>% 
  ungroup() %>% 
  mutate(ratio = n / sum(n))

# unemp1 %>%
#   filter(hc_largefam) %>% 
#   filter(profession_last_educ=="Программист")

# стаж в этом году
unemp1 %>%
  filter(hc_largefam) %>% 
  count(experience, sort = TRUE)
  
  # нашли ли работу люди без опыта за 12 месяцев
  # all
unemp1 %>%
  count(experience, sort = TRUE)
# интересна доля многодетных с разным стажем
unemp1 %>%
  filter(hc_largefam) %>%
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

#  многодетные без опыта за год по регионам
unemp1 %>%
  filter(hc_largefam) %>% 
  filter(experience == "0.0") %>% 
  count(region, sort = TRUE)

# all
unemp1 %>%
  filter(experience == "0.0") %>% 
  count(region, sort = TRUE)

# ЗП за 3 месяца!
unemp1 %>%
  filter(hc_largefam) %>% 
  count(salary_average, sort = TRUE)

#all
unemp1 %>%
  count(salary_average, sort = TRUE)

unemp1 %>%
  filter(hc_largefam) %>% 
  filter(region == "г. Москва") %>% 
  count(salary_average, sort = TRUE)


# ВОЗРАСТ
unemp1 %>% 
  filter(hc_largefam) %>% 
  count(age, sort = TRUE)

# среди всех
unemp1 %>% 
  count(age, sort = TRUE)


# МЕСЯЦ УВОЛЬНЕНИЯ 
unemp1 %>% 
  filter(hc_largefam) %>% 
  count(month_dismissal, sort = TRUE)
###
# когда увольняли(сь)
unemp1 %>% 
  filter(hc_largefam) %>%
  count(month_dismissal, sort = TRUE) %>% 
  ggplot(aes(month_dismissal, n))+
  geom_col()
# когда обратились в СЗН
unemp1 %>% 
  filter(hc_largefam) %>%
  count(month_unemployed, sort = TRUE) %>% 
  ggplot(aes(month_unemployed, n))+
  geom_col()

# устроились
unemp1 %>% 
  filter(hc_largefam) %>%
  count(month_employment) %>% 
  ggplot(aes(month_employment, n))+
  geom_col()

# снятие с учета как безработного
unemp1 %>% 
  filter(hc_largefam) %>%
  count(month_close) %>% 
  ggplot(aes(month_close, n))+
  geom_col()

# тип трудоустройства многодетных и всех
unemp1 %>% 
  filter(hc_largefam) %>%
  count(employment_type, sort = TRUE)

unemp1 %>% 
  filter(hc_largefam) %>%
  count(employment_mode, sort = TRUE)


