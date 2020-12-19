library(tidyverse)

ru_confirmed <- read_csv("https://raw.githubusercontent.com/grwlf/COVID-19_plus_Russia/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_RU.csv")
#ru_deaths <- read_csv("https://raw.githubusercontent.com/grwlf/COVID-19_plus_Russia/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_RU.csv")

# ru_confirmed %>% 
#   bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total")) %>% 
#   View()

ru_confirmed_longer <- ru_confirmed %>% 
  select(-c(UID : Admin2, Combined_Key)) %>% 
  pivot_longer(-c(`Province_State`, `Country_Region`, Lat, Long_), names_to = "date", values_to = "num_of_confirmed_cases") %>%
  rename(
    province_state = `Province_State`,
    country_region = `Country_Region`,
    latitude = Lat,
    longitude = Long_
  )

ru_confirmed_longer %>% 
  filter(province_state=="Moscow") %>% 
  ggplot(aes(date, num_of_confirmed_cases)) +
  geom_col()

# ru_confirmed_longer %>% 
#   group_by(country_region, date) %>%
#   summarise(num_of_confirmed_cases = sum(num_of_confirmed_cases)) %>%
#   ungroup() %>% 
#   ggplot(aes(date, num_of_confirmed_cases)) +
#   geom_line()

