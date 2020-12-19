# доля
unemp1 %>%
  filter(hc_singleparent) %>% 
  group_by(gender) %>% 
  count(gender) %>% 
  ungroup() %>% 
  mutate(ratio = n / sum(n))

unemp1 %>%
  filter(hc_singleparent) %>% 
  group_by(gender) %>% 
  count(gender) %>% 
  ungroup() %>% 
  mutate(ratio = n / sum(n)) %>% 
  ggplot(aes("", ratio, fill = gender)) +
  geom_bar(width = 1, stat = "identity", color="white",  alpha=0.6) +
  coord_polar("y", start = 0) +
  scale_fill_brewer(palette = "Paired") +
  theme_void() +
  labs(title = "соотношение родителей-одиночек")

unemp1 %>%
  filter(hc_largefam) %>% 
  group_by(gender) %>% 
  count(gender) %>% 
  ungroup() %>% 
  mutate(ratio = n / sum(n)) %>% 
  ggplot(aes("", ratio, fill = gender)) +
  geom_bar(width = 1, stat = "identity", color="white",  alpha=0.6) +
  coord_polar("y", start = 0) +
  scale_fill_brewer(palette = "Paired") +
  theme_void() +
  labs(title = "соотношение многодетных родителей")
  


# общее
unemp1 %>%
  group_by(gender) %>% 
  count(gender) %>% 
  ungroup() %>% 
  mutate(ratio = n / sum(n))

unemp1 %>%
  count(hc_singleparent) %>%  
  ggplot(aes("", n, fill = hc_singleparent)) +
  geom_bar(width = 1, stat = "identity", color="white",  alpha=0.6) +
  coord_polar("y", start = 0) +
  scale_fill_brewer(palette = "Paired") +
  theme_void()


unemp1 %>%
  count(hc_largefam) %>%  ggplot(aes("", n, fill = hc_largefam)) +
  geom_bar(width = 1, stat = "identity", color="white") +
  coord_polar("y", start = 0) +
  scale_fill_brewer(palette = "Paired") +
  theme_void()


  


unemp1 %>%
  filter(hc_largefam) %>% 
  group_by(gender) %>% 
  count(gender) %>% 
  ungroup() %>% 
  mutate(ratio = n / sum(n))

unemp1 %>%
  filter(hc_largefam) %>%
  filter(hc_singleparent) %>% 
  count(sort = TRUE)

# ОБРАЗОВАНИЕ
unemp1 %>%
  filter(hc_largefam) %>%
  count(education_filtered) %>%
  ggplot(aes("", n, fill = education_filtered)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(x = "",
       y = "count",
       title = "Многодетные семьи",
       caption = "Образование безработных родителей в многодетной семье") +
  scale_fill_brewer(palette = "Spectral")

### образование
unemp1 %>%
  filter(hc_singleparent) %>%
  group_by(gender) %>% 
  count(education_filtered) %>%
  filter(gender=="Мужской") %>% 
  #pivot_wider(values_from = n, names_from = gender) %>% 
  ggplot(aes("", n, fill = education_filtered)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  theme_void() +
  facet_wrap(~gender) +
  labs(x = "",
       y = "count",
       title = "родители-одиночки",
       caption = "Образование родителей-одиночек (ж)") +
  scale_fill_brewer(palette = "Spectral")


unemp1 %>%
  filter(hc_largefam) %>%
  group_by(gender) %>% 
  count(education_filtered) %>%
  filter(gender=="Мужской") %>% 
  ggplot(aes("", n, fill = education_filtered)) +
  geom_col(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  theme_void() +
  facet_wrap(~gender) +
  labs(x = "",
       y = "count",
       title = "родители-одиночки",
       caption = "Образование родителей-одиночек") +
  scale_fill_brewer(palette = "Spectral")

unemp1 %>%
  filter(hc_largefam) %>%
  group_by(gender) %>% 
  count(education_filtered) %>%
  filter(gender=="Женский") %>% 
  ggplot(aes("", n, fill = education_filtered)) +
  geom_col(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  theme_void() +
  facet_wrap(~gender) +
  labs(x = "",
       y = "count",
       title = "родители-одиночки",
       caption = "Образование родителей-одиночек") +
  scale_fill_brewer(palette = "Spectral")

## работа 

  


# unemp1 %>%
#     filter(hc_singleparent) %>%
#     group_by(gender) %>%
#     count(education_filtered) %>% 
#     pivot_wider(values_from = n, names_from = gender) %>% 
#     View()

# ПО ВОЗРАСТУ
unemp1 %>% 
  filter(hc_singleparent) %>%
  group_by(gender) %>% 
  count(age_new_group) %>% 
  ggplot(aes(age_new_group, n, fill=gender))+
  geom_col() +
  facet_wrap(~gender, scales = "free") -> age_bar_chart

ggplotly(age_bar_chart)
