# доля одиноких среди всех безработных
unemp1 %>%
  count(hc_singleparent) %>% 
  mutate(ratio = n / sum(n)) %>%
  ggplot(aes("", ratio, fill = hc_singleparent)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  #scale_fill_brewer(aesthetics = "fill") +
  scale_fill_manual(values = c("thistle3","wheat2")) +
  #scale_y_log10() +
  theme_void() +
  theme(legend.position ="none")

unemp1 %>% 
  count(hc_singleparent) %>% 
  mutate(ratio = round(n / sum(n) * 100, 2)) %>%
  ggplot(aes("", ratio, n, fill = hc_singleparent, label = ratio)) +
  geom_bar(width = 1, stat = "identity", color="white") +
  coord_polar("y", start = 0) +
  geom_label_repel() +
  scale_fill_brewer(aesthetics = "fill") +
  theme_void() +
  labs(title = "Доля родителей-одиночек" )

unemp1 %>%
  count(hc_largefam) %>% 
  mutate(ratio = round(n / sum(n) * 100, 2)) %>%
  ggplot(aes("", ratio, n, fill = hc_largefam, label = ratio)) +
  geom_bar(width = 1, stat = "identity", color="white") +
  coord_polar("y", start = 0) +
  geom_label() +
  scale_fill_brewer(aesthetics = "fill") +
  theme_void() +
  labs(
    title = "Доля многодетных родителей"
  )
unemp1 %>% 
  filter(hc_singleparent) %>%
  count(gender) %>%
  mutate(ratio = round(n / sum(n) * 100, 2)) %>%
  ggplot(aes(gender, ratio, fill = gender, label = ratio)) +
  geom_bar(width = 1, stat = "identity", color="white") +
  #coord_polar("y", start = 0) +
  geom_label(show.legend = FALSE) +
  #scale_fill_brewer(palette = "Paired") +
  scale_fill_manual(values = c("thistle3","wheat2")) +
  theme_void() +
  scale_y_log10() +
  guides(fill=guide_legend(title="Пол")) +
  labs(title = "Соотношение мужчин и женщин среди одиноких родителей") #-> singleparent_bar

unemp1 %>% 
  filter(hc_largefam) %>%
  count(gender) %>%
  mutate(ratio = round(n / sum(n) * 100, 2)) %>%
  ggplot(aes(gender, ratio, fill = gender, label = ratio)) +
  geom_bar(width = 1, stat = "identity", color="white") +
  #coord_polar("y", start = 0) +
  geom_label(show.legend = FALSE) +
  #scale_fill_brewer(palette = "Set2") +
  scale_fill_manual(values = c("steelblue3","tan")) +
  theme_void() +
  #scale_y_log10() +
  guides(fill=guide_legend(title="Пол")) +
  labs(title = "Соотношение мужчин и женщин среди многодетных родителей")
# steelblue3 tan
singleparent_bar
ggplotly(singleparent_bar)

library(treemap)
#### treemap
unemp1 %>% 
  filter(hc_largefam) %>%
  count(education_filtered) %>%
  treemap(index = "education_filtered",
          vSize = "n",
          type = "index")

unemp1 %>% 
  filter(hc_largefam) %>%
  count(age_new_group) %>%
  treemap(index = "age_new_group",
          vSize = "n",
          type = "index")

unemp1 %>%
  filter(hc_largefam) %>%
  count(age_new_group) %>%
  treemap(index = "age_new_group",
          vSize = "n",
          type = "index") 

unemp1 %>% 
  filter(hc_singleparent) %>%
  count(age_new_group) %>%
  treemap(index = "age_new_group",
          vSize = "n",
          type = "index")

## последние места работы

unemp1 %>%
  #group_by(gender) %>%
  filter(gender == "Женский",
         hc_singleparent, 
         profession_last_work != "None",
         profession_last_work != "ANONYMIZATION") %>%
  #group_by(gender)%>%
  count(profession_last_work) %>%
  top_n(10)%>%
  mutate(profession_last_work = ifelse(str_detect(profession_last_work, "Сотрудник"), "Сотрудник обр. учреждения", profession_last_work))%>%
  mutate(profession_last_work = factor(profession_last_work, levels = profession_last_work)) %>%
  ggplot(aes(fct_reorder(profession_last_work, n), n, profession_last_work))+
  geom_col(fill = "steelblue3")+
  coord_flip()+
  labs(title = "Последние места занятости среди безработных одиноких родителей-мужчин",
       x = "",
       y = "")+
  theme_linedraw()
