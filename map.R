library(sf)
library(tidyverse)

rus_shp <- read_sf("https://code.highcharts.com/mapdata/countries/ru/custom/ru-all-disputed.geo.json")

unemp1 %>% count(region) %>% View()

unemp1 %>% filter(str_detect(region, "Алта")) %>% 
  count(region)

sample %>% distinct(region) %>% View()

rus_shp %>% distinct(name) %>% View()
rus_shp %>% count(name) 

rus_shp %>% 
  ggplot(aes()) +
  geom_sf() +
  coord_sf(datum = NA)

num_of_single_parents_by_region <- unemp1 %>%
  filter(hc_singleparent) %>% 
  count(region)

rus_shp_with_names %>%
  left_join(num_of_single_parents_by_region, by = c("rus_name" = "region")) %>%
  ggplot(aes(fill = n)) + 
  geom_sf() +
  coord_sf(datum = NA) +
  #theme(legend.position = "none") +
  scale_fill_gradient(low = "lightblue", high = "tomato")


#######
rus_shp_with_names %>% 
  ggplot(aes(fill = rus_name)) +
  geom_sf() +
  
  coord_sf(datum = NA) +
  theme(legend.position = "none") -> rus_map
########

# map
num_of_unempl <- unemp1 %>% 
  count(region)

rus_shp_with_names %>%
  left_join(num_of_unempl, by = c("rus_name" = "region")) %>%
  ggplot(aes(fill = n)) + 
  geom_sf() +
  coord_sf(datum = NA) +
  #theme(legend.position = "none") +
  scale_fill_gradient(low = "lightblue", high = "red") +
  labs(fill = "Количество безработных",
       title = "Уровень безработицы по регионам")


# map single
num_of_unempl_single <- unemp1 %>% 
  filter(hc_singleparent) %>% 
  count(region)

rus_shp_with_names %>%
  left_join(num_of_unempl_single, by = c("rus_name" = "region")) %>%
  ggplot(aes(fill = n)) + 
  geom_sf() +
  coord_sf(datum = NA) +
  #theme(legend.position = "none") +
  scale_fill_gradient(low = "lightblue", high = "red") +
  labs(fill = "Количество безработных",
       title = "Уровень безработицы одиноких родителей по регионам")

# map многодетные
num_of_unempl_largefam <- unemp1 %>% 
  filter(hc_largefam) %>% 
  count(region)

rus_shp_with_names %>%
  left_join(num_of_unempl_largefam, by = c("rus_name" = "region")) %>%
  ggplot(aes(fill = n)) + 
  geom_sf() +
  coord_sf(datum = NA) +
  #theme(legend.position = "none") +
  scale_fill_gradient(low = "lightblue", high = "red") +
  labs(fill = "Количество безработных",
       title = "Уровень безработицы многодетных родителей по регионам")

ggplotly(single_parents_map, tooltip=c(rus_shp_with_names$n))
# int_rus_map <- ggplotly(ggplot(rus_shp_with_names, aes(fill = rus_name)) +
#   geom_sf() +
#   coord_sf(datum = NA) +
#   theme(legend.position = "none"))
rus_shp_with_names %>%
  left_join(num_of_single_parents_by_region, by = c("rus_name" = "region")) %>%
  plot_ly(split = ~n)
# ggplotly(rus_map, tool_tip=c("text", "size"))



# добавить регионы
rus_shp %>% 
  mutate(rus_name = case_when(name == "Sevastopol" ~ "г. Севастополь",
                              name == "Crimea" ~ "Республика Крым",
                              name == "Chukchi Autonomous Okrug" ~ "Чукотский автономный округ",
                              name == "Arkhangel'sk" ~ "Архангельская область",
                              name == "Nenets" ~ "Ненецкий автономный округ",
                              name == "Yamal-Nenets" ~ "Ямало-Ненецкий автономный округ",
                              name == "Krasnoyarsk" ~ "Красноярский край",
                              name == "Sakha (Yakutia)" ~ "Республика Саха (Якутия)",
                              name == "Khabarovsk" ~ "Хабаровский край",
                              name == "Sakhalin" ~ "Сахалинская область",
                              name == "Kamchatka" ~ "Камчатский край",
                              name == "Kostroma" ~ "Костромская область",
                              name == "Moskva" ~ "г. Москва",
                              name == "Ryazan'" ~ "Рязанская область",
                              name == "Samara" ~ "Самарская область",
                              name == "Ul'yanovsk" ~ "Ульяновская область",
                              name == "Omsk" ~ "Омская область",
                              name == "Novosibirsk" ~ "Новосибирская область",
                              name == "Murmansk" ~ "Мурманская область",
                              name == "Leningrad" ~ "Ленинградская область",
                              name == "City of St. Petersburg" ~ "г. Санкт-Петербург",
                              
                              name == "Karelia" ~ "Республика Карелия",
                              name == "Karachay-Cherkess" ~ "Карачаево-Черкесская Республика",
                              name == "Ingush" ~ "Республика Ингушетия",
                              name == "Kabardin-Balkar" ~ "Кабардино-Балкарская Республика",
                              name == "North Ossetia" ~ "Республика Северная Осетия - Алания",
                              name == "Stavropol'" ~ "Ставропольский край",
                              name == "Smolensk" ~ "Смоленская область",
                              name == "Pskov" ~ "Псковская область",
                              name == "Tver'" ~ "Тверская область",
                              name == "Vologda" ~ "Вологодская область",
                              name == "Ivanovo" ~ "Ивановская область",
                              name == "Yaroslavl'" ~ "Ярославская область",
                              name == "Kaluga" ~ "Калужская область",
                              name == "Bryansk" ~ "Брянская область",
                              name == "Kursk" ~ "Курская область",
                              name == "Lipetsk" ~ "Липецкая область",
                              name == "Moskovsskaya" ~ "Московская область",
                              name == "Orel" ~ "Орловская область",
                              name == "Nizhegorod" ~ "Нижегородская область",
                              name == "Penza" ~ "Пензенская область",
                              
                              name == "Vladimir" ~ "Владимирская область",
                              name == "Voronezh" ~ "Воронежская область",
                              name == "Komi" ~ "Республика Коми",
                              name == "Sverdlovsk" ~ "Свердловская область",
                              name == "Bashkortostan" ~ "Республика Башкортостан",
                              name == "Udmurt" ~ "Удмуртская Республика",
                              name == "Mordovia" ~ "Республика Мордовия",
                              name == "Chuvash" ~ "Чувашская Республика - Чувашия",
                              name == "Chelyabinsk" ~ "Челябинская область",
                              name == "Orenburg" ~  "Оренбургская область",
                              name == "Saratov" ~ "Саратовская область",
                              name == "Tatarstan" ~ "Республика Татарстан (Татарстан)",
                              name == "Tomsk" ~ "Томская область",
                              name == "Tyumen'" ~ "Тюменская область",
                              name == "Khakass" ~ "Республика Хакасия",
                              name == "Chechnya" ~ "Чеченская Республика",
                              name == "Kalmyk" ~ "Республика Калмыкия",
                              name == "Dagestan" ~ "Республика Дагестан",
                              name == "Rostov" ~ "Ростовская область",
                              name == "Belgorod" ~ "Белгородская область",
                              name == "Gorno-Altay" ~ "Республика Алтай", 
                              
                              
                              name == "Tuva" ~ "Республика Тыва",
                              name == "Irkutsk" ~ "Иркутская область",
                              name == "Chita" ~ "Забайкальский край",
                              name == "Yevrey" ~ "Еврейская автономная область",
                              name == "Amur" ~ "Амурская область",
                              name == "Tambov" ~ "Тамбовская область",
                              name == "Tula" ~ "Тульская область",
                              name == "Novgorod" ~ "Новгородская область", 
                              name == "Volgograd" ~ "Волгоградская область", 
                              name == "Kirov" ~ "Кировская область", 
                              name == "Mariy-El" ~ "Республика Марий Эл",
                              name == "Kemerovo" ~ "Кемеровская область",
                              name == "Astrakhan'" ~ "Астраханская область",
                              name == "Primor'ye" ~ "Приморский край",
                              name == "Maga Buryatdan" ~ "Магаданская область",
                              name == "Buryat" ~ "Республика Бурятия",
                              name == "Kaliningrad" ~ "Калининградская область",
                              name == "Krasnodar" ~ "Краснодарский край",
                              name == "Kurgan" ~ "Курганская область",
                              name == "Altay" ~ "Алтайский край",
                              name == "Khanty-Mansiy" ~ "Ханты-Мансийский автономный округ - Югра",
                              name == "Perm'" ~ "Пермский край",
                              name == "Adygey" ~ "Республика Адыгея (Адыгея)")) -> rus_shp_with_names

rus_shp %>% 
  
  ggplot(aes(name)) +
  geom_sf() +
  coord_sf(datum = NA)

