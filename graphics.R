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
