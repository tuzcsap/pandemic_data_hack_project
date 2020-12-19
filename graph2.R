# доля одиноких среди всех безработных
unemp1 %>%
  count(hc_singleparent) %>% 
  mutate(ratio = n / sum(n)) %>%
  ggplot(aes("", ratio, n, fill = hc_singleparent)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  scale_fill_brewer(aesthetics = "fill") +
  theme_void() +
  theme(legend.position ="none")

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


