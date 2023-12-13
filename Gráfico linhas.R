library(tidyverse)

banco <- read.csv("Meteorite_Landings.csv", na.strings = "")

banco$year <- replace(banco$year, banco$year == 2101, 2010)

banco_linhas1 <- banco %>% filter(year != "NA") %>%
  filter(fall != "NA") %>%
  filter(year > 1971) %>%
  group_by(year, fall) %>%
  summarise(total =  n())

ggplot(banco_linhas1) +
  aes(x = year, y = total, group = fall, colour = fall) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_colour_manual(name = "Legenda", labels = c("Observado", "Achado"), values = c("#fde725", "#440154")) +
  labs(x = "Ano", y = "Número de meteoritos") +
  theme_minimal()