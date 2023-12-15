library(tidyverse)

banco <- read.csv("Meteorite_Landings.csv", na.strings = "")

banco$year <- replace(banco$year, banco$year == 2101, 2010)

banco$fall <- as.factor(banco$fall)

banco_linhas1 <- banco %>% 
  filter(!is.na(year), !is.na(fall)) %>%
  filter(year > 1971) %>%
  group_by(year, fall) %>%
  summarise(total = n())

banco_observados <- banco_linhas1 %>%
  filter(fall == "Fell")
banco_encontrados <- banco_linhas1 %>%
  filter(fall == "Found")

ggplot() +
  geom_line(data = banco_encontrados, aes(x = year, y = total, color = "encontrados"), linewidth = 1) +
  geom_point(data = banco_encontrados, aes(x = year, y = total, color = "encontrados"), size = 2) +
  geom_line(data = banco_observados, aes(x = year, y = total * 100, color = "observados"), linewidth = 1) +
  geom_point(data = banco_observados, aes(x = year, y = total * 100, color = "observados"), size = 2) +
  scale_color_manual(name = "Legenda", labels = c("encontrados", "observados"), values = c("encontrados" = "#440154", "observados" = "#fde725")) +
  labs(x = "Ano", y = "Número de meteoritos encontrados") +
  theme_minimal() +
  scale_y_continuous(
    sec.axis = sec_axis(~./100, name = "Número de meteoritos observados", breaks = seq(0, 30, by = 5))
  )
