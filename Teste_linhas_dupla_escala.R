library(tidyverse)

# Your data manipulation remains the same

banco_linhas1 <- banco %>% 
  filter(!is.na(year), !is.na(fall)) %>%
  filter(year > 1971) %>%
  group_by(year, fall) %>%
  summarise(total = n())

# Create separate data frames for each category
banco_observed <- banco_linhas1 %>%
  filter(fall == "Observado")
banco_found <- banco_linhas1 %>%
  filter(fall == "Achado")

# Create the plot with dual Y-axes
ggplot() +
  geom_line(data = banco_found, aes(x = year, y = total, color = "Achado"), size = 1) +
  geom_point(data = banco_found, aes(x = year, y = total), size = 2) +
  geom_line(data = banco_observed, aes(x = year, y = total * 100, color = "Observado"), size = 1) + # Scaling the observed data for secondary axis
  geom_point(data = banco_observed, aes(x = year, y = total * 100), size = 2) + # Scaling the observed data for secondary axis
  scale_color_manual(name = "Legenda", labels = c("Observado", "Achado"), values = c("Observado" = "#fde725", "Achado" = "#440154")) +
  labs(x = "Ano", y = "Número de meteoritos") +
  theme_minimal() +
  scale_y_continuous(
    sec.axis = sec_axis(~./100, name = "Observado", breaks = seq(0, 30, by = 5))
  )
