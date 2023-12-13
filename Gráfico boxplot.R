library(tidyverse)
library(scales)

banco3 <- banco %>% 
  filter(recclass != "NA") %>%
  filter(mass..g. != "NA") %>%
  filter_all(any_vars(. %in% c("L6", "H5", "L5", "H6", "H4", "LL6", "LL5", "L4", "H4/5", "CM2"))) %>%
  mutate(mass..g. = mass..g./ 1000)


ggplot(banco3) +
  aes(x = reorder(recclass, mass..g., median), y = mass..g.) +
  geom_boxplot(fill = c("#fde725"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Classificação", y = "Peso") +
  scale_y_continuous(trans = "log10") +
  theme_minimal()