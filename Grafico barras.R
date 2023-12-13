library(tidyverse)
library(scales)

banco2 <- banco
banco2$recclass <- as.factor(banco2$recclass)

tabela_class <- banco2 %>% filter(recclass != "NA") %>%
  group_by(recclass) %>%
  summarise(total =  n())

tabela_class <- tabela_class %>% 
  arrange(desc(total)) %>% 
  {.[0:10,]}

classes <- tabela_class %>%
  mutate(
    freq = total %>% percent(),
  ) %>%
  mutate(
    freq = paste0(round(100*total/45716, digits = 2),'%'),
    label = str_c(total, " (", gsub("\\.", ",", freq), ")") %>% str_squish()
  )

ggplot(classes) +
  aes(x = fct_reorder(recclass, total, .desc=T), y = total, label = label) +
  geom_bar(stat = "identity", fill = "#440154", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = 0.35, hjust = -0.1,
    size = 3
  ) + 
  labs(x = "Classficação", y = "Frequência") +
  coord_flip() +
  scale_y_continuous(limits = c(0,10000)) +
  theme_minimal()