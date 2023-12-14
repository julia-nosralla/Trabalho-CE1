library(tidyverse)
library(scales)

quadro_resumo <- banco3 %>% 
  group_by(recclass) %>%
  summarize(Média = round(mean(mass..g.),2),
            `Desvio Padrão` = round(sd(mass..g.),2),
            `Variância` = round(var(mass..g.),2),
            `Mínimo` = min(mass..g.),
            `1º Quartil` = round(quantile(mass..g., probs = .25),4),
            Mediana = round(quantile(mass..g., probs = .5),2),
            `3º Quartil` = round(quantile(mass..g., probs = .75),2),
            `Máximo` = round(max(mass..g.),2)) %>% t() %>% as.data.frame() %>% 
  mutate(V1 = str_replace(V1,"\\.",","),
         V2 = str_replace(V2,"\\.",","),
         V3 = str_replace(V3,"\\.",","),
         V4 = str_replace(V4,"\\.",","),
         V5 = str_replace(V5,"\\.",","),
         V6 = str_replace(V6,"\\.",","),
         V7 = str_replace(V7,"\\.",","),
         V8 = str_replace(V8,"\\.",","),
         V9 = str_replace(V9,"\\.",","),
         V10 = str_replace(V10,"\\.",",")
         ) %>%
  rename(CM2 = V1, H4 = V2, `H4/5` = V3, H5 = V4, H6 = V5, L4 = V6, L5 = V7, L6 = V8, LL5 = V9, LL6 = V10)

quadro_resumo <- quadro_resumo[-1,]
