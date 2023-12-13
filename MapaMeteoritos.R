library(leaflet)
library(tidyverse)

banco_mapa <- banco %>% rename(peso = mass..g.) %>%
  filter(reclat != "NA") %>%
  filter(reclong != "NA") %>%
  mutate(fall = case_when(
    fall %>% str_detect("Fell") ~ "Observado",
    fall %>% str_detect("Found") ~ "Achado"
  ))

banco_mapa$peso <- str_c(banco_mapa$peso) %>% str_replace("\\.", ",")
banco_mapa$fall <- as.factor(banco_mapa$fall)

pal <- colorFactor("viridis", levels = levels(banco_mapa$fall))

mytext <- paste(
  "Nome: ", banco$name, "<br/>", 
  "Tipo: ", banco$recclass, "<br/>", 
  "Peso em gramas: ", banco_mapa$peso, sep="") %>%
  lapply(htmltools::HTML)


m <- leaflet(banco_mapa) %>% 
  addTiles()  %>% 
  setView(lat=0, lng=0, zoom=2) %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addCircleMarkers(~reclong, ~reclat, 
                   fillColor = ~pal(banco_mapa$fall), fillOpacity = 0.7, color="white", radius=4, stroke=FALSE,
                   label = mytext,
                   labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
  ) %>% addLegend(pal = pal, values = banco_mapa$fall, opacity=0.9, title = "Legenda", position = "bottomright")

m 

