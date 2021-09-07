library(readxl)
library(tidyverse)
library(highcharter)

participantes <- readxl::read_xlsx(path = "participantes.xlsx", 
                                   col_names = TRUE)

prop.table(table(participantes$Genero))
table(participantes$Area) 
sort(table(participantes$Ciudad), decreasing = TRUE)

participantes$Nombre[participantes$Area == "Sociología"]

participantes %>% 
  dplyr::group_by(Area) %>% 
  dplyr::count() %>% 
  highcharter::hchart("item", hcaes(name = Area, y = n),
                      name = "Area disciplinar",
                      marker = list(symbol = "square"),
                      showInLegend = TRUE,
                      size = "100%",
                      center = list("50%, 75%"),
                      start_angle = -100,
                      end_angle = 100) %>%
  hc_add_theme(hc_theme_hcrt()) %>% 
  hc_chart(style = list(fontFamily = "Oswald")) %>% 
  hc_title(text = "Áreas disciplinarias participantes")

participantes %>% dplyr::group_by(Ciudad) %>% count() %>% 
  highcharter::hchart("bar", hcaes(x = Ciudad, y = n), 
                      name = "Frecuencia") %>% 
  hc_tooltip(crosshairs = TRUE) %>% 
  hc_add_theme(hc_theme_google()) %>% 
  hc_chart(style = list(fontFamily = "Oswald")) 

hchart(type = "area",
  density(participantes$Edad), 
  color = "#B71C1C", name = "Edad"
) 

f <- participantes %>% filter(Genero == "Femenino")
m <- participantes %>% filter(Genero == "Masculino")

hchart(
  density(f$Edad), type = "area", 
  color = "steelblue", name = "Femenino"
) %>%
  hc_add_series(
    density(m$Edad), type = "area",
    color = "#B71C1C", 
    name = "Masculino"
  )





