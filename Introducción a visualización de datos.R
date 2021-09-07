library(highcharter)
library(tidyverse)
library(echarts4r)

# Carguemos la base de datos países, proveniente de la librería datos.
paises <- datos::paises

# Filtremos Chile para comenzar a graficar.
g1 <- paises %>% dplyr::filter(pais == "Chile")

# Diagrama de puntos y líneas ------------------------------------------------------

# ¿Cómo hacer un gráfico por defecto?
plot(g1$esperanza_de_vida ~ g1$anio, xlab = "Año", ylab = "GDP per capita",
     main = "Evolución del GDP per capita en Chile", col = "tomato", 
     cex.lab = 1, # Tamaño ejes.
     pch = 16) # Forma de los puntos.

# ggplot
ggplot2::ggplot(data = g1, aes(x = anio, y = esperanza_de_vida)) +
  ggplot2::geom_point(color = "black") +
  ggplot2::geom_line(color = "darkblue", size = 1) +
  ggplot2::labs(title = "Chile",
       subtitle = "Esperanza de vida 1952 a 2007",
       caption = "",
       x = "Año",
       y = "Esperanza de vida") +
  hrbrthemes::theme_ipsum() +
  ggplot2::geom_hline(yintercept = mean(g1$esperanza_de_vida), 
             color = "black", 
             linetype = "dashed", 
             alpha = 0.7)

# highchart

highcharter::hchart(g1, type = "line",hcaes(x = anio, 
                                            y = pib_per_capita), 
                    name = 'PIB per cápita') %>% 
highcharter::hc_tooltip(valueDecimals = 0, crosshairs = TRUE) %>% 
highcharter::hc_add_theme(hc_theme_google()) %>% 
highcharter::hc_title(text = "PIB per cápita 1952 a 2007",
           style = list(fontWeight = "bold", fontSize = "15px",
                        fontFamily = "Oswald"),
           align = "center") %>% 
highcharter::hc_subtitle(text = "Chile") %>% 
highcharter::hc_exporting(enabled = TRUE)

# ¿Qué pasa si queremos graficar por continentes?

g2 <- paises %>% 
      dplyr::filter(pais %in% c("Chile", "Argentina", "Perú",
                            "Colombia", "Brasil", "Bolivia",
                            "Ecuador", "Venezuela"))
# ggplot
ggplot2::ggplot(data = g2, aes(x = anio, y = esperanza_de_vida, color = pais)) +
  geom_line(size = 1.5) +
  labs(title = "Países latinoamericanos",
       subtitle = "Esperanza de vida 1952 a 2007",
       x = "Año",
       y = "Esperanza de vida") +
  hrbrthemes::theme_ipsum() 

# highcharter

highcharter::hchart(g2, type = "line", 
       hcaes(x = anio, y = esperanza_de_vida, group = pais)) %>% 
       hc_tooltip(crosshairs = TRUE, table = TRUE) %>%
       hc_add_theme(hc_theme_google()) %>% 
       hc_title(text = "Esperanza de vida 1952 a 2007",
           style = list(fontWeight = "bold", fontSize = "15px",
                        fontFamily = "Oswald"),
           align = "center") %>% 
      hc_subtitle(text = "Países latinoamericanos") 

# Ejemplos adicional a los expuestos en clases:

# Diagrama de puntos con etiquetas.
g3 <- paises %>% filter(anio == 2007)

# ggplot 
ggplot2::ggplot(g3, aes(y = pib_per_capita, x = esperanza_de_vida), fill = pais) +
  ggplot2::geom_point(size = 1) +
  ggrepel::geom_label_repel(aes(
    label = ifelse(pib_per_capita > 30000, as.character(pais),"")),
    size = 3,
    box.padding = 0.35,
    point.padding = 0.5,
    segment.color = 'grey50')

# highcharter
highcharter::hchart(g3, "scatter", 
       hcaes(esperanza_de_vida, pib_per_capita, group = continente)) %>% 
  highcharter::hc_add_theme(hc_theme_538()) %>% 
  highcharter::hc_title(text = "Esperanza de vida y PIB per cápita",
           style = list(fontWeight = "bold", fontSize = "15px",
                        fontFamily = "Oswald"))  %>% 
  highcharter::hc_xAxis(title = list(text = "Esperanza de vida")) %>% 
  highcharter::hc_yAxis(title = list(text = "PIB per cápita")) 

# Facetas -----------------------------------------------------------------

g4 <- paises %>% select(everything())

# ggplot

ggplot2::ggplot(g4, aes(x = pib_per_capita, y = esperanza_de_vida)) +
  ggplot2::geom_point(pch = 20, color = "darkblue") +
  ggplot2::facet_wrap(~continente) +
  ggplot2::theme(plot.title = element_text(colour = "cornflowerblue"),
        strip.text.x = element_text(size = 8, colour = "white"), 
        strip.background = element_rect(colour = "white", fill = "cornflowerblue"),
        axis.text.x = element_text(colour = "cornflowerblue"),
        axis.text.y = element_text(colour = "cornflowerblue"),
        axis.text = element_text(colour = "cornflowerblue"),
        legend.title = element_text(colour = "cornflowerblue"))

g4 %>% dplyr::filter(anio %in% c(1962, 2007)) %>% 
  ggplot(aes(esperanza_de_vida, pib_per_capita, col = continente)) +
  geom_point() +
  facet_grid(. ~ anio)

# Gráfico de barras -------------------------------------------------------

# Contemos el número de países según continente

g5 <- paises %>% 
  dplyr::group_by(continente) %>% 
  dplyr::count()

ggplot2::ggplot(g5, aes(x= continente, y = n, fill= continente)) + 
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::labs(y="Número de países", x = "Continente") 
  ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  ggplot2::geom_text(aes(label = n), vjust = -0.25)
  ggplot2::coord_flip()

# ¿Si queremos visualizar la población según continente?

g5 <- paises %>% 
  dplyr::select(pais, continente, poblacion) %>% 
  dplyr::group_by(continente) %>% 
  dplyr::summarise(Poblacion = sum(poblacion)) 

# ggplot
ggplot2::ggplot(data = g5, aes(x = reorder(continente, -Poblacion), y = Poblacion)) +
  ggplot2::geom_bar(stat = "identity", fill = "darkblue") +
  ggplot2::labs(title = "Población según continente",
       x = "Continente",
       y = "Población") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) #Si es que desea modificar el eje X.

# highcharter

highcharter::hchart(g5, "column", hcaes(x = continente, y = Poblacion)) %>% 
  highcharter::hc_tooltip(crosshairs = TRUE) %>% 
  highcharter::hc_add_theme(hc_theme_economist())

highcharter::hchart(g5, "bar", hcaes(x = continente, y = Poblacion)) %>% 
  highcharter::hc_tooltip(crosshairs = TRUE) %>% 
  highcharter::hc_add_theme(hc_theme_economist())

# Boxplot -----------------------------------------------------------------

g6 <- paises %>% dplyr::filter(continente == "Américas")

# ggplot
ggplot2::ggplot(g6, aes(x = pais, y = esperanza_de_vida)) +
  ggplot2::geom_boxplot(fill = "steelblue") + 
  ggplot2::labs(x = "País", y = "Años",
       title = "Esperanza de vida en América",
       subtitle = "Entre los años 1952 a 2007",
       caption = "Fuente Datos: gapminder") +
  ggplot2::coord_flip()

# highchart (grafiquemos los continentes)
g6 <- highcharter::data_to_boxplot(data = paises,
                      variable = esperanza_de_vida,
                      group_var = continente,
                      name = "Esperanza de vida",
                      add_outliers = TRUE,
                      color = 'steelblue')

highcharter::highchart() %>%
  highcharter::hc_xAxis(type = "category") %>%
  highcharter::hc_add_series_list(g6) %>% 
  highcharter::hc_exporting(enabled = TRUE) 


# Histograma y densidad --------------------------------------------------------------

# ggplot

ggplot2::ggplot(paises, aes(x = esperanza_de_vida)) +
  ggplot2::geom_histogram(aes(color = continente, fill = continente), 
                 position = "identity", bins = 30, alpha = 0.6) 
# highcharter

highcharter::hchart(type = "area", 
                    density(paises$pib_per_capita), color = "#B71C1C")  

# Gráfico de sectores -----------------------------------------------------

g7 <- paises %>% 
  dplyr::select(pais, poblacion, anio, continente) %>% 
  dplyr::mutate(porcentaje = poblacion/sum(poblacion)*100) %>% 
  dplyr::filter(anio == max(anio, na.rm = TRUE) &
                continente == "Europa")

# highcharter
highcharter::hchart(g7, "pie", hcaes(x = pais, y = porcentaje), innerSize = 100) %>% 
  highcharter::hc_exporting(enabled = TRUE) %>% 
  highcharter::hc_chart(style = list(fontFamily = "Roboto")) %>% 
  highcharter::hc_tooltip(valueDecimals = 2, crosshairs = TRUE) %>% 
  highcharter::hc_title(text = "Porcentaje de la población europea") %>% 
  highcharter::hc_subtitle(text = "Gráfico de sectores") %>% 
  highcharter::hc_credits(enabled = TRUE, text = "Autor: Sebastián Massa Slimming",
             style = list(fontSize = "12px"))

# ggplot

g7 <- g7 %>% filter(pais %in% c("Alemania", "España", "Italia", "Polonia")) 

ggplot2::ggplot(g7, aes(x = "", y = porcentaje, fill = pais)) +
  ggplot2::geom_bar(stat = "identity", color = "white", alpha = 0.8, width = 2) +
  ggplot2::coord_polar(theta = "y", start = 0) +
  ggplot2::theme_void() + 
  ggplot2::geom_text(aes(label = round(porcentaje, 2)),
            position = position_stack(vjust = 0.5),
            color = "white",
            size = 3) +
  guides(fill = guide_legend(reverse = TRUE)) 
  # scale_fill_manual(values = rainbow(30)) #Escala de colores.

# Actividad 1 -------------------------------------------------------------

nacimientos <- guaguas::guaguas

# Efecto biblia

nacimientos %>% 
  dplyr::filter(nombre %in% c("Osmán", "Noé", "Lázaro", "Jesús", "Isaías", "Abraham")) %>% 
  ggplot2::ggplot(aes(anio, n, color = nombre)) +
  ggplot2::geom_line() +
  ggplot2::labs(x = "año", y = "total inscripciones", color = "nombre", 
       title = "Inscripciones de nombres bíblicos entre 1925 - 2000") +
  hrbrthemes::theme_ipsum()

# Contexto político de los años 60/70

nacimientos  %>% 
  dplyr::filter(nombre %in% c("Salvador", "Augusto"), 
         anio >= 1960 & anio <= 1980) %>% 
  ggplot2::ggplot(aes(anio, n, color = forcats::fct_reorder2(nombre, n, anio))) + 
  ggplot2::geom_line(size = 1) +
  ggplot2::labs(x = "año", y = "total inscripciones", color = "nombre", 
       title = "Inscripciones de 'Salvador' y 'Augusto' entre 1960 - 1980") +
  hrbrthemes::theme_ipsum()

# Efecto romané año 2000

nacimientos %>% 
  dplyr::filter(nombre %in% c("Milenka", "Branco", "Salomé"), 
         anio > 1980) %>% 
  ggplot2::ggplot(aes(anio, n, color = nombre)) + 
  ggplot2::geom_line() +
  ggplot2::labs(x = "año", y = "total inscripciones",
       title = "Inscripciones de nombres de personajes de 'Romané'")

# Actividad 2 --------------------------------------------------------------

# La base de datos data_vis posee informacion de distintas sucursales.

library(readxl)
data_vis <- read_xlsx(path = "datos/data_vis.xlsx")

head(data_vis)

# 1. Construir gráfico que muestre la relación entre solicitudes de tarjetas y 
# ventas por internet

# Primer gráfico: observemos plot por defecto
plot(data_vis$Venta_internet ~ data_vis$Solicitud_tarjeta)

#Utilizando ggplot2
ggplot2::ggplot(data = data_vis, aes(x= Solicitud_tarjeta, 
                            y = Venta_internet, 
                            col = Comuna)) +
  ggplot2::geom_point() 
# scale_y_log10() + #Escala logarítmica
# geom_smooth(method="lm") # Regresión

#2. Construir gráfico que muestre la relación entre la rentabilidad por 
# interés y la solicitud de tarjetas, según comuna.

ggplot2::ggplot(data = data_vis, aes(x= Rentabilidad_interes, y = Solicitud_tarjeta, 
                            color = Comuna)) + 
  ggplot2::geom_point()

# 3. Construir gráfico que muestre sucursales de Providencia, durante el año 2020, 
# que muestren el total de venta por internet.

filtro <- dplyr::filter(data_vis, Anio == "2020", 
                        Comuna == "Providencia") %>% 
          dplyr::arrange(Venta_internet)

ggplot2::ggplot(data = filtro, 
       aes(x= reorder(Sucursal, Venta_internet), y = Venta_internet, 
           fill = Comuna)) + 
  ggplot2::geom_col() +
  ggplot2::coord_flip()

# 4. Comuna que más ha solicitado tarjetas.

e <- ggplot2::ggplot(data_vis, aes(x = Comuna, 
                                    y = Solicitud_tarjeta, 
                                    fill = Comuna)) +
      ggplot2::geom_boxplot() 

# Guardar gráficos

if(!dir.exists("graficos")) dir.create("graficos")

pdf("graficos/e.pdf")
e <- ggplot(data_vis, aes(x = Comuna, y = Solicitud_tarjeta, fill = Comuna)) +
  geom_boxplot() 
print(e)
dev.off()

ggsave("graficos/e.pdf")
ggsave("graficos/e.png")

# Animación ---------------------------------------------------------------

# ¿Cómo podemos animar un gráfico?

library(gganimate) # Librería para animar
library(gifski) # Gif converter

# Leer datos
data_vis <- read_xlsx(path = "datos/data_vis.xlsx")

# Transformar la variable edad en número entero.
data_vis$Anio <- as.integer(data_vis$Anio) 

animacion <- ggplot(data = data_vis,
                    mapping = aes(x = Solicitud_tarjeta,
                                  y = Venta_internet,
                                  color= Comuna,
                                  size= Venta_anual)) +
  geom_point() +
  guides(size=FALSE) +
  theme_minimal() +
  # scale_x_continuous(trans = 'log10',breaks = c(1000,10000,70000)) +
  # scale_color_manual(breaks=c("Las Condes","Providencia","Ñuñoa","Puente Alto","Santiago"),
  #                    values = c("#E41A1C","#377EB8","#4DAF4A" ,"#984EA3","#FF7F00")) +
  labs(x = "Solicitud de tarjeta",
       y = "Venta por internet",
       title="Año:{frame_time}",
       color="Comuna",
       caption="Fuente: Ripley") +
  transition_time(time = Anio) + 
  ease_aes('linear')

# Si desea guardar su gráfico
anim_save(filename="animacion.gif",
          animation = animacion,
          width=2100,
          height=1500,
          res=300)

# Reproducir el gif
animacion

# Otras librerías de visualización ----------------------------------------

# plotly

library(plotly)

ejemplo <- ggplot2::ggplot(data = data_vis,
                  mapping = aes(x= Solicitud_tarjeta, y = Venta_internet, col = Comuna)) +
  ggplot2::geom_point() 

plotly::ggplotly(ejemplo) %>% 
  config(locale = "es") %>% 
  config(displaylogo = FALSE, 
         modeBarButtonsToRemove = list(
           'sendDataToCloud',
           'resetScale2d',
           'hoverClosestCartesian',
           'hoverCompareCartesian',
           'lasso2d',
           'pan2d',
           'zoomIn2d',
           'zoomOut2d'
         )) 

# echarts4r 
library(echarts4r)

data_vis %>% 
  dplyr::group_by(Comuna) %>% 
  echarts4r::e_charts(Solicitud_tarjeta) %>% 
  echarts4r::e_scatter(Venta_internet, symbol_size = 10) %>% 
  echarts4r::e_tooltip(trigger="axis", axisPointer = list(type = "cross"),
            textStyle=list(fontFamily="sans-serif",
                           fontSize=10)) %>% 
  echarts4r::e_title("Gráfico prueba", subtext = "Gráfico prueba") %>% 
  echarts4r::e_text_style(fontFamily = "sans-serif") %>% 
  echarts4r::e_toolbox_feature(feature = "saveAsImage", title = "Guardar gráfico") %>% 
  echarts4r::e_toolbox_feature("dataZoom", title = list(zoom = "Ampliar gráfico", back = "Deshacer")) %>% 
  echarts4r::e_toolbox_feature("restore", title = 'Restaurar')
# e_labels(position = 'inside', show = FALSE, fontSize = 10, rotate = 90) 

# Treemap -----------------------------------------------------------------

require(treemap)

dplyr::glimpse(paises)

paises$poblacion <- as.numeric(paises$poblacion)

tm <- treemap(
  paises,
  index = c("continente", "pais"),
  vSize = "poblacion",
  vColor = "pib_per_capita",
  type = "value"
)

highcharter::hctreemap2(data = paises,
           group_vars = c("pais", "continente"),
           size_var = "poblacion",
           color_var = "pib_per_capita",
           layoutAlgorithm = "squarified",
           levelIsConstant = FALSE,
           levels = list(
             list(level = 1, dataLabels = list(enabled = TRUE)),
             list(level = 2, dataLabels = list(enabled = FALSE))
           )) %>% 
  hc_add_theme(hc_theme_hcrt()) 

# Corplot -----------------------------------------------------------------

cor <- paises %>% select_if(is.numeric)
corplot <- cor(cor)
highcharter::hchart(corplot)
  



