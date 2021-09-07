
# Correlación lineal ------------------------------------------------------

# Ambas categóricas: coef. de Chi-cuadrado Pearson, coef. de Phi de Pearson, entre otros.
# Ambas continuas: coef. de correlación lineal, coef.de Spearman, entre otros.
# Una categórica y una contínua: coef. f de Cohen, coef. biserial, test de independencia, 
# test de medias.

Mailings <- c(96, 40, 104, 128, 164, 76, 72, 80, 84, 180, 44, 36)
Conversiones <- c(41, 41, 51, 53, 60, 61, 50, 28, 48, 70, 33, 30)

x <- data.frame(Mailings, Conversiones)

sum_producto <- x %>% 
  mutate(x_barra = Mailings - mean(Mailings),
         y_barra = Conversiones - mean(Conversiones),
         producto = round(x_barra * y_barra, digits = 1)) %>% 
  summarise(sum_prod = sum(producto)) 

sum_producto/((nrow(x)-1)*sqrt(var(Mailings))*sqrt(var(Conversiones)))

cor(Mailings, Conversiones)

# Actividad campaña de marketing ------------------------------------------

library(readxl)
library(dplyr)

MKT <- read_xlsx(path = "datos/mkt_campana.xlsx", col_names = TRUE)

# Recodificar variables 'Mes' por los meses en formato caracter y
# realizar coerción explícita para transformar variable en factor.

MKT %>% 
  select(Mes, everything()) %>% 
  mutate(Mes = ifelse(Mes == 1, "Ene",
                      ifelse(Mes == 2, "Feb",
                             ifelse(Mes == 3, "Mar",
                                    ifelse(Mes == 4, "Abr",
                                           ifelse(Mes == 5, "May",
                                                  NA))))))

# Identificar el mínimo, máximo, promedio y mediana de leads por año.

MKT %>% 
  group_by(Año) %>%
  summarize(Max = max(Leads),
            Min = min(Leads),
            Median = median(Leads),
            Prom = mean(Leads)) 

# Calcular Churn Rate (N clientes perdidos/N clientes al inicio del periodo). 
# Redondear resultado a 3 decimales y expresar en porcentaje.

MKT %>% 
  mutate(Churn_rate = round(Leads_perdidos/Leads_anterior,3)*100) %>% 
  View()

# Calcular Burn Rate (visitan página pero la han abandonado). 

names(MKT)

MKT %>% 
  mutate(Burn_rate = Abandonos_web/Clicks_web) 

# Calcular el costo de adquisición por cliente.

names(MKT)

MKT %>% 
  mutate(CAC = Investment/Leads) 

# Determinar costo de inversión (ROI) para el mes de octubre del año 2018. 
# Luego, evaluar si hubo o no retorno negativo (<0)

MKT %>% 
  mutate(ROI = (Rentabilidad-Investment)/Investment) 

# Actividad CASEN  ------------------------------------------------------------

# La Encuesta Casen es una encuesta a hogares, de carácter transversal y multipropósito, realizada por el
# Ministerio de Desarrollo Social y Familia. Ha sido levantada de manera regular en el país desde 1987. 
# 
# Objetivos:
#   
# 1. Conocer periódicamente la situación socioeconómica de los hogares (composición hogares, familias, Educ, salud, etc)
# 2. Contar con diagnósticos para elaboración de políticas sociales.
# 3. Evaluar brechas entre distintos segmentos de la pob.
# 4. Estimar cobertura, focalización y distribución del gasto fiscal de los principales
# 
# Pob. Objetivo:  población que reside en viviendas particulares a lo largo del territorio nacional 
# Unidad de información: jefe de hogar o en su ausencia, algún miembro del hogar de 18 años o más. 
# 
# Marco muestral 
# El INE mantiene vigente al año 2017 un marco muestral conformado por dos marcos independientes, el marco de manzanas para el área Urbana y el marco de secciones para las áreas Rural y Resto de Área Urbana (RAU). 
# 
# Las unidades que lo componen se denominan conglomerados, que corresponden a conjuntos de viviendas agrupadas y contenidas en áreas geográficas definidas por límites de calles, pasajes, alturas de calles o aglomeraciones de viviendas particulares conformadas a partir de una o más entidades pobladas 
# 
# Estrategia muestral 
# 
# En Casen 2017 el diseño corresponde a la obtención de una muestra probabilística, estratificada y bietápica, siendo los estratos conformados por la dupla Comuna-Área. Adicionalmente, en el área urbana, las manzanas se clasificaron según su tamaño (en número de viviendas), creando 5 estratos de tamaño, en los cuales la muestra fue seleccionada de forma independiente, quedando los estratos del área urbana conformados por la terna Comuna-Área-Grupo de tamaño 

library(dplyr)
library(purrr)
library(readstata13)

# Lectura base de datos ---------------------------------------------------

casen <- readstata13::read.dta13("datos/Casen 2017.dta", 
                                 generate.factors=TRUE,
                                 convert.factors=TRUE, 
                                 nonint.factors=TRUE)

# Análisis exploratorio  --------------------------------------------------

casen <- tidyr::as_tibble(casen)
glimpse(casen)

# comuna,
# expc,                    #factor de expansión comunal
# expr,                    #factor de expansión regional
# sexo,                    #género
# esc,                     #años de escolaridad
# edad,                    #edad
# ytotcorh,                #Ingreso total del hogar corregido
# ytotcor,                 #Ingreso total corregido
# yoprcor,                 #Ingreso ocupación principal
# ypc,                     #Ingreso total per cápita del hogar corregido
# ytrabajocor,             #ingreso del trabajo
# ytrabajocorh,            #ingreso del trabajo del hogar
# ypchautcor,              #ingreso autónomo per cápita 
# y26_2c,                  #jubilación o pensión
# numper,                  #numero de personas en el hogar
# s4,                      #hijos vivos
# #hasta acá las numéricas
# pco1,                    #jefe de hogar
# activ,                   #actividad
# hacinamiento,            #hacinamiento
# pobreza,                 #pobreza
# pobreza_multi_5d,        #pobreza multidimensional
# r1a,                     #nacionalidad
# r3,                      #pertenencia a pueblos originarios
# v12,                     #metros cuadrados de la casa
# indmat)                  #índice de materialidad de la vivienda

# Análisis ----------------------------------------------------------------

# Seleccionar la variable región, edad, sexo, esc, pco1 y filtrar por la región metropolitana.

casen %>% 
  select(region, edad, sexo, pco1, esc) %>% 
  filter(region == "Región Metropolitana de Santiago") 

# Ingreso promedio del trabajo en Comuna de Vicatura, Ñuñoa, La Reina y Las Condes.

casen %>% 
  select(ytrabajocor, region, sexo, comuna) %>% 
  filter(comuna  %in% c("Vitacura", "Las Condes", "La Reina", "Ñuñoa")) %>% 
  group_by(comuna) %>% 
  summarize(ingreso_promedio = mean(ytrabajocor, na.rm = TRUE))

# Escolaridad promedio del jefe de hogar, según sexo

casen %>% 
  select(sexo, pco1, esc, region) %>% 
  filter(region == "Región Metropolitana de Santiago",
         pco1 == "Jefe(a) de hogar") %>% 
  group_by(sexo) %>% 
  summarize(escolaridad_promedio = mean(as.numeric(esc), na.rm = TRUE),
            n = n())

# Pero qué pasa si no sabemos cómo está escrito "Región Metropolitana"?
casen %>%
  select(region, edad, sexo, pco1, esc) %>% 
  filter(region == grep(pattern = "Metropolitana", x = region, value = TRUE))

casen %>% 
  select(region, edad, sexo, pco1, esc) %>% 
  filter(stringr::str_detect(region, "Metropolitana"))

# Número de extranjeros según comunas de Santiago.

casen %>%
  select(region, comuna, r1a, esc) %>% 
  filter(r1a == "Otra nacionalidad. Especifique país",
         region == "Región Metropolitana de Santiago") %>% 
  group_by(comuna) %>% 
  summarize(n = n(), prom_escolaridad = mean(esc, na.rm = TRUE)) %>% 
  mutate(ranking = min_rank(desc(n))) 

# edad promedio según actividad y sexo.

casen %>% 
  select(activ, edad, sexo) %>% 
  group_by(activ, sexo) %>%
  summarize(edad_promedio = mean(edad, na.rm = TRUE)) 

# Filtrar por mujeres jefas de hogares mayores a 24 años de edad,

casen %>%
  select(region, edad, sexo, pco1, esc) %>% 
  filter(sexo == "Mujer" & 
           edad < 24, 
         pco1 == "Jefe(a) de hogar") %>% 
  group_by(region) %>% 
  summarize(n = n()) %>% 
  mutate(prop = n/sum(n)) 

# agrupar por comunas de Santiago y calcular jubilacion promedio (y26_2c)

casen %>%
  group_by(comuna) %>%
  filter(region == "Región Metropolitana de Santiago") %>% 
  summarize(jubilación_promedio = mean(as.numeric(y26_2c), na.rm=TRUE)) %>% 
  arrange(desc(jubilación_promedio))

# mostrar las primeras 4 comunas con mayores ingresos en pensión, 
# hogar e independiente.

casen %>%
  group_by(comuna) %>%
  summarize(pension = mean(as.numeric(y26_2c), na.rm=TRUE),
            ingreso_hogar = median(ytotcorh, na.rm=TRUE),
            ingreso_ind = median(ytotcor, na.rm=TRUE)) %>% 
  top_n(pension, n = 4) 

# Hacinamiento por comuna

table(casen$hacinamiento, casen$comuna)

EJ <- casen %>% 
  select(hacinamiento, comuna, region) %>% 
  filter(region == "Región Metropolitana de Santiago") %>% 
  group_by(comuna, hacinamiento) %>% 
  count() %>% 
  tidyr::nest(comuna, n) # ¿qué pasa si queremos almacenar todo en una lista?

# Contabilicemos el nivel de hacinamiento, desocupados, pobreza y n de extranjeros, según Comuna.
options(scipen = 9999)

vis <-casen %>%
  group_by(comuna) %>%
  filter(region == "Región Metropolitana de Santiago") %>% 
  summarize(hacinamiento =  sum(hacinamiento == "Hacinamiento crítico (5 y más)" | hacinamiento == "Hacinamiento medio alto (3,5 a 4,9)" | hacinamiento == "Hacinamiento medio bajo (2,5 a 3,49)", na.rm = TRUE),
            desocupados = sum(activ == "Desocupados", na.rm=TRUE),
            pobreza = sum(pobreza == "Pobres extremos" | pobreza == "Pobres no extremos", na.rm=T),
            extranjero = sum(r1a == "Otra nacionalidad. Especifique país", na.rm=TRUE)
  )


