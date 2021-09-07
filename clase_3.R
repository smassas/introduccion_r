library(tidyverse)
library(Lock5Data)
library(openintro)

data <- Lock5Data::HollywoodMovies
oscars <- openintro::oscars

# Análisis exploratorio data ----------------------------------------------

str(data)
glimpse(data)
names(data)
View(data)
sapply(data, class)

# Select ------------------------------------------------------------------
# Extrae columnas en una base de datos.

data %>% dplyr::select(Year)
data %>% dplyr::select(Year, Movie) 
data %>% dplyr::select(1, 15)
data %>% dplyr::select(-c(Year, Movie))
data %>% dplyr::select(starts_with(match = "G"))
data %>% dplyr::select(ends_with(match = "R"))
data %>% dplyr::select(where(is.character))
data %>% dplyr::select_if(is.numeric)

variables <- names(data)[c(3:5)]
data %>% dplyr::select(-all_of(variables)) 

# Como recordatorio de la sesión 1, también podemos utilizar comandos por defectos:

data$Movie
data[, c("Genre", "Year")]
data[, c(2:5)]
data[, -c(3,5)]

# Actividad 1 -------------------------------------------------------------

names(oscars)

oscars %>% dplyr::select(oscar_yr, name, movie)
oscars %>% dplyr::select(c(2,4,6))
oscars %>% dplyr::select(-c(1,6))
oscars %>% dplyr::select(!starts_with(match = "birth"))

# Filter ------------------------------------------------------------------
# Filtrar variables de acuerdo a condiciones lógicas.

data %>% dplyr::filter(Genre == "Drama") 
data %>% dplyr::filter(Genre == "Drama" & Year == 2017)
data %>% dplyr::filter(LeadStudio %in% c("Universal Pictures ", "Warner Bros. ")) 
data %>% dplyr::filter(Year >= 2012)
data %>% dplyr::filter(between(Year, 2012, 2014))
data %>% dplyr::filter(Budget == max(Budget, na.rm = TRUE))

# Como recordatorio de la sesión 1, también podemos utilizar comandos por defectos:

data[data$Year == 2007,]
data[data$Year >= 2009,]
data[data$Budget >= 210, 5]
data[data$Genre == "Drama",]
data[data$Genre == "Action" & data$Year == 2013,]
data[data$Genre == "Comedy" | data$Genre == "Drama",]
data[data$Year == max(data$Year), c("Movie", "Year"),]
data[!data$Year == 2007, c(1, 16),]
subset(x = data, subset = Genre == "Drama", select = c(1,16))

# Actividad 2 -------------------------------------------------------------

names(oscars)

oscars %>% dplyr::filter(oscar_yr >= 2015)
oscars %>% dplyr::filter(award == "Best actress" & age >= 50) 
oscars %>% dplyr::filter(name == "Meryl Streep")
oscars %>% dplyr::filter(name %in% c("Leonardo Di Caprio", "Meryl Streep"))
oscars %>% dplyr::filter(name == "Leonardo Di Caprio")
oscars %>% dplyr::filter(between(age, 20, 30),award == "Best actor")
oscars %>% dplyr::filter(age == max(age, na.rm = TRUE))
oscars[which.max(oscars$age),]

# Mutate ------------------------------------------------------------------
# Crea nuevas variables o modifica las ya existentes.

data %>% dplyr::mutate(Dif_rating = RottenTomatoes - AudienceScore)
data %>% dplyr::mutate(ROI = (WorldGross - Budget)/Budget) 
data %>% dplyr::mutate(Criterio = ifelse(AudienceScore >= 60, "Buena película", 
                                  "Mala película"))
data %>% dplyr::mutate_all(str_to_upper)
data %>% dplyr::mutate_if(is.double, as.integer) 

# Otros ejemplos

NSE <- data.frame(ID = paste("Entrevistado", seq(1, 6)),
                  NSE = c(1,1,2,3,4,5))

NSE %>% dplyr::mutate(NSE = ifelse(NSE == 1, "ABC1", "OTRO NIVEL"))

# Otros mecanismos, como comenté en clases:
NSE %>% dplyr::mutate(NSE = case_when(NSE == 1 ~ "ABC1", TRUE ~ "OTRO NIVEL"))
NSE %>% dplyr::mutate(NSE = gsub(pattern = "3", replacement = "C3", x = NSE))
NSE %>% dplyr::mutate(NSE = stringr::str_replace(string = NSE, 
                                          pattern = "1", replacement = "ABC1"))

# Agrego un ejemplo adicional, ajeno a la clase:
paises <- datos::paises 

paises %>% 
  dplyr::select(pais, esperanza_de_vida, poblacion,
                pib_per_capita, anio) %>% 
  dplyr::filter(pais == "Chile") %>% 
  dplyr::mutate(gobierno = case_when(anio >= 1952 & anio <= 1958 ~ "C. Ibañez",
                                     anio >= 1958 & anio <= 1964 ~ "Jorge Alessandri",
                                     anio >= 1964 & anio <= 1970 ~ "Eduardo Frei Montalva",
                                     anio >= 1970 & anio <= 1973 ~ "Salvador Allende",
                                     anio >= 1973 & anio <= 1990 ~ "Augusto Pinochet",
                                     anio >= 1990 & anio <= 1994  ~ "Patricio Aylwin",
                                     anio >= 1994 & anio <= 2000 ~ "Eduardo Frei Ruiz Tagle",
                                     anio >= 2000 & anio <= 2006 ~ "Ricardo Lagos Escobar",
                                     anio >= 2006 ~ "Michelle Bachelet Jeria"))

paises %>% 
  dplyr::select(pais, esperanza_de_vida, poblacion,
                pib_per_capita, anio) %>% 
  dplyr::filter(pais == "Chile") %>% 
  dplyr::mutate(gobierno = case_when(between(anio, 1952, 1958) ~ "C.Ibañez",
                                     between(anio, 1958, 1964) ~ "Jorge Alessandri",
                                     between(anio, 1964, 1970) ~ "Eduardo Frei Montalva",
                                     TRUE ~ "Otro gobierno"))


# Actividad 3: Mutate ------------------------------------------------------------------
# Calcular edad de ganadores de oscars

oscars %>% 
  dplyr::mutate(diferencia_dias = Sys.Date() - birth_date,
                edad_actual = as.numeric(round(diferencia_dias/365), digits = 0)) %>% 
  dplyr::select(diferencia_dias, edad_actual) 

oscars %>% 
  dplyr::mutate(diferencia_dias = as.numeric(difftime(Sys.Date(), birth_date)),
                edad_actual = round(diferencia_dias/365))  %>% 
  dplyr::select(edad_actual, diferencia_dias) 

# Group_by ----------------------------------------------------------------
# Agrupa variables de tipo categórico.

data %>% 
  dplyr::group_by(Genre) %>% 
  dplyr::count()

# Si no se comprendió, utilicemos otros ejemplos:

gapminder::gapminder %>% group_by(continent) %>% count()

# Agrego un ejemplo adicional, ajeno a la clase:

ciudad <- c("Santiago", "Santiago", "Concepción", "Concepción", "Temuco", "Temuco")
cantidad <- c(24, 11, 22, 16, 31, 21)
contaminacion <- data.frame(ciudad,cantidad)
contaminacion

contaminacion %>% 
  dplyr::group_by(ciudad) %>% 
  dplyr::summarise(promedio = mean(cantidad), n = n()) %>% 
  dplyr::mutate(ranking_MPF = min_rank(desc(promedio)))

# Actividad 4: Group_by ----------------------------------------------------------------

# De la base oscar, agrupar por nombre y contabilizar el numero de 
# óscars.

oscars %>% 
  dplyr::group_by(name) %>% 
  dplyr::count() %>% 
  dplyr::arrange(desc(n)) %>% 
  dplyr::filter(n >= 2)

oscars %>% 
  dplyr::group_by(name) %>% 
  dplyr::summarise(n = n()) %>% 
  dplyr::arrange(desc(n)) %>% 
  dplyr::mutate(ranking = min_rank(desc(n))) %>% 
  dplyr::filter(ranking == 3)

# Summarize ---------------------------------------------------------------
# Realiza resumen de los datos.

data %>% 
  dplyr::group_by(Genre) %>% 
  dplyr::summarize(Freq = n(),
                   Prom = mean(AudienceScore, na.rm = TRUE),
                   Median = median(AudienceScore, na.rm = TRUE),
                   SD = sd(AudienceScore, na.rm = TRUE)) %>% 
  dplyr::rename(Frecuencia = Freq,
         Promedio = Prom,
         Mediana = Median,
         Desviación = SD) %>% 
  tidyr::drop_na() 

# Otros mecanismos más sofisticados, que comenté en clases:

data %>% 
  dplyr::group_by(Genre) %>% 
  dplyr::summarise_at(vars(c(AudienceScore)), list(~n(), 
                                            ~mean(.x, na.rm = TRUE), 
                                            ~median(.x, na.rm =TRUE))) 

data %>% 
  dplyr::group_by(Genre) %>%
  dplyr::summarise_at(vars("AudienceScore"), funs(n(), mean(., na.rm = TRUE))) 

# Actividad 5: ¿Quiénes se salvan del Titanic? ---------------------------------------------------------------

titanic <- data.frame(Titanic)

titanic %>% 
  dplyr::rename(Clase = Class,
                Sexo = Sex,
                Edad = Age,
                Sobrevive = Survived) %>% 
  dplyr::group_by(Clase) %>% 
  dplyr::filter(Sobrevive == "Yes") %>% 
  dplyr::summarise(Sobrevivientes = sum(Freq)) %>% 
  dplyr::mutate(Prop = round(Sobrevivientes/sum(Sobrevivientes), 2),
                Ranking = min_rank(desc(Prop))) %>% 
  dplyr::arrange(Ranking) 

# Actividad 6: Presupuesto según productora ---------------------------------------------------------------

data <- Lock5Data::HollywoodMovies

library(tidyverse)

data %>% 
  dplyr::group_by(LeadStudio) %>% 
  # dplyr::mutate(LeadStudio = stringr::str_trim(LeadStudio),
  #               LeadStudio = stringr::str_remove_all(LeadStudio, "[[:punct:]]")) %>%
  # dplyr::filter(LeadStudio %in% c("Lionsgate",
  #                          "Universal Pictures", "Warner Bros",
  #                          "Paramount Pictures")) %>%
  dplyr::summarize(Presupuesto = sum(Budget, na.rm = TRUE),
                   Q1 = quantile(Budget, probs = 0.25, na.rm = TRUE),
                   Q2 = median(Budget, na.rm = TRUE),
                   Q3 = quantile(Budget, probs = 0.75, na.rm = TRUE),
                   Asimetría = moments::skewness(Budget, na.rm = TRUE),
                   Curtosis = moments::kurtosis(Budget, na.rm = TRUE)) %>% 
  dplyr::arrange(desc(Presupuesto))

# Podríamos trabajar con otra opción, aunque más engorrosa.

filtro <- c("Lionsgate", "Universal Pictures", "Warner Bros", "Paramount Pictures")

x <- data %>%
  dplyr::mutate(LeadStudio = stringr::str_trim(LeadStudio),
                LeadStudio = stringr::str_remove_all(LeadStudio, "[[:punct:]]")) %>%
  dplyr::filter(LeadStudio %in% filtro) %>%
  dplyr::group_by(LeadStudio) %>%
  dplyr::summarise_at(.vars = c("Budget"),
                      list(~quantile(.x, na.rm = TRUE,
                                     probs = c(0.25, 0.50, 0.75)))) %>%
  dplyr::mutate(Cuartiles = paste0("Q", seq(from = 1, to = 3, by = 1)))

wider <- x %>% pivot_wider(names_from = LeadStudio, values_from = Budget)
matriz <- t(wider)
colnames(matriz) <- c("Q1", "Q2", "Q3")
matriz <- matriz[-1,]
matriz <- apply(matriz, 2, as.numeric)
row.names(matriz) <- c("Lionsgate", "Paramount Pictures", "Universal Pictures",
                       "Warner Bros")
matriz

longer <- pivot_longer(wider, cols = 2:5, names_to = "Producción",
                       values_to = "Estadístico")
longer

# Cruce de base de datos --------------------------------------------------

base_1 <- data.frame(Tipo = c("IPA","Lager","Porter", "Lambic"), 
                     Lupulo = c("Alto","Medio","Medio", "Bajo"),
                     Premios = c(4, 3, 2, 4), 
                     IBU = c(93.2, 38.5, 55.2, 63.5))

base_2 <- data.frame(Tipo = c("IPA","Lambic", "Porter", "Weissbier"), 
                     Caracter = c("Amarga", "Amarga", "Ácido", "Ácido"),
                     Acentuacion = c("Lúpulo-frutal", "Terroso-frutal", "Chocolate-tostado", 
                                     "Cítrico-trigo"),
                     Origen = c("Inglaterra", "Estados Unidos", "Inglaterra", "Alemania"), 
                     Amargura = c("9", "7", "6", "4"))

# Left_join ---------------------------------------------------------------
# Toma como referencia las observaciones de la tabla 1.

dplyr::left_join(base_1, base_2, by = "Tipo")
dplyr::left_join(base_1, base_2, by = c("Tipo" = "Tipo"))
dplyr::left_join(base_1, base_2)
merge(x = base_1, y = base_2, all.x = T, all.y = F)

# Right_join --------------------------------------------------------------
# Toma como referencia las observaciones de la tabla 2

dplyr::right_join(x = base_1, y = base_2, by = c("Tipo" = "Tipo")) 
dplyr::right_join(base_1, base_2) 
dplyr::right_join(base_1, base_2, by = "Tipo")

# Inner_join --------------------------------------------------------------
# Toma como referencia solamente los campos donde existan las mismas observaciones

dplyr::inner_join(x = base_1, y = base_2, by = c("Tipo" = "Tipo"))
dplyr::inner_join(base_1, base_2, by = "Tipo")
dplyr::inner_join(x = base_1, y = base_2)

# Full_join ---------------------------------------------------------------
# Retorna todas las grabaciones en una nueva base de datos, independiente de si 
# figuran en ambas bases. 

dplyr::full_join(x = base_1, y = base_2, by = c("Tipo" = "Tipo"))
dplyr::full_join(x = base_1, y = base_2, by = "Tipo")
dplyr::full_join(base_1, y = base_2)

# Bind_rows ---------------------------------------------------------------

Tipo_1 <- data.frame(Tipo = c("IPA","PORTER", "LAMBIC"),
                     Premios = c(4, 3, 4), 
                     IBU = c(93.2, 38.5, 63.5))

Tipo_2 <- data.frame(Tipo = c("BOCK","STOUT", "WEISSBIER"),
                     Premios = c(2, 3, 5), 
                     IBU = c(78.2, 60.5, 40.8))

Tipo_1 %>% dplyr::bind_rows(Tipo_2)

# Bind_cols ---------------------------------------------------------------

Tipo_cervezas <- Tipo_1 %>% dplyr::bind_rows(Tipo_2)
Tipo_cervezas

Amargura_sabor <- data.frame(Amargura = c(9, 6, 8, 6, 6, 4),
                             Acentuacion = c("Lúpulo-frutal", 
                                             "Chocolate-tostado",
                                             "Terroso-frutal", 
                                             "Malta-tostado",
                                             "Malta-tostado",
                                             "Cítrico-trigo"))

Base_completa <- Tipo_cervezas %>% dplyr::bind_cols(Amargura_sabor)
Base_completa

# Pivot_longer ------------------------------------------------------------

Base_completa <- Base_completa %>% dplyr::mutate(anio_2018 = c(1,1,2,0,1,2),
                                                 anio_2019 = c(1,1,1,1,1,2),
                                                 anio_2020 = c(2,1,1,1,1,1)) %>% 
                                   dplyr::select(Tipo, starts_with("anio"))

data_1 <- dplyr::pivot_longer(data = Base_completa, 
                              cols = c("anio_2018", "anio_2019", "anio_2020"),
                              names_to = "Anio", values_to = "Frecuencia")

data_1

# Pivot_wider -------------------------------------------------------------

data_1 %>% dplyr::pivot_wider(names_from = Anio, values_from = Frecuencia)



