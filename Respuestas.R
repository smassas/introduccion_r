library(tidyverse)
library(readxl)

# Lectura base de datos

data <- readxl::read_xlsx(path = "proteinas.xlsx", col_names = TRUE)

# 1. De acuerdo a la tabla mostrada anteriormente, crear un vector filtro con todos los países nórdicos. Almacenar en objeto
# paises_nordicos.

Paises_Nordicos <- c("Dinamarca", "Finlandia", "Suecia", "Noruega")

# 2. De acuerdo a la base de datos, crear un vector filtro con todos los países de Europa del Este.
# Almacenar en objeto llamado "Europa_del_Este".

Europa_del_Este = c("URSS", "Bulgaria", "Checoslovaquia",
                      "Alemania Oriental", "Hungría", "Rumania",
                      "Yugoslavia", "Polonia")

# 3. De acuerdo a la base de datos, crear un vector filtro con todos los países de Europa del Este.
# Almacenar en objeto llamado "Europa_Occidental".

Europa_Occidental = c("Francia", "Alemania Occidental", "Holanda",
                      "Austria", "Portugal", "España", "Reino Unido",
                      "Grecia", "Irlanda", "Italia", "Suiza",
                      "Irlanda", "Bélgica", "Albania")

# 4. Categorizar países en función de las divisiones o filtros realizados anteriormente. 
# Guardar en vector data_analisis.

data_analisis <- data %>% 
  dplyr::mutate(Categoria = 
           forcats::fct_collapse(Pais,
                                 `Países Nórdicos` = c("Dinamarca", "Finlandia", "Suecia", "Noruega"),
                                 `Europa del Este` = c("URSS", "Bulgaria", "Checoslovaquia",
                                                       "Alemania Oriental", "Hungría", "Rumania",
                                                       "Yugoslavia", "Polonia"),
                                 `Europa Occidental` = c("Francia", "Alemania Occidental", "Holanda",
                                                         "Austria", "Portugal", "España", "Reino Unido",
                                                         "Grecia", "Irlanda", "Italia", "Suiza",
                                                         "Irlanda", "Bélgica", "Albania"))) 

data %>% 
  dplyr::mutate(Categoria = case_when(Pais %in% Paises_Nordicos ~ "Países Nórdicos", 
                               Pais %in% Europa_del_Este ~ "Europa del Este",
                               Pais %in% Europa_Occidental ~ "Europa Occidental", 
                               TRUE ~ "Otro"))

data %>% 
  dplyr::mutate(Categoria = ifelse(Pais %in% Paises_Nordicos, "Países Nórdicos", 
                         ifelse(Pais %in% Europa_del_Este, "Europa del Este",
                               ifelse(Pais %in% Europa_Occidental,  "Europa Occidental", 
                                  "Otro"))))

# Tabla con categorías

data_analisis %>% 
  dplyr::select(Pais, Categoria) %>% 
  dplyr::arrange(Pais) %>% 
  knitr::kable()

# 5. ¿Cuántos países hay en Europa Occidental, Europa del Este y Países Nórdicos?

data_analisis %>% 
  dplyr::group_by(Categoria) %>% 
  dplyr::count() %>% 
  dplyr::mutate(prop = n / nrow(data_analisis))

data_analisis %>% 
  dplyr::group_by(Categoria) %>% 
  dplyr::summarise(n = n()) %>%
  dplyr::mutate(freq = n / sum(n))

prop.table(table(data_analisis$Categoria))

# 6. ¿Qué categoría (Europa Occidental, Europa del Este, Países Nórdicos) es la que más consume (en promedio) carnes rojas?

data_analisis %>% 
  dplyr::group_by(Categoria) %>% 
  dplyr::summarize(consumo_promedio_rojas = mean(Rojas)) 

# 7. ¿Qué categoría (Europa Occidental, Europa del Este, Países Nórdicos) es la que más consume (en promedio) carnes blancas?

data_analisis %>% 
  dplyr::group_by(Categoria) %>% 
  dplyr::summarize(consumo_promedio_rojas = mean(Blancas)) 

# 8. ¿De cuánto es la diferencia de calorías entre el país que más consume carnes rojas y el que menos consume?

data_analisis %>% 
  dplyr::select(Pais, Rojas) %>% 
  dplyr::summarise(Diferencia = max(Rojas) - min(Rojas))
  
# 9. País de Europa occidental que más consume vegetales.

data_analisis %>% 
  dplyr::filter(Categoria == "Europa Occidental") %>% 
  dplyr::select(Vegetales, Pais) %>% 
  dplyr::slice_max(Vegetales)

data_analisis %>% 
  dplyr::select(Vegetales, Pais, Categoria) %>% 
  dplyr::filter(Categoria == "Europa Occidental" &
         Vegetales == max(Vegetales, na.rm = TRUE))

subset(data_analisis, subset = 
         Categoria == "Europa Occidental" & 
         Vegetales == max(Vegetales, na.rm = TRUE))


# 10. Calcular y filtrar por el primer cuartil (Q1) de los cereales consumidos. 
# Agrupar según categoría y determinar qué categoría posee mayor cantidad de países 
# que cumplen con la condición realizada (cálculo y filtro por Q1)

data_analisis %>% 
  dplyr::select(Pais, Cereales, Categoria) %>% 
  dplyr::filter(Cereales <= quantile(Cereales, prob = c(0.25))) %>% 
  dplyr::group_by(Categoria) %>% 
  dplyr::count()
  dplyr::slice_min(order_by = Cereales)
  
# 11. Consumo promedio de pescado, carnes rojas y blancas en Europa del Este, Europa
# Occidental y Países Nórdicos.

data_analisis %>% 
  dplyr::group_by(Categoria) %>% 
  dplyr::summarize_at(vars(Pescado, Rojas, Blancas, -Pais), list(~mean(.x)))

data_analisis %>% 
  dplyr::group_by(Categoria) %>% 
  dplyr::summarize(Carnes_Rojas = mean(Rojas),
            Carnes_Blancas = mean(Blancas),
            Pescado = mean(Pescado))

# 12. ¿Cuántos países son mayores o iguales a la mediana del consumo calórico de leche y huevos?

data_analisis %>% 
  dplyr::filter(Leche >= median(Leche) & Huevos >= median(Huevos)) %>% 
  nrow()

data_analisis %>% 
  dplyr::filter(Leche >= median(Leche) & Huevos >= median(Huevos)) %>% 
  dplyr::count()

# 13. IQR

LI <- quantile(data_analisis$Huevos, probs = 0.25) - 1.5 * 
  (quantile(data_analisis$Huevos, probs = 0.75) - quantile(data_analisis$Huevos, probs = 0.25))

LS <- quantile(data_analisis$Huevos, probs = 0.75) + 1.5 * 
  (quantile(data_analisis$Huevos, probs = 0.75) - quantile(data_analisis$Huevos, probs = 0.25))

data[data$Huevos <= LI,][c(1,4)]
data$Pais[data$Huevos >= LS][c(1,4)]

# 14. ¿Qué continentes tienen un consumo mayor o igual al promedio de carnes rojas y menor
# o igual al consumo de azucares? 

data_analisis %>% 
  dplyr::filter(Rojas >= mean(Rojas, na.rm = TRUE) & 
           Azucares <= mean(Azucares, na.rm = TRUE)) %>% 
  dplyr::group_by(Categoria) %>% 
  dplyr::count()

# 15. ¿En qué país e ítem de alimento se encuentra el mayor consumo de calorías?

data_analisis %>% 
  tidyr::pivot_longer(cols = c(2:10), 
               names_to = "Consumo",
               values_to = "Calorías") %>% 
  dplyr::rename(Continente = Categoria) %>% 
  dplyr::filter(Calorías >= max(Calorías, na.rm = TRUE))


