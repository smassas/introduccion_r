# Sesión 1 --------------------------------------------------------

# R como calculadora simple.

5+5
25/5
2*2
27-2

# Operatoria con más cálculos

sqrt(2^4 + exp(3)/55 - log(5*8-2))
12*(7+2)+(45-32)+8
30-(-2)*(-10)+(-5)*(-2)
520 + 202 * log(25)
2 + 4 * exp(3)
22^2 - 2^2
5 + (5 * 10 + 2 * 3)
log(5) + pi/sqrt(5)
52 + 203 + 1002 + 204

# R como calculadora lógica

5 == 5 # igualdad
25 >= 2 # mayor o igual que
25 <= 2 # menor o igual que
25 != 10 # no es igual a
p = 20; y = 5; p <= y # operatoria en objetos


# Actividad 1: Cálculo aritmético -------------------------------------------------------------

520 + 202 * log(25)
2 + 4 * exp(3)
2^2* sqrt(2-sqrt(2))
5 + (5 * 10 + 2 * 3)^2
log(5) + pi/sqrt(5)
5**2 + 20**3 + 100**2 + 20**4

# Actividad 2: Lógica orientada a objetos -------------------------------------------------------------

# ¿Cuál es el valor de a y b? Si a <- 5; b <- a; a <- 4

a <- 5
b <- a
a <- 4
print(a)

# Sea x = 30, w = 5 y z = a^2, ¿qué resultado obtenemos de x * y + z?

x <- 30
w <- 5
z <- a^2
z*y+z

# Almacenar en un objeto el resultado anterior llamado variable_1.

variable_1 <- z*y+z

# ¿Qué resultado obtenemos al dividir variable_1 y z?

variable_1/z

# Asignar el valor 20000 a la variable ventas_2020

ventas_2020 <- 20000

# Ejecutar variable ventas_2020

ventas_2020

# Asignar el valor 30000 a la variable ventas_2021

ventas_2021 <- 30000

# Ejecutar la variable ventas_2021

ventas_2021

# Actividad 3: Secuencias y repeticiones ---------------------------------

#Repetición secuencia 1 2 3 4 1 2 3 4
rep(x=1:4, times=2)
rep(1:4, times = 2)
rep(seq(1:4), 2)

#Repetición secuencia 1 1 2 3 3 4 4
rep(x=1:4, times=c(2,1,2,2))

#Repetición secuencia 1 1 2 3 3 4
rep(x=1:4, times=c(2,1,2,1))

#Repetición secuencia 1 1 2 2 3 3 4
rep(x=1:4, each=2)

# Once valores igualmente espaciados desde 0 hasta 1 usando la función seq()
seq(from=0, to=1, length.out = 11)

# Una secuencia de dos en dos comenzando en 1 y finalizando en 200.
seq(from=1, to=9, by=2)  

# Una secuencia desde 1 con un salto de pi y sin pasar del número 9.
seq(from=1, to=9, by=pi) 

# Crear un vector con los números de 1 a 17 y extraer los números que 
# son mayores o iguales a 12.

which(seq(from = 1, to = 17, by = 1) > 12)

# Actividad 4: Pérdidas y ganancias retail  -------------------------------------------------------------

# Desea analizar la evolución semanal de ganancias y pérdidas de 2
# departamentos de retail. A continuación se observan los indicadores.

departamento_1 <- c(140, -50, 20, -120, 240)
departamento_2 <- c(-24, -50, 100, -350, 10)
names(departamento_1) <- c("Lun", "Mar", "Mie", "Jue", "Vie")
names(departamento_2) <- c("Lun", "Mar", "Mie", "Jue", "Vie")

# 1. Comparar los días de semana por departamento 1 y 2.

departamento_1 > departamento_2

# 2. ¿Qué departamento tuvo menos pérdidas? 
#Calcular total de pérdida y ganancia en cada departamento.

sum(departamento_1) 
sum(departamento_2)

# 3. Calcular día donde hubo más ganancia y pérdida por departamento.

which.min(departamento_1) #Posición en el que se encuentra la mayor pérdida
which.min(departamento_2) #Posición en el que se encuentra la mayor pérdida
which.max(departamento_1) #Posición en el que se encuentra la mayor ganancia
which.max(departamento_2) #Posición en el que se encuentra la mayor ganancia

# 4. Calcular total ganancias departamento 1 y departamento 2

sum(departamento_1, departamento_2)
rentabilidad <- sum(departamento_1) + sum(departamento_2)

# Actividad 5: Contagios covid --------------------------------------------

# Número de casos Covid-19 diarios según Comuna.

Las_Condes <- c(80, 90, 50, 40, 35)
La_Florida <- c(75, 68, 50, 90, 98)

# Crear vector que contenga los días de la semana.
Dias_contagios <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes")

# Asignar los días de la semana al vector Las_Condes y vector La_Florida.
names(Las_Condes) <- Dias_contagios
names(La_Florida) <-  Dias_contagios

# Calcule el total de contagios semanales por comuna.
Total_Las_Condes <- sum(Las_Condes)
Total_La_Florida <- sum(La_Florida)

# Determinar si los contagios en Las Condes son mayores a La Florida

Total_Las_Condes > Total_La_Florida

# Determinar qué día de la semana se encuentra la mayor cantidad de contagios, 
# según comuna.

Las_Condes[which.max(Las_Condes)]
La_Florida[which.max(La_Florida)]

# Elementos adicionales. 

# ¿Qué días fueron mayores a 75 casos diarios por cada comuna?

Las_Condes[which(Las_Condes > 75)]
La_Florida[which(La_Florida > 75)]

names(Las_Condes)[Las_Condes > 75]
names(La_Florida)[La_Florida > 75]

# ¿Qué días fueron menores a 40 casos en la comuna de Las Condes?
Las_Condes[which(Las_Condes < 75)]

# ¿Qué días fueron menores a 68 casos en la comuna de La Florida?
Las_Condes[which(Las_Condes > 75)]


# Actividad 6: Subsetting -------------------------------------------------

Conteo <- data.frame(Candidato = c("Jadue", "Boric"),
                     Votos = c("64000", "78000"),
                     Mesas = c("Mesa1", "Mesa1"))

# 1. Seleccionar solo la columna votos
Conteo[, "Votos"]
Conteo[, 2]

# 2. Seleccionar la fila 1 y columna 1 y 2.
Conteo[1, c(1:2)]
Conteo[1, c(1,2)]
Conteo[1, c("Candidato", "Votos")]

# 3. Seleccionar columna 2 mediante operador $.
Conteo$Votos

# 4. Selccionar la fila 1 y columna 1 y 3.
Conteo[1, c(1,3)]
Conteo[1, c("Candidato", "Mesas")]

# 5. Seleccionar fila 2, además de la columna 3.
Conteo[2, 3]

# Síntesis subsetting -----------------------------------------------------

paises <- datos::paises

paises$anio >= 2000 #retornamos TRUE y FALSE
paises[paises$anio >= 2007,] 
paises[paises$anio >= 2007, c(1,3)]  #retornemos solo los países y el filtro.
paises[paises$esperanza_de_vida >= 80, c("pais", "continente")]
paises[paises$pib_per_capita >= 1000 & paises$continente == "Europa",]
paises[!(paises$pib_per_capita >= 1000 & paises$continente == "Europa"),]
subset(x = paises, subset = pais == "Chile", select = c(1:5))

# Vectores ----------------------------------------------------------------

# 1. Usar "seq()" para crear un vector de 10 números con espacio desde 0 a 12.
vec_num <- seq(from = 0, to = 12, length.out = 10)
trunc(vec_num, 2)

# 2. Usar ":" para crear un vector de valores enteros entre el 31 y 40
vec_int <- c(31:40)

# 3. Usar "LETTERS" ,"[ ]" y "c()" para crear un vector con 9 letras comenzando desde la "C" y que además contenga la "Z"
vec_cha <- c(LETTERS[3:11], "Z")
length(vec_cha)
#4. Usar "letters" y "[ ]" para crear un vector de tipo factor con las primeras 10 letras minusculas.
vec_fac <- factor(letters[1:10])
length(vec_fac)
# 5. Combinar los vectores obtenidos en (3) y (4) usando "c()". No convertir a factor.
vec_let <- c(vec_cha, vec_fac)

# 6. combinar usando "c()" y "[ ]" los primero 4 elemento de "vec_num" 
# con los ultimos 4 elementos de "vec_int"
c(vec_num[1:4], vect_int[1:4])

# 7. Usar "rev()" para revertir el orden del vector obtenido en (4).
fac_rev <- rev(vec_fac)

# extra: sort()
sort(vec_fac)

# Matrices ----------------------------------------------------------------

# 1. Crear una matriz con 10 filas  y 4 columnas llenas de NA usando "matrix()"
mat.0 <- matrix(nrow = 10, ncol = 4, data = NA)

# 2. Asignar "vec_num" a la primera columna de "mat_1"
mat_1 <- mat.0 # no editar
mat_1[,1] <- vec_num
mat_1 
# 3. Asignar "vec_int" a la ultima columna de mat_2
mat_2 <- mat_1 # no editar
dim(mat_1)
mat_2[,2] <- vec_int
mat_2
# 4. Asignar "vec_cha" y "vec_fac" a las restantes columnas de "mat_2" para obtener "mat_3".
mat_3 <- mat_2 # no editar.
mat_3[,3:4] <- c(vec_cha, vec_fac)
mat_3

# 5. Selecionar la sexta fila de la matriz y guardar en "fila_6" como vector.
fila_6 <- mat_3[6,]

# 6. extraer el elemento asignado en la fila 5 y columna 3 como valor numerico.
valor_5.3 <-  mat_3[5,3] # solo nos indica la letra "g"

# buscamos la posicion de la letra "e"    # Hint: which()
valor_5.3 <- which(mat_3 == "G", arr.ind = TRUE)

# 7. Usando "cbind()" combinar "vec_num", "vec_int", "vec_cha", y "vec_fac" en "mat_4".
mat_4 <- cbind(vec_num, vect_int, vec_cha, vec_fac)

# 8. Reordenar las columnas de "mat_4" para que sea igual a "mat_3"
mat_ord <- cbind(mat_4, mat_3)
mat_ord[4]

# 9. Trasponer la matriz mat_4 y extraer solo las primeras 4 columnas. 
# Almacenar en vector mat_ord.

mat_ord <- t(mat_4)[, 1:4]
mat_t <- t(mat_4)[, 6:10]

# 10. Usar rbind() y añadir mat_4 y mat_3
mat_final <- rbind(mat_4, mat_3)
mat_final

# 11. Asignar filas y columnas con comando paste() y rep()

row.names(mat_final) <- paste("fila", rep(1:20))
colnames(mat_final) <- paste("columna", rep(1:4))
mat_final

# Atajos ------------------------------------------------------------------

# Comentarios en R

# Cmd + Shift + C (Mac) 
# Ctrl + Shift + C (Windows).

# Seccionar líneas de código

# Cmd + Shift + R (Mac) 
# Ctrl + Shift + R (Windows).

# Instalar y leer librería

# install.packages("nombre del paquete")
# library("nombre de librería")

# Ejecutar una línea de código

# Opción 1: Click en Run 
# Opción 2: Ctrl + Enter (Windows)
# Opciób 3: Cmd + Enter (Mac)



