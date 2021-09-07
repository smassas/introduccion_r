## Vectores ----

# 1. Usar "seq()" para crear un vector de 10 números con espacio desde 0 a 12.
vec_num <- seq(from = 10, to = 12, length.out = 10)
# 2. Usar ":" para crear un vector de valores enteros entre el 31 y 40
vec_int <- 31:40
# 3. Usar "LETTERS" ,"[ ]" y "c()" para crear un vector con 9 letras comenzando desde la "C" y que además contenga la "Z"
vec_cha <- c(LETTERS[3:11], "Z")
# 4. Usar "letters" y "[ ]" para crear un vector de tipo factor con las primeras 10 letras minúsculas.
vec_fac <- factor(letters[1:10])
# 5. Combinar los vectores obtenidos en (3) y (4) usando "c()". No convertir a factor.
vec_let <- c(vec_cha, vec_fac)
# 6. Combinar usando "c()" y "[ ]" los primero 4 elemento de "vec_num" con los ultimos 4 elementos de "vec_int"
vec_comb <- c(vec_num[1:4], vec_int[7:10])

# Comentarios:
  
# Letters y LETTERS son las letras del abecedario (minúscula y mayúscula, respectivamente).
# Si ejecuta letters, podrá visualizar las letras en su consola.
  
# Las secuencias pueden ser creadas con la función seq() o con el comando :
# Ejemplo: seq(from = 1, to = 4) o 1:4. El problema de esta última forma, es 
# que no podrá establecer otros criterios.

# Matrices ----

# 1. Crear una matriz con 10 filas  y 4 columnas llenas de NA usando "matrix()"
mat.0 <- matrix(data = NA, nrow = 10, ncol = 4)
# 2. Asignar "vec_num" a la primera columna de "mat_1"
mat_1 <- mat.0 # no editar
mat_1[,1] <- vec_num
mat_1
# 3. Asignar "vec_int" a la segunda columna de mat_2
mat_2 <- mat_1 # no editar
mat_2[,2] <- vec_int
mat_2
# 4. Asignar "vec_cha" y "vec_fac" a las restantes columnas de "mat_2" para obtener "mat_3".
mat_3 <- mat_2 # no editar.
mat_3[,3] <- vec_cha
mat_3[,4] <- vec_fac
mat_3
# 5. Selecionar la sexta fila de la matriz y guardar en "fila_6" como vector.
fila_6 <- mat_3[6,]
fila_6
# 6. extraer el elemento asignado en la fila 5 y columna 3 como valor numerico.
valor_5.3 <- mat_3[5,3]
# 7. Usando "cbind()" combinar "vec_num", "vec_int", "vec_cha", y "vec_fac" en "mat_4".
mat_4 <- cbind(vec_num, vec_int, vec_cha, vec_fac)
# 9. Reordenar las columnas de "mat_4" para que sea igual a "mat_3"
mat_ord <- cbind(mat_4, mat_3)
# 9. Trasponer la matriz mat_4 y extraer solo las primeras 4 columnas. Almacenar en vector mat_ord.
mat_ord <- t(mat_4)[, 1:4]
# 10. Usar rbind() y añadir mat_4 y mat_3
mat_final <- rbind(mat_4, mat_3)
mat_final
# 11. Asignar filas y columnas con comando paste() y rep()
rownames(mat_final) <- paste("fila", rep(1:20))
colnames(mat_final) <- paste("columna", rep(1:4))
mat_final

## Listas ---
  
# 1. Usar "list()" para crear una lista que contenga "vec_num" and "fila_6", asignándole los mismos nombres
list_1 <- list(vec_num, fila_6)
list_1 
# 2. Usando "$", extraer "fila_6" de "list_1" y asignar  "fila_6_2".
fila_6_2 <- list_1
fila_6_2 <- fila_6_2[[2]]
# 3. Crear otra lista que contenga "valor_5.3" and "mat_final".
list_2 <- NULL
list_2 <- list(valor_5.3, mat_final)
# 4. Combinar "list_1" y "list_2" usando "c()"
list_3 <- NULL
list_3 <- c(list_1, list_2)
# 5. Usar "unlist()" para convertir "list_3" en un vector
vector_3 <- NULL 
vector_3 <- unlist(list_3)
vector_3
# 6. Usar "as.list()"para convertir "vector_3" en una lista 
list_grande <- NULL
list_grande <- as.list(vector_3)




  
  

