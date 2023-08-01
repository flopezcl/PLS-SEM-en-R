# C. Datos Faltantes

## C.1 Obtener columnas con datos faltantes

#Con la siguiente función obtenemos un resumen de datos faltantes por columnas
nan <- function(df) {
  nulos <- list()
  for (i in 1:length(df)) {
    if (sum(is.na(df[[i]])) != 0) {
      nulos[[length(nulos) + 1]] <- c(names(df)[i], sum(is.na(df[[i]])))
    }
  }
  print(nulos)
}

nan(pls_data)
#Nota: Si aparece list() no hay datos faltantes

## C.2 Reemplazar datos faltantes por -99

#A continuación reemplazaremos los datos faltantes por -99, este valor será el que utilizaremos en el algoritmo.

reemp_falt <- function(df) {
  for (i in 1:length(df)) {
    if (sum(is.na(df[[i]])) != 0) {
      df[[i]] <- replace(df[[i]], is.na(df[[i]]), -99)
    }
  }
  return(df)
}

pls_data2 <-reemp_falt(pls_data2)
#Nota 1: Si cambia a un valor distinto(-99), luego al estimar el modelo se deberá indicar el valoir utilizado.
#Nota 2: Recordar que el modelo PLS-SEM realiza un reemplazo de estos valores faltantes.

## C.3 Eliminar datos faltantes o con una condicion

#Eliminar los Datos que no se desea utilizar


pls_data2$PE1 == "-99"  #Buscar un dato en una columna especifica
pls_data2 <- pls_data2[(pls_data2$PE1 != "-99"),]  #Eliminación de esos datos del dataset

