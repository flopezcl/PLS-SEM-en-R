# A. Carga de libreria y directorio a trabajar

## A.1 Carga de librerias

# install.packages('seminr')
# install.packages('xlsx')
#install.packages("genpathmox")
#install.packages("cSEM")
#install.packages("psych")
library(xlsx)   # Libreria para exportar a Excel
library(psych)  #libreria a utilizar para obtener datos de la variables como Simetria y curtosis

library(seminr)  # Libreria para ecuaciones estructurales 
library(cSEM)    # Libreria para ecuaciones estructurales, trabaja con estructura Lavaan

library(genpathmox)   # Libreria para análisis Pathmox 



## A.2 Carga de datos

### A.2.1 Carga de directorio de trabajo y datos

#Reemplace directorio de trabajo


getwd()
directorio <- "P:/R_Proyect/PLS-SEM/Proyecto" ## Reemplace por su directorio
setwd(directorio)  # si desea dejar fijo el directorio de trabajo
getwd()

#En file sustituya por archivo de datos
#Archivo debe ser de tipo csv con separación ";" 

pls_data <- read.csv(file = "P:/R_Proyect/PLS-SEM/Proyecto/2023.TRI_MGA.csv", header = TRUE, sep = ';')
dim(pls_data)  ## Ver cantidad de filas y columnas

# Ver cabecera de los datos y tipos
head(pls_data)  ### Primeros datos
str(pls_data) ### Tipo de datos
nrow(pls_data) ## numero filas
ncol(pls_data) ## numero Columnas

#Crearemos una copia de la tabla en la que haremos los cambios
  
pls_data2 <-pls_data


### A.2.2 Corrección de datos

#Cambiar nombre a una variable


names(pls_data2)
names(pls_data2)[1] = 'indice'
names(pls_data2)

#Corregir nombre de la Región Bío-Bío

table(pls_data2$REGION) #Vemos composición de la tabla 
pls_data2$REGION =ifelse(pls_data2$REGION=='BiobÃ­o', 'Bio-Bio', pls_data2$REGION) # reemplazamos si es 'BiobÃ­o' por 'Bio-Bio'
table(pls_data2$REGION) #Vemos composición de la tabla 


#Crear o modificar el tipo de dato dejándolo como Numerico

pls_data2$AA <- as.integer(pls_data2$BORN)  #AA es el nuevo campo a crear extraido desde BORN.

pls_data2$TRI1 <- as.integer(pls_data2$TRI1) #En este caso estamos modificamos un campo existente sin crear uno nuevo. 
