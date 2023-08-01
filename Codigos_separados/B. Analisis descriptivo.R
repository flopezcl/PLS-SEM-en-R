# B. Estadistica descriptiva

## B.1 Gráficos tablas

#Crear tabla de frecuencia con variable categóricas

### B.1.1 Gráfico de frecuencia
#Gráfico 1 nivel de educación 
tab1 <- table(pls_data2$EDU) #creación de tabla de frecuencia

#gráfico usando barplot de r
barplot(tab1,
        main = "Cantidad de datos por niveles de enseñanza",
        xlab = "Nivel de enseñanza",
        ylab = "Cantidad",
        col = c("red", "green", "blue", 'yellow'),
)


#Gráfico 2 Estado civil

tab2 <- table(pls_data2$SOC)

barplot(tab2,
        main = "Cantidad de datos por Estado Civil",
        xlab = "Estado civil",
        ylab = "Cantidad",
        col = c("red", "green", "blue", 'yellow', 'brown', 'orange'),
)

#Gráfico 3 Género

tab3 <- table(pls_data2$GENDER)

barplot(tab3,
        main = "Cantidad de datos por Género",
        xlab = "Género",
        ylab = "Cantidad",
        col = c("red", "green", "blue", 'yellow', 'brown', 'orange'),
)

#Gráfico 4 por Generación 
tab4 <- table(pls_data2$GENERATION)

barplot(tab4,
        main = "Cantidad de datos por Generación",
        xlab = "Generación",
        ylab = "Cantidad",
        col = c("red", "green", "blue", 'yellow', 'brown', 'orange'),
)

#Gráfico 5 Porcentaje en esfera (pie) por generación
porcentaje <- round(tab4 / sum(tab4) * 100, 2)
colores <- rainbow(length(tab4))
pie(porcentaje, labels = paste0(porcentaje, "%"), main = "Porcentaje de Generación", col = colores)
legend("right", legend = names(tab4), cex = 0.8, fill = colores)


#Gráfico 6 por región 
tab4c <- table(pls_data2$REGION)  #Creamos 

barplot(tab4c, main = "Cantidad de datos por Región",
        xlab = "Región", ylab = "Frecuencia", col = rainbow(2))


### B.1.2 Pruebas normalidad
#Gráficos 7 por año de nacimiento 

#Análisis de normalidad 

#Box-Plot por año de nacimiento 
boxplot(pls_data2$BORN, main = "Gráfico de cajas Año nacimiento",
        outline = TRUE)

#Histograma Año de nacimiento
hist(pls_data2$BORN, main = "Histograma Año nacimiento",
     xlab = "Año de nacimiento",
     ylab = "Frecuencia",
     col = "red",
     border = "black")

#Obtención de Simetria y Curtosis de los años de nacimiento 
library('psych') # Utilizamos libreria Psych
skew(pls_data2$BORN,) # Simetría
kurtosi(pls_data2$BORN,) #Curtosis
multi.hist(pls_data2$BORN,dcol= c("blue","red"),dlty=c("dotted", "solid")) 
# Test de Shapiro 
shapiro.test(pls_data2$BORN)
# Test de kolmogorov-smirnov
ks.test(pls_data2$BORN, "pnorm", mean(pls_data2$BORN), sd(pls_data2$BORN))

#Con los siguientes comandos se pueden realizar pruebas adicionales de normalidad
#se requiere paquete nortest
# require(nortest)
# ad.test(pls_data2$BORN) #test de Anderson-Darling
# cvm.test(pls_data2$BORN) #test de Cramer von mises
# pearson.test(pls_data2$BORN) #Chi cuadrado de pearson


#Gráficos 8 por año de experiencia en internet 
#Box-Plot por año de experiencia en internet 
boxplot(pls_data2$EXP, main = "Gráfico de cajas Años de Experiencia en Internet",
        outline = TRUE)

#Gráfico de densidad por año de experiencia en internet 
densidad_EXP <- density(pls_data$EXP)
plot(densidad_EXP, 
     main = "Densidad Experiencia Internet",
     xlab = "Años Exp",
     ylab = "Densidad")

#Obtención de Simetria y Curtosis de los años de experiencia 
skew(pls_data$EXP) # Simetría
kurtosi(pls_data$EXP) #Curtosis

#Gráfico 9 Doble entrada Género y región
tabla1 <- table(pls_data2$REGION, pls_data2$GENDER) 
barplot(tabla1,
        main = "Gráfico por Género y Región",
        xlab = "Género", ylab = "Frecuencia",
        legend.text = rownames(tabla1),
        beside = TRUE,
        col = rainbow(2), label = TRUE)


### B.1.3 Gráfico de distribución y doble entrada
#Gráfico 10 Mosaico distribución de Género y generación
tabla2 <- table(pls_data2$GENERATION, pls_data2$GENDER)   

mosaicplot(tabla2, main = "Mosaico de Género y edad",
           color = TRUE)

#Gráfico 11 Doble entrada Género y nivel socioeconomico
tabla3 <- table(pls_data2$SOC, pls_data2$GENERATION)          
barplot(tabla3,
        main = "Gráfico por Generación y Nivel socioeconomico",
        xlab = "Generación", ylab = "Frecuencia",
        legend.text = rownames(tabla3),
        beside = FALSE,
        col = rainbow(5), label = TRUE)


## B.2 Separar variables categóricas

names(pls_data)  ### Ver los nombres de las columnas 
str(pls_data) ### Tipo de datos
#Separamos variables Demograficas
categoricas <- c( 'EDU', 'SOC', 'WSTATUS', 'RETIRED', 'GENDER', 'GENERATION', 'REGION' )

#Separamos variables del cuestionario utilizado
otras <- c("PE1"  ,      "PE2"    ,    "PE3"      ,  "PE4"     ,   "EE1"     ,   "EE2"    ,    "EE3"   ,     "SI1"   ,     "SI2"  ,      "SI3",
           "SI4"   ,     "FC1"   ,     "FC2"  ,     "FC3" ,     "HM1",       "HM2"  ,    "HM3"  ,    "HA1"   ,    "HA2"  ,    "HA3"  ,     "HA4",
           "HA5"     ,   "IU1"   ,    "IU2"    ,   "U1"    ,    "U2"  ,     "U3"     ,   "U4"    ,    "TRI1"  ,    "TRI2" ,     "TRI3",     "TRI4" ,
           "TRI5"     ,  "TRI6" ,     "TRI7"    , "TRI8"    , "TRI9"   ,   "TRI10"    ,"TRI11"    ,"TRI12"     , "TRI13"   , "TRI14",   "TRI15",
           "TRI16")

## B.3 Generar resumen de campos no categóricos

resumen <- summary(pls_data2[,otras])
print(resumen)


#Exportar a Excel con datos resumen
library(xlsx)   # Utilizamos libreria para exportar datos a excel
write.xlsx2(x=resumen,   #Datos a exportar
            'resumen.xlsx',  #nombre del archivo excel
            sheetName = "resumen",  #nombre de la hoja 
            col.names = TRUE,  #Exportar nombre de columnas
            row.names = TRUE,  #Exportar nombre de filas 
            append = FALSE,    #Append a un archivo existente
            showNA = TRUE)    #Mostrar N/A
       



## B.4 Tablas de frecuencias

#Tablas de frecuencia de doble entrada. 
xtabs(~EDU + GENDER, data =pls_data2) ## Educacion y Género
xtabs(~GENDER + WSTATUS, data =pls_data2) ##Género y Estatus Laboral
xtabs(~GENDER + RETIRED, data =pls_data2) ##Género y Retirado
xtabs(~GENDER + GENERATION, data =pls_data2) ## Género y Generación
xtabs(~GENDER + REGION, data =pls_data2) ##Género y Región
xtabs(~REGION + GENERATION, data =pls_data2) ##Género y Región



