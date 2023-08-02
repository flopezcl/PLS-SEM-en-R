---
title: "Guía de PLS-SEM en R"
author: "Felipe A. López"
date: "julio de 2023"
---

**NOTA:** Se entiende que usted maneja los conceptos básicos de ecuaciones estructurales y que realizó la limpieza y validación de sus datos.

# A. Carga de librería y directorio a trabajar

## A.1 Carga de librerías

```{r message = FALSE, warning=FALSE}
# install.packages(pkgs = 'seminr')
# install.packages(“xlsx”)
#install.packages("genpathmox")
#install.packages("cSEM")
#install.packages("psych")
library(seminr)
library(xlsx)

library(cSEM)
library(genpathmox)

library('psych')

```

## A.2 Carga de datos

### A.2.1 Carga de directorio de trabajo y datos

Reemplace directorio:

```{r directorio}
getwd()
directorio <- "P:/R_Proyect/PLS-SEM/Proyecto" ## Reemplace por su directorio
#setwd(directorio)  # si desea dejar fijo el directorio de trabajo
getwd()

```

En file sustituya por archivo de datos

```{r Carga datos}
pls_data <- read.csv(file = "P:/R_Proyect/PLS-SEM/Proyecto/2023.TRI_MGA.csv", header = TRUE, sep = ';')
dim(pls_data)  ## Ver cantidad de filas y columnas
```

Ver cabecera de los datos y tipos

```{r}
head(pls_data)  ### Primeros datos

str(pls_data) ### Tipo de datos

nrow(pls_data) ## numero filas
ncol(pls_data) ## numero Columnas

```

**Crearemos una copia de la tabla en la que haremos los cambios**

```{r}
pls_data2 <-pls_data
```

### A.2.2 Corrección de datos

Cambiar nombre a una variable

```{r}
names(pls_data2)
names(pls_data2)[1] = 'indice'
```

Corregir nombre de la Región Bío-Bío

```{r}
table(pls_data2[,54])
table(pls_data2$REGION)
pls_data2$REGION =ifelse(pls_data2$REGION=='BiobÃ­o', 'Bio-Bio', pls_data2$REGION)
```

Crear o modificar el tipo de dato dejándolo como numérico

```{r}
pls_data2$AA <- as.integer(pls_data2$BORN)  #AA es el nuevo campo a crear extraído desde BORN.

pls_data2$TRI1 <- as.integer(pls_data2$TRI1) #En este caso estamos modificamos un campo existente sin crear uno nuevo. 

```

# B. Estadística descriptiva

## B.1 Gráficos tablas

Crear tabla de frecuencia con variable categóricas

```{r message=FALSE, warning=FALSE}
table(pls_data2$EDU)
tab1 <- table(pls_data2$EDU)
head(tab1)

barplot(tab1,
        main = "Cantidad de datos por niveles de enseñanza",
        xlab = "Nivel de enseñanza",
        ylab = "Cantidad",
        col = c("red", "green", "blue", 'yellow'),
)
```

```{r message=FALSE, warning=FALSE}
table(pls_data2$SOC)
tab2 <- table(pls_data2$SOC)
head(tab2)

barplot(tab2,
        main = "Cantidad de datos por Estado Civil",
        xlab = "Estado civil",
        ylab = "Cantidad",
        col = c("red", "green", "blue", 'yellow', 'brown', 'orange'),
)
```

```{r message=FALSE, warning=FALSE}
table(pls_data2$GENDER)
tab3 <- table(pls_data2$GENDER)
head(tab3)

barplot(tab3,
        main = "Cantidad de datos por Género",
        xlab = "Género",
        ylab = "Cantidad",
        col = c("red", "green", "blue", 'yellow', 'brown', 'orange'),
)
```

```{r message=FALSE, warning=FALSE}
table(pls_data2$GENERATION)
tab4 <- table(pls_data2$GENERATION)
head(tab4)

barplot(tab4,
        main = "Cantidad de datos por Generación",
        xlab = "Generación",
        ylab = "Cantidad",
        col = c("red", "green", "blue", 'yellow', 'brown', 'orange'),
)
```

```{r message=FALSE, warning=FALSE}
tab4c <- table(pls_data2$REGION)

barplot(tab4c, main = "Cantidad de datos por Región",
     xlab = "Región", ylab = "Frecuencia", col = rainbow(2))
```

```{r message=FALSE, warning=FALSE}
porcentaje <- round(tab4 / sum(tab4) * 100, 2)
colores <- rainbow(length(tab4))
pie(porcentaje, labels = paste0(porcentaje, "%"), main = "Porcentaje de Generación", col = colores)
legend("right", legend = names(tab4), cex = 0.8, fill = colores)
```

### B.1.2 Pruebas normalidad

```{r message=FALSE, warning=FALSE}
boxplot(pls_data2$BORN, main = "Gráfico de cajas Año nacimiento",
        outline = TRUE)
```

```{r message=FALSE, warning=FALSE}
hist(pls_data2$BORN, main = "Histograma Año nacimiento",
     xlab = "Año de nacimiento",
     ylab = "Frecuencia",
     col = "red",
     border = "black")
```

```{r message=FALSE, warning=FALSE}
densidad_BORN <- density(pls_data$BORN)
plot(densidad_BORN, 
     main = "Densidad Experiencia Internet",
     xlab = "Años Exp",
     ylab = "Densidad")
```

```{r message=FALSE, warning=FALSE}
skew(pls_data2$BORN,) # Simetría
kurtosi(pls_data2$BORN,)
```

```{r}
multi.hist(pls_data2$BORN,dcol= c("blue","red"),dlty=c("dotted", "solid")) 
# Test de Shapiro 
shapiro.test(pls_data2$BORN)
# Test de kolmogorov-smirnov
ks.test(pls_data2$BORN, "pnorm", mean(pls_data2$BORN), sd(pls_data2$BORN))

#Con los siguientes comandos se pueden realizar pruebas adicionales de normalidad
#requiere paquete nortest

# require(nortest)
# ad.test(pls_data2$BORN) #test de Anderson-Darling
# cvm.test(pls_data2$BORN) #test de Cramer von mises
# pearson.test(pls_data2$BORN) #Chi cuadrado de pearson
```

```{r message=FALSE, warning=FALSE}
boxplot(pls_data2$EXP, main = "Gráfico de cajas Años de Experiencia en Internet",
        outline = TRUE)

```

```{r message=FALSE, warning=FALSE}
densidad_EXP <- density(pls_data$EXP)
plot(densidad_EXP, 
     main = "Densidad Experiencia Internet",
     xlab = "Años Exp",
     ylab = "Densidad")
```

```{r}
skew(pls_data$EXP) # Simetría
kurtosi(pls_data$EXP)

```

### B.1.3 Otros gráficos

```{r}
tabla1 <- table(pls_data2$REGION, pls_data2$GENDER) 
barplot(tabla1,
        main = "Gráfico por Género y Región",
         xlab = "Género", ylab = "Frecuencia",
         legend.text = rownames(tabla1),
         beside = TRUE,
         col = rainbow(2), label = TRUE)
```

```{r}
tabla2 <- table(pls_data2$GENERATION, pls_data2$GENDER)   

mosaicplot(tabla2, main = "Mosaico de Género y edad",
         color = TRUE)
```

```{r}
tabla3 <- table(pls_data2$SOC, pls_data2$GENERATION)          
barplot(tabla3,
        main = "Gráfico por Generación y Nivel socioeconomico",
        xlab = "Generación", ylab = "Frecuencia",
        legend.text = rownames(tabla3),
        beside = FALSE,
         col = rainbow(5), label = TRUE)
```

## B.2 Separar variables categóricas

```{r}
names(pls_data)  ### Ver los nombres de las columnas 
str(pls_data) ### Tipo de datos
categoricas <- c( 'EDU', 'SOC', 'WSTATUS', 'RETIRED', 'GENDER', 'GENERATION', 'REGION' )
otras <- c("PE1"  ,      "PE2"    ,    "PE3"      ,  "PE4"     ,   "EE1"     ,   "EE2"    ,    "EE3"   ,     "SI1"   ,     "SI2"  ,      "SI3",
           "SI4"   ,     "FC1"   ,     "FC2"  ,     "FC3" ,     "HM1",       "HM2"  ,    "HM3"  ,    "HA1"   ,    "HA2"  ,    "HA3"  ,     "HA4",
           "HA5"     ,   "IU1"   ,    "IU2"    ,   "U1"    ,    "U2"  ,     "U3"     ,   "U4"    ,    "TRI1"  ,    "TRI2" ,     "TRI3",     "TRI4" ,
           "TRI5"     ,  "TRI6" ,     "TRI7"    , "TRI8"    , "TRI9"   ,   "TRI10"    ,"TRI11"    ,"TRI12"     , "TRI13"   , "TRI14",   "TRI15",
           "TRI16")
```

## B.3 Generar resumen de campos no categóricos

```{r}
resumen <- summary(pls_data2[,otras])
print(resumen)

```

Exportar a Excel con datos resumen

```{r}
write.xlsx2(x=resumen, 
            'resumen.xlsx', 
            sheetName = "resumen", 
            col.names = TRUE,
            row.names = TRUE, 
            append = FALSE, 
            showNA = TRUE, 
            password = NULL)

```

## B.4 Tablas de frecuencias

```{r}
xtabs(~EDU + GENDER, data =pls_data2) ## Educación y Género
xtabs(~GENDER + WSTATUS, data =pls_data2) ##Género y Estatus Laboral
xtabs(~GENDER + RETIRED, data =pls_data2) ##Género y Retirado
xtabs(~GENDER + GENERATION, data =pls_data2) ## Género y Generación
xtabs(~GENDER + REGION, data =pls_data2) ##Género y Región
xtabs(~REGION + GENERATION, data =pls_data2) ##Género y Región

```

# C. Datos Faltantes

## C.1 Obtener columnas con datos faltantes

Si aparece list() no hay datos faltantes

```{r}
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
```

## C.2 Reemplazar datos faltantes por -99

Si cambia a un valor distinto, luego al estimar modelo cambiar.

```{r}
reemp_falt <- function(df) {
  for (i in 1:length(df)) {
    if (sum(is.na(df[[i]])) != 0) {
      df[[i]] <- replace(df[[i]], is.na(df[[i]]), -99)
    }
  }
  return(df)
}

pls_data2 <-reemp_falt(pls_data2)
```

## C.3 Eliminar datos faltantes o con una condición

Eliminar los que se desea

```{r}
pls_data2$PE1 == "-99"
pls_data2 <- pls_data2[(pls_data2$PE1 != "-99"),]
```

# D. Crear una variable categórica desde otra variable

## D.1 Convertir en categóricas

```{r}

str(pls_data2)
pls_data2$EDU3=pls_data2$EDU
pls_data2$SOC3= pls_data2$SOC
pls_data2$EXP3= pls_data2$EXP
pls_data2$EDU2= as.factor(pls_data2$EDU)
pls_data2$SOC2= as.factor(pls_data2$SOC)
pls_data2$EXP2= as.factor(pls_data2$EXP)

```

## D.2 Cambiar variables categóricas

```{r}
pls_data2$GENERO= ifelse(pls_data2$GENDER=='Male', 1, 2)
pls_data2$REGION3= ifelse(pls_data2$REGION=='Coquimbo', 1, 2)
pls_data2$WSTATUS3= ifelse(pls_data2$WSTATUS=='N', 1, 2)
pls_data2$RETIRED3= ifelse(pls_data2$RETIRED=='N', 1, 2)
pls_data2$GENERATION3= ifelse(pls_data2$GENERATION=="Silent generation ", 1,pls_data2$GENERATION)
pls_data2$GENERATION3= ifelse(pls_data2$GENERATION=="Late Baby boomer ", 3, pls_data2$GENERATION3)
pls_data2$GENERATION3= ifelse(pls_data2$GENERATION=="Early Baby boomer ", 2,pls_data2$GENERATION3)
pls_data2$GENERATION3 <- as.numeric(pls_data2$GENERATION3)
#pls_data2$WSTATUS2= as.factor(pls_data2$WSTATUS)
#pls_data2$REGION2= as.factor(pls_data2$REGION)
pls_data2$GENERO3= pls_data2$GENERO
#pls_data2$GENERATION2= as.factor(pls_data2$GENERATION)
#pls_data2$RETIRED2 = as.factor(pls_data2$RETIRED)
```

# E. Modelo de ecuaciones estructurales (semir)

## E.1 Crear el modelo de medida

Por defecto se crean como reflectivo, para crear formativo agregar "weights = mode_B"

### E.1.1 PLS Normal

```{r}
## Reflectivo = mode_A  (default)
## Formativo  = mode_B  (weights = mode_B)


modelo_medida <- constructs(
  composite('PE', multi_items('PE', 1:4), weights = mode_A),
  composite('EE', multi_items('EE', 1:3)),
  composite('SI', multi_items('SI', 1:4)),
  composite('FC', multi_items('FC', 1:3)),
  composite('HM', multi_items('HM', 1:3)),
  composite('HA', multi_items('HA', 1:5)),
# composite('CUSA', single_item('cusa')),  # En el caso de ser un unico item dejar como single_item
  composite('IU', multi_items('IU', 1:2)),
  composite('SNS', multi_items('U', 1:4)) 
 )

plot(modelo_medida)
save_plot("modelo_medida.pdf")
```

### E.1.2 PLS Consistente

```{r}
modelo_medida <- constructs(
  reflective('PE', multi_items('PE', 1:4)),
  reflective('EE', multi_items('EE', 1:3)),
  reflective('SI', multi_items('SI', 1:4)),
  reflective('FC', multi_items('FC', 1:3)),
  reflective('HM', multi_items('HM', 1:3)),
  reflective('HA', multi_items('HA', 1:5)),
# composite('CUSA', single_item('cusa')),  # En el caso de ser un unico item dejar como single_item
  reflective('IU', multi_items('IU', 1:2)),
  reflective('SNS', multi_items('U', 1:4)) 
 )

plot(modelo_medida)
save_plot("modelo_medida.pdf")
```

## E.2 Crear Modelo estructural

```{r}
modelo_estruc <- relationships(
  paths(from = c('PE', 'EE', 'SI', 'FC', 'HM', "HA"), to = c('IU')),
  paths(from = c('FC', 'HA', "IU"), to = c('SNS'))
)
  
## ----- Generamos el modelo con colores
thm <- seminr_theme_create(plot.rounding = 2,  ## Decimales
                           plot.adj = FALSE, 
                           sm.node.fill = "cadetblue1",
                           mm.node.fill = "lightgray",
                           )

seminr_theme_set(thm)
## ----
plot(modelo_estruc, title =  "Fig. 1: Modelo Estructural")


save_plot("fig1.Modelo_Estructural.pdf")
```

## E.3 Estimación del Modelo

```{r message = FALSE, warning=FALSE}

estimacion_model <- estimate_pls(data = pls_data2,
                                      measurement_model = modelo_medida,  #Constructos
                                      structural_model = modelo_estruc,   # Caminos Path
                                      inner_weights = path_weighting,  
                                      # path_weighting para path weighting (default) o path_factorial para factor weighting,
                                      missing = mean_replacement, #Reemplazar los valores perdido mean es default
                                      missing_value = '-99' ) # Valores perdidos

summary_estimacion_model <- summary(estimacion_model)                                      


plot(estimacion_model, title =  "Fig. 2: Modelo Estimado")
save_plot("fig2.Modelo_Estimado.pdf")
```

## E.4 Reportes modelo

### E.4.1. Valores perdidos y estadísticas de cada variable

```{r}
summary_estimacion_model$descriptives$statistics  ## Valores perdidos y representación 

```

```{r}
x <- summary_estimacion_model$descriptives$statistics
write.xlsx2(x=x["items"], 
            'resumen.xlsx', 
            sheetName = "resumen_hor", 
            col.names = TRUE,
            row.names = TRUE, 
            append = TRUE, 
            showNA = TRUE, 
            password = NULL)

write.xlsx2(x=x["constructs"], 
            'resumen.xlsx', 
            sheetName = "resumen_hor_const", 
            col.names = TRUE,
            row.names = TRUE, 
            append = TRUE, 
            showNA = TRUE, 
            password = NULL)

```

### E.4.2. Número de iteraciones

**Nota***:* Si es mayor a 300 significa que no converge

```{r}
summary_estimacion_model$iterations  

```

### E.4.3. R\^2

**Exógenos**

```{r}

summary_estimacion_model$paths  

plot(summary_estimacion_model$paths[,1], pch = 2, col = "red", main="Betas y R^2 (Exógenos)", 
     xlab = "Variables", ylab = "Valores estimados", xlim = c(0,length(row.names(summary_estimacion_model$paths))+1)
     ) 
text(summary_estimacion_model$paths[,1],labels = row.names(summary_estimacion_model$paths) , pos = 4)


```

**Endógenos**

```{r}
plot(summary_estimacion_model$paths[,2], pch = 2, col = "red", main="Betas y R^2 (Endógenos)", 
     xlab = "Variables", ylab = "Valores estimados" , xlim = c(0,length(row.names(summary_estimacion_model$paths))+1) )
text(summary_estimacion_model$paths[,2],labels = row.names(summary_estimacion_model$paths) , pos = 4)

```

Exportar Excel

```{r}
write.xlsx2(x=summary_estimacion_model$paths, 
            'resumen.xlsx', 
            sheetName = "BetasyR", 
            col.names = TRUE,
            row.names = TRUE, 
            append = TRUE, 
            showNA = TRUE, 
            password = NULL)
```

### E.4.4. Fiabilidad

Cronbach's alpha (alpha), composite reliability (rhoC), average variance extracted (AVE),

```{r}
summary_estimacion_model$reliability 

plot(summary_estimacion_model$reliability, title =  "Fig. 3: Fiabilidad")

```

Alpha

```{r}
plot(summary_estimacion_model$reliability[,1], pch = 1, col = "red", main="Alpha ", 
     xlab = "Variables", ylab = "Valor estimados",  ylim = c(0, 1))
text(summary_estimacion_model$reliability[,1],labels = row.names(summary_estimacion_model$reliability) , pos = 3)
abline(h=0.7,col="red",lty=2,lwd=2) 

```

AVE

```{r}
plot(summary_estimacion_model$reliability[,3], pch = 2, col = "red", main="AVE ",
     xlab = "Variables", ylab = "Valor estimados",  ylim = c(0, 1))
text(summary_estimacion_model$reliability[,1],labels = row.names(summary_estimacion_model$reliability) , pos = 3)
abline(h=0.5,col="red",lty=2,lwd=2) 
```

Exportar a Excel

```{r}

write.xlsx2(x=summary_estimacion_model$reliability, 
            'resumen.xlsx', 
            sheetName = "reliability", 
            col.names = TRUE,
            row.names = TRUE, 
            append = TRUE, 
            showNA = TRUE, 
            password = NULL)
```

### E.4.5. Cargas

```{r}
summary_estimacion_model$loadings # Cargas -> reflectivas mayor a 0.70
summary_estimacion_model$loadings^2

summary_estimacion_model$weights  # Pesos -> Formativos

```

Exportar a Excel

```{r}

write.xlsx2(x=summary_estimacion_model$loadings, 
            'resumen.xlsx', 
            sheetName = "loadings", 
            col.names = TRUE,
            row.names = TRUE, 
            append = TRUE, 
            showNA = TRUE, 
            password = NULL)

write.xlsx2(x=summary_estimacion_model$weights, 
            'resumen.xlsx', 
            sheetName = "weights", 
            col.names = TRUE,
            row.names = TRUE, 
            append = TRUE, 
            showNA = TRUE, 
            password = NULL)
```

### E.4.6. Cargas Cruzadas

```{r}
summary_estimacion_model$validity$cross_loadings

write.xlsx2(x=summary_estimacion_model$validity$cross_loadings, 
            'resumen.xlsx', 
            sheetName = "cross_loadings", 
            col.names = TRUE,
            row.names = TRUE, 
            append = TRUE, 
            showNA = TRUE, 
            password = NULL)

```

### E.4.7. VIF

```{r}
summary_estimacion_model$vif_antecedents

summary_estimacion_model$validity$vif_items 

```

### E.4.8. Fornell-Larcker

```{r}
summary_estimacion_model$validity$fl_criteria

write.xlsx2(x=summary_estimacion_model$validity$fl_criteria, 
            'resumen.xlsx', 
            sheetName = "Fornell-Larcker", 
            col.names = TRUE,
            row.names = TRUE, 
            append = TRUE, 
            showNA = TRUE, 
            password = NULL)
```

### E.4.9. fSquare

```{r}
summary_estimacion_model$fSquare 

write.xlsx2(x=summary_estimacion_model$fSquare, 
            'resumen.xlsx', 
            sheetName = "fSquare", 
            col.names = TRUE,
            row.names = TRUE, 
            append = TRUE, 
            showNA = TRUE, 
            password = NULL)
```

### E.4.10. HTMT

```{r}
summary_estimacion_model$validity$htmt 

write.xlsx2(x=summary_estimacion_model$validity$htmt , 
            'resumen.xlsx', 
            sheetName = "htmt", 
            col.names = TRUE,
            row.names = TRUE, 
            append = TRUE, 
            showNA = TRUE, 
            password = NULL)

```

### E.4.11. Tabla de correlaciones

```{r}
summary_estimacion_model$descriptives$correlations$constructs 

write.xlsx2(x=summary_estimacion_model$descriptives$correlations$constructs  , 
            'resumen.xlsx', 
            sheetName = "Correl_constructos", 
            col.names = TRUE,
            row.names = TRUE, 
            append = TRUE, 
            showNA = TRUE, 
            password = NULL)

```

### E.4.12. Otros

b\) Efectos totales

c\) Efectos indirectos

d\) Puntuaciones estimadas para los constructos

e\) Selección de modelo BIC, AIC

```{r}
summary_estimacion_model$total_effects              ## b)
summary_estimacion_model$total_indirect_effects     ## c)
# summary_estimacion_model$composite_scores           ## d) 
summary_estimacion_model$it_criteria                ## e)

```

## E.5. Estimación Bootstrap

```{r message = FALSE, warning=FALSE}

boot_estimacion <- bootstrap_model(seminr_model = estimacion_model , #modelo estimado E.3  estimate_pls()
                nboot = 500,  ### N° Subsamples  >5000
                cores = parallel::detectCores(),        #CPU cores -parallel processing
                seed = 123)     #Semilla inicial


sum_boot <- summary(boot_estimacion,
                    alpha=0.05   ### Intervalo de confianza, en este caso es dos colas 90%
                                    ) 

plot(boot_estimacion, title = "Fig. 4 Bootstrapped Model")

save_plot("fig4.Bootstrapped_Modelo.pdf")

```

## E.6. Reportes Bootstrapped

### E.6.1. Paths

```{r}
sum_boot$bootstrapped_paths 

write.xlsx2(x=sum_boot$bootstrapped_paths   , 
            'resumen.xlsx', 
            sheetName = "bootstrapped_Coef_Path", 
            col.names = TRUE,
            row.names = TRUE, 
            append = TRUE, 
            showNA = TRUE, 
            password = NULL)

```

### E.6.2. Cargas, pesos y efectos totales del modelo

```{r}
sum_boot$bootstrapped_loadings 
sum_boot$bootstrapped_weights #bootstrap standard error, t-statistic, and confidence intervals for the indicator weights
sum_boot$bootstrapped_total_paths  #bootstrap standard error, t-statistic, and confidence intervals total effects

write.xlsx2(x=sum_boot$bootstrapped_loadings    , 
            'resumen.xlsx', 
            sheetName = "bootstrapped_loadings", 
            col.names = TRUE,
            row.names = TRUE, 
            append = TRUE, 
            showNA = TRUE, 
            password = NULL)

write.xlsx2(x=sum_boot$bootstrapped_weights   , 
            'resumen.xlsx', 
            sheetName = "bootstrapped_weights", 
            col.names = TRUE,
            row.names = TRUE, 
            append = TRUE, 
            showNA = TRUE, 
            password = NULL)

write.xlsx2(x=sum_boot$bootstrapped_total_paths   , 
            'resumen.xlsx', 
            sheetName = "bootstrapped_total_paths", 
            col.names = TRUE,
            row.names = TRUE, 
            append = TRUE, 
            showNA = TRUE, 
            password = NULL)


```

### E.6.3. HTMT CI

```{r}
sum_boot$bootstrapped_HTMT    

summary_estimacion_model$validity$htmt  ### HTMT modelo estructural ( <0.85 )


write.xlsx2(x=sum_boot$bootstrapped_HTMT   , 
            'resumen.xlsx', 
            sheetName = "bootstrapped_HTMT", 
            col.names = TRUE,
            row.names = TRUE, 
            append = TRUE, 
            showNA = TRUE, 
            password = NULL)

```

# F. Análisis de Mediación

## F.1 Efectos Indirectos

*Efectos totales indirectos*

```{r}
summary_estimacion_model$total_indirect_effects
#Evaluación de la significancia de los efectos indirectos. p1 * p2 es significante 

specific_effect_significance(boot_estimacion,  ###Boot
                             from ='FC', #Desde
                             through = 'IU',  #Mediador ## podría ser un vector del tipo c('construct1', 'construct2')).
                             to = 'SNS', 
                             alpha = 0.05)

specific_effect_significance(boot_estimacion,  ###Boot
                             from ='HA',
                             through = 'IU',
                             to = 'SNS',
                             alpha = 0.05)

#FC ==> SNS No significativo ==> Evaluar si p3 es Directo o no efecto
#HA ==> SNS  Significativo  ==> Efecto Complementario/ Competitivo o Indirecto solo                       
sum_boot$total_indirect_effects

```

## F.2 Efecto directo

### F.2.1 Paso 1: Significancia

Evaluar la significancia y luego para ver si es mediación full o parcial se revisan los path directos.

```{r}
summary_estimacion_model$paths
sum_boot$bootstrapped_paths 

#FC ==> SNS No significativo ==> No efecto
#HA ==> SNS  Significativo  ==> Evaluar si es complementario (0<) o competitivo (0>)

```

### F.2.2 Paso 2: tipo de mediación

```{r}
## Calcula el signo de ESE CAMINO p1*p2*p3 complementario (0<) o competitivo (0>)
summary_estimacion_model$paths['HA', 'SNS'] *
  summary_estimacion_model$paths['HA', 'IU'] *
  summary_estimacion_model$paths['IU', 'SNS'] 


summary_estimacion_model$paths['FC', 'SNS'] *
  summary_estimacion_model$paths['FC', 'IU'] *
  summary_estimacion_model$paths['IU', 'SNS'] 
```

# G. Predict PLS

## G.1. Generar la predicción del modelo

```{r message = FALSE, warning=FALSE}
predict_modelo <- predict_pls(
  model = estimacion_model,   ### modelo de medida E.3
  technique = predict_DA,           
      # direct antecedent (predict_DA) consideraría tanto el antecedente como el mediador predictor del constructo
      # earliest antecedent (predict_EA) mediador se excluiría del análisis
  noFolds = 10,                     ### Folds a generar
  reps = 10)                        ### Número de repeticiones CV


sum_predict_modelo <- summary(predict_modelo)
#sum_predict_modelo
```

Comparamos los RMSE de PLS out-of-sample metrics v/s LM out-of-sample metrics. Si PLS\<LM Ok

```{r}
#sum_predict_modelo$PLS_out_of_sample
#sum_predict_modelo$LM_out_of_sample

predict_dif <- sum_predict_modelo$PLS_out_of_sample-sum_predict_modelo$LM_out_of_sample 
predict_dif 

# Si todos los items son negativos ==> Alta predicción (PLS<LM)
# Si es la mayoría ==> Baja predicción
# Si ninguno ==> No poder de predicción
```

```{r}
write.xlsx2(x=predict_dif, 
            'resumen.xlsx', 
            sheetName = "Predict_dif (PLS-LM)", 
            col.names = TRUE,
            row.names = TRUE, 
            append = TRUE, 
            showNA = TRUE, 
            password = NULL)
```

```{r}
sum_predict_modelo$prediction_error

write.xlsx2(x=sum_predict_modelo$prediction_error, 
            'resumen.xlsx', 
            sheetName = "Predict_erro", 
            col.names = TRUE,
            row.names = TRUE, 
            append = TRUE, 
            showNA = TRUE, 
            password = NULL)
```

## G.2. Analizar la distribución del error (indicador en específico)

```{r}
par(mfrow=c(1,3))
plot(sum_predict_modelo,
     indicator = 'IU1')
plot(sum_predict_modelo,
     indicator = 'IU2')
par(mfrow=c(1,1))

```

```{r}
par(mfrow=c(1,4))
plot(sum_predict_modelo,
     indicator = 'U1')
plot(sum_predict_modelo,
     indicator = 'U2')
plot(sum_predict_modelo,
     indicator = 'U3')
plot(sum_predict_modelo,
     indicator = 'U4')
par(mfrow=c(1,1))

```

# H. Análisis de Moderadores

## H.1. Modelo de medida Moderadores

```{r message = FALSE, warning=FALSE}
modelo_medida_mod <- constructs(
  composite('PE', multi_items('PE', 1:4), weights = mode_A),
  composite('EE', multi_items('EE', 1:3)),
  composite('SI', multi_items('SI', 1:4)),
  composite('FC', multi_items('FC', 1:3)),
  composite('HM', multi_items('HM', 1:3)),
  composite('HA', multi_items('HA', 1:5)),
# composite('CUSA', single_item('cusa')),  # En el caso de ser un unico item dejar como single_item
  composite('TRI_A', multi_items('TRI', 1:4)),
  composite('TRI_B', multi_items('TRI', 5:8)),
  composite('IU', multi_items('IU', 1:2)),
  composite('SNS', multi_items('U', 1:4)),
  interaction_term(iv = 'IU', moderator = 'TRI_A', method = two_stage),  #Moderador method = orthogonal o method = two_stage
  interaction_term(iv = 'FC', moderator = 'TRI_B', method = two_stage)  #Moderador method = orthogonal o method = two_stage
)

plot(modelo_medida_mod)
```

## H.2. Modelo Estructural Moderadores

```{r message = FALSE, warning=FALSE}
modelo_estruc_mod <- relationships(
  paths(from = c('PE', 'EE', 'SI', 'HM','FC', "HA"), to = c('IU')),
  paths(from = c('HA'), to = c('SNS')),
  paths(from = c('IU', 'TRI_A', 'IU*TRI_A'), to = c('SNS')),
  paths(from = c('FC', 'TRI_B', 'FC*TRI_B'), to = c('SNS'))
)

 plot(modelo_estruc_mod)
```

## H.3. Ejecución modelo

```{r message = FALSE, warning=FALSE}
pls_model_mod_med <- estimate_pls(data = pls_data2,
                                  measurement_model = modelo_medida_mod,
                                  structural_model = modelo_estruc_mod,
                                  missing = mean_replacement,
                                  missing_value = '-99'
                                  )

boot_pls_model_mod_med <- bootstrap_model(seminr_model = pls_model_mod_med,
                                          nboot = 500)   #Cambiar al menos a 5000

sum_pls_model_mod_med <- summary(pls_model_mod_med)
sum_boot_pls_model_mod <- summary(boot_pls_model_mod_med, alpha = 0.05)

plot(pls_model_mod_med, title =  "Fig. 5: Bootstrap Modelo Estimado Moderador")
save_plot("fig 5.Bootstrap Modelo Estimado Moderador.pdf")

```

## H.4. Evaluar el modelo Moderador

### H.4.1. R\^2 Exógenos

```{r}

sum_pls_model_mod_med$paths  

plot(sum_pls_model_mod_med$paths[,1], pch = 2, col = "red", main="Betas y R^2 moderador (Exógenos)", 
     xlab = "Variables", ylab = "Valores estimados", xlim = c(0,length(row.names(sum_pls_model_mod_med$paths))+1)
     ) 
text(sum_pls_model_mod_med$paths[,1],labels = row.names(sum_pls_model_mod_med$paths) , pos = 4)


```

Endógenos

```{r}
plot(sum_pls_model_mod_med$paths[,2], pch = 2, col = "red", main="Betas y R^2 moderador (Endógenos)", 
     xlab = "Variables", ylab = "Valores estimados" , xlim = c(0,length(row.names(sum_pls_model_mod_med$paths))+1) )
text(sum_pls_model_mod_med$paths[,2],labels = row.names(sum_pls_model_mod_med$paths) , pos = 4)

```

Exportar Excel

```{r}
write.xlsx2(x=sum_pls_model_mod_med$paths, 
            'resumen.xlsx', 
            sheetName = "BetasyR_Moderador", 
            col.names = TRUE,
            row.names = TRUE, 
            append = TRUE, 
            showNA = TRUE, 
            password = NULL)
```

### H.4.2. Fiabilidad Cronbach's alpha (alpha), composite reliability (rhoC), average variance extracted (AVE),

```{r}
sum_pls_model_mod_med$reliability 

plot(sum_pls_model_mod_med$reliability)

```

Exportar a Excel

```{r}

write.xlsx2(x=sum_pls_model_mod_med$reliability, 
            'resumen.xlsx', 
            sheetName = "reliability_moderador", 
            col.names = TRUE,
            row.names = TRUE, 
            append = TRUE, 
            showNA = TRUE, 
            password = NULL)
```

### H.4.3. Cargas

```{r}
sum_pls_model_mod_med$loadings # Cargas -> reflectivas mayor a 0.70
sum_pls_model_mod_med$weights  # Pesos -> Formativos

```

Exportar a Excel

```{r}

write.xlsx2(x=sum_pls_model_mod_med$loadings, 
            'resumen.xlsx', 
            sheetName = "loadings_moderador", 
            col.names = TRUE,
            row.names = TRUE, 
            append = TRUE, 
            showNA = TRUE, 
            password = NULL)

write.xlsx2(x=sum_pls_model_mod_med$weights, 
            'resumen.xlsx', 
            sheetName = "weights_moderador", 
            col.names = TRUE,
            row.names = TRUE, 
            append = TRUE, 
            showNA = TRUE, 
            password = NULL)
```

### H.4.4. Cargas Cruzadas

```{r}
sum_pls_model_mod_med$validity$cross_loadings

write.xlsx2(x=sum_pls_model_mod_med$validity$cross_loadings, 
            'resumen.xlsx', 
            sheetName = "cross_loadings_moderador", 
            col.names = TRUE,
            row.names = TRUE, 
            append = TRUE, 
            showNA = TRUE, 
            password = NULL)

```

### H.4.5. VIF

```{r}
sum_pls_model_mod_med$vif_antecedents

sum_pls_model_mod_med$validity$vif_items 

```

### H.4.6. Fornell-Larcker

```{r}
sum_pls_model_mod_med$validity$fl_criteria

write.xlsx2(x=sum_pls_model_mod_med$validity$fl_criteria, 
            'resumen.xlsx', 
            sheetName = "Fornell-Larcker_moderador", 
            col.names = TRUE,
            row.names = TRUE, 
            append = TRUE, 
            showNA = TRUE, 
            password = NULL)
```

### H.4.7. fSquare

```{r}
sum_pls_model_mod_med$fSquare 

write.xlsx2(x=sum_pls_model_mod_med$fSquare, 
            'resumen.xlsx', 
            sheetName = "fSquare_moderador", 
            col.names = TRUE,
            row.names = TRUE, 
            append = TRUE, 
            showNA = TRUE, 
            password = NULL)
```

### H.4.8. HTMT

```{r}
sum_pls_model_mod_med$validity$htmt 

write.xlsx2(x=sum_pls_model_mod_med$validity$htmt , 
            'resumen.xlsx', 
            sheetName = "htmt_moderador", 
            col.names = TRUE,
            row.names = TRUE, 
            append = TRUE, 
            showNA = TRUE, 
            password = NULL)

```

### H.4.9. Tabla de correlaciones

```{r}
sum_pls_model_mod_med$descriptives$correlations$constructs 

write.xlsx2(x=sum_pls_model_mod_med$descriptives$correlations$constructs  , 
            'resumen.xlsx', 
            sheetName = "Correl_constructos_moderador", 
            col.names = TRUE,
            row.names = TRUE, 
            append = TRUE, 
            showNA = TRUE, 
            password = NULL)

```

### H.4.10. Otros

b\) Efectos totales

c\) Efectos indirectos

d\) Puntuaciones estimadas para los constructos

e\) seleccion de modelo BIC, AIC

```{r}
sum_pls_model_mod_med$total_effects              ## b)
sum_pls_model_mod_med$total_indirect_effects     ## c)
# sum_pls_model_mod_med$composite_scores           ## d) 
sum_pls_model_mod_med$it_criteria                ## e)

```

## H.5. Evaluar Boot Moderador

```{r}
sum_boot_pls_model_mod$bootstrapped_paths

sum_boot$bootstrapped_HTMT    

summary_estimacion_model$validity$htmt
```

## H.6. Simple slope analysis plot

```{r}
slope_analysis(
moderated_model = pls_model_mod_med,
dv = 'SNS',
moderator = 'TRI_A',
iv = 'IU',
leg_place = 'bottomright')
```

```{r}
#plot_interaction(pls_model_mod_med, 'IU*TRI_A', 'SNS')
```

```{r}
slope_analysis(
moderated_model = pls_model_mod_med,
dv = 'SNS',
moderator = 'TRI_B',
iv = 'FC',
leg_place = 'bottomright')
```

```{r}
#plot_interaction(pls_model_mod_med, 'FC*TRI_B', 'SNS')
```

# I. Comparación con otros modelos

## I.1 Creamos los modelos adicionales.

*Nota:* No se modificará el modelo de medida. Comparación es a nivel de modelo estructural

```{r}
#modelo 0 #modelo evaluado creado en E.2
#modelo_estruc <- relationships(
#  paths(from = c('PE', 'EE', 'SI', 'FC', 'HM', "HA"), to = c('IU')),
#  paths(from = c('FC', 'HA', "IU"), to = c('SNS'))
# )


# Modelo 1
structural_model1 <- relationships(
  paths(from = c('PE', 'EE', 'SI', 'FC', 'HM', "HA"), to = c('IU')),
  paths(from = c('HA', "IU"), to = c('SNS'))
)

# Modelo 2
structural_model2 <- relationships(
  paths(from = c('PE', 'EE', 'SI', 'FC', 'HM', "HA"), to = c('IU')),
  paths(from = c("IU"), to = c('SNS'))
)

# Modelo 3
structural_model3 <- relationships(
  paths(from = c('PE', 'EE', 'SI',  'HM' ), to = c('IU')),
  paths(from = c( 'HA', 'FC','IU'), to = c('SNS'))
)
```

```{r}
plot(modelo_estruc) # Modelo inicial 
plot(structural_model1)
plot(structural_model2)
plot(structural_model3)
```

## I.2 Generamos los modelos

```{r message = FALSE, warning=FALSE}
pls_model1 <- estimate_pls(
  data = pls_data2,
  measurement_model = modelo_medida,
  structural_model = structural_model1,
  missing_value = '-99'
)
sum_model1 <- summary(pls_model1)

pls_model2 <- estimate_pls(
  data = pls_data2,
  measurement_model = modelo_medida,
  structural_model = structural_model2,
  missing_value = '-99'
)
sum_model2 <- summary(pls_model2)

pls_model3 <- estimate_pls(
 data = pls_data2,
  measurement_model = modelo_medida,
  structural_model = structural_model3,
  missing_value = '-99'
)
sum_model3 <- summary(pls_model3)
```

## I.3 Comparamos los modelos

```{r}
summary_estimacion_model$it_criteria
sum_model1$it_criteria
sum_model2$it_criteria
sum_model3$it_criteria
# Menor BIC  tiene mejor poder predictivo
```

```{r}
# Recogemos los valores BIC de cada modelo. 
#Nos centramos en este ya que es el que intermedia, el que está cambiando los modelos 

itcriteria_vector <- c(summary_estimacion_model$it_criteria['BIC', 'IU'], 
                       sum_model1$it_criteria['BIC', 'IU'],
                       sum_model2$it_criteria['BIC', 'IU'],
                       sum_model3$it_criteria['BIC', 'IU'])
                       
itcriteria_vector2 <- c(summary_estimacion_model$it_criteria['BIC', 'SNS'], 
                       sum_model1$it_criteria['BIC', 'SNS'],
                       sum_model2$it_criteria['BIC', 'SNS'],
                       sum_model3$it_criteria['BIC', 'SNS'])
# Asignamos los nombres de los modelos a IT Criteria vector
names(itcriteria_vector) <- c('Original','Model1', 'Model2', 'Model3')
names(itcriteria_vector2) <- c('Original','Model1', 'Model2', 'Model3')
```

```{r}
# Valores BIC por modelos # El menor BIC seleccionamos - IU
itcriteria_vector

# Calcula BIC Akaike # Mayor implica  mejor poder predictivo - IU
compute_itcriteria_weights(itcriteria_vector)

# Valores BIC para SNS en distintos modelos - SNS
itcriteria_vector2

# Calcula BIC Akaike # Mayor implica  mejor poder predictivo -SNS
compute_itcriteria_weights(itcriteria_vector2)
```

# J. Análisis Multigrupo

Asumiremos que se desea crear multigrupo con la variable género.

*NOTA:* Solo se puede hacer multigrupo de 2 grupos. Más grupos no es posible en esta versión.

*NOTA2: Cambiaremos el modelo estructural para que MGA sea significativo*

```{r  message = FALSE, warning=FALSE}
modelo_estruc_mga <- relationships(
  paths(from = c('PE', 'SI',  "HA"), to = c('IU')),
  paths(from = c('HA', "IU"), to = c('SNS'))
)

plot(modelo_estruc_mga)

mga_esti <- estimate_pls(data = pls_data2,
                         measurement_model = modelo_medida, #E1
                         structural_model = modelo_estruc_mga, 
                         missing = mean_replacement,
                         missing_value = -99)


```

## J.1. Preparación de variable

En caso que no se haya convertido en D.2

```{r}
#pls_data2$GENDER
#pls_data2$GENERO = ifelse(pls_data2$GENDER=='Male', 1, 2)
#pls_data2$GENDER
#pls_data2$REGION3= ifelse(pls_data2$REGION=='Coquimbo', 1, 2)  #59
```

```{r}
sum(pls_data2$GENERO==1) #Male
sum(pls_data2$GENERO==2)
```

## J.2. Generamos el multigrupo

En este caso probaremos 2 MGA uno con el Género y otro con la región

```{r  message = FALSE, warning=FALSE}
pls_mga <- estimate_pls_mga(mga_esti, 
                            pls_data2$GENERO == 1, 
                            nboot=500) ## sobre 5000
```

```{r message = FALSE, warning=FALSE}
pls_mga_region <- estimate_pls_mga(mga_esti, 
          pls_data2$REGION3 == 1, 
          nboot=500) ## sobre 5000
```

## J.3. Análisis del Multigrupo

```{r}
Desde <- pls_mga$source
Hasta <- pls_mga$target
Grupo_1 <- pls_mga$group1_beta
Grupo_2 <- pls_mga$group2_beta
p_value <- pls_mga$pls_mga_p

mga_1 <- data.frame(Desde, Hasta, "B Grupo1" = Grupo_1, "B Grupo2" = Grupo_2, p_value)
mga_1
# p-values <0.05 significa que hay diferencia significativa, entre los grupos por cada Hipo.
```

```{r}
Desde <- pls_mga_region$source
Hasta <- pls_mga_region$target
Grupo_1 <- pls_mga_region$group1_beta
Grupo_2 <- pls_mga_region$group2_beta
p_value <- pls_mga_region$pls_mga_p

mga_2 <- data.frame(Desde, Hasta, "B Grupo1" = Grupo_1, "B Grupo2" = Grupo_2, p_value)
mga_2
# p-values <0.05 significa que hay diferencia significativa, entre los grupos por cada Hipo.
```

```{r}
write.xlsx2(x=pls_mga  , 
            'resumen.xlsx', 
            sheetName = "MGA", 
            col.names = TRUE,
            row.names = TRUE, 
            append = TRUE, 
            showNA = TRUE, 
            password = NULL)
```

## J.4. Análisis MICOM

NOTA: Utilizaremos paquete cSEM y sentencias en Lavaan

Modelo de medida y estructural se crean en conjunto entre ""

```{r message = FALSE, warning=FALSE}
cSmodel2 <- "
# modelo estructural 
SNS  ~ IU +  HA   
IU  ~  HA + SI + PE
# modelo de medida
PE =~ PE1 + PE2 + PE3 + PE4
SI =~ SI1 + SI2 + SI3 + SI4
HA =~ HA1 + HA2 + HA3 + HA4 + HA5
IU =~ IU1 + IU2
SNS =~ U1 + U2+ U3 + U4 
"
```

Generamos data y probamos los modelos

```{r message = FALSE, warning=FALSE}
#1 Data género
g11 <- pls_data2[(pls_data2$GENERO==1),]
g12 <- pls_data2[(pls_data2$GENERO!=1 ),]

#2 Data región
g21 <- pls_data2[(pls_data2$REGION=='Coquimbo'),]
g22 <- pls_data2[(pls_data2$REGION!='Coquimbo' ),]

csem_results1 <- csem(.data = g11, cSmodel2)
csem_results2 <- csem(.data = g12, cSmodel2)


## Análisis con cSEM
csem_results1 <- csem(.data = g11, cSmodel2)
csem_results2 <- csem(.data = g12, cSmodel2)

#Si en Status da "not Ok", no se puede usar para MGA
verify(csem_results1)
verify(csem_results2)

## Análisis con cSEM
csem_results1 <- csem(.data = g21, cSmodel2)
csem_results2 <- csem(.data = g22, cSmodel2)

#Si en Status da "not Ok", no se puede usar para MGA
verify(csem_results1)
verify(csem_results2)


```

Test MICOM

```{r message = FALSE, warning=FALSE}

csem_results <- csem(.data = list("group1" = g11, "group2" = g12), # Data creada por grupo
                      cSmodel2, .resample_method = "bootstrap", 
                     .R = 500) ##Subir número


testMICOM(csem_results, 
          .R = 500)  ##Subir número
```

Test de comparacion MGA

```{r}
testmgd <- testMGD(csem_results, .parameters_to_compare = NULL,
                   .alpha = 0.05,
        .approach_p_adjust = c("none", "bonferroni"),   ## Tipo de ajuste a los p
        .R_permutation         = 60,
        .R_bootstrap = 60,  #Subir número
        .saturated             = FALSE,
        .approach_mgd = "all", #test a aplicar 
        .output_type           = "complete", #"c("complete", "structured"),
        .eval_plan             = c("sequential", "multicore", "multisession"), 
        .verbose = FALSE)
###Test no rechazarán sus respectivas H0, los grupos son prácticamente idénticos.

testmgd
```

# K. Análisis Segundo Orden

Data contiene TRI el cual está conformado por 4 constructos, asumiremos que corresponde a un constructo de segundo orden el que afecta a IU

## K.1. Evaluar constructos de orden inferior

### K.1.1. Modelo de medida

*K.1.1.a Modelo de medida Formativo*

```{r  message = FALSE, warning=FALSE}
m_medida_1 <- constructs(
  composite('TRI_A', multi_items('TRI', 1:4), weights = mode_B ), #Formativo de ejemplo
  composite('TRI_B', multi_items('TRI', 5:8), weights = mode_B ),
  composite('TRI_C', multi_items('TRI', 9:12), weights = mode_B ),
  composite('TRI_D', multi_items('TRI', 13:16), weights = mode_B ),
  composite('IU', multi_items('IU', 1:2)),
  composite('SNS', multi_items('U', 1:4)) 
 )
plot(m_medida_1)

```

*K.1.2. Modelo de medida Reflectivo*

```{r  message = FALSE, warning=FALSE}
m_medida_2 <- constructs(
  composite('TRI_A', multi_items('TRI', 1:4)), 
  composite('TRI_B', multi_items('TRI', 5:8)),
  composite('TRI_C', multi_items('TRI', 9:12)),
  composite('TRI_D', multi_items('TRI', 13:16) ),
  composite('IU', multi_items('IU', 1:2)),
  composite('SNS', multi_items('U', 1:4)) 
 )

plot(m_medida_2)

```

### K.1.2. Modelo estructural

```{r  message = FALSE, warning=FALSE}
m_estruc_1  <- relationships(
  paths(from = c('TRI_A', 'TRI_B', 'TRI_C', 'TRI_D'), to = c('IU')),
  paths(from = c("IU"), to = c('SNS'))
)
plot(m_estruc_1)
```

### K.1.3. Estimación modelo

```{r  message = FALSE, warning=FALSE}
estimacion_model_1 <- estimate_pls(data = pls_data2,
                                      measurement_model = m_medida_1,  #K.1.1. - modelo de medida
                                      structural_model = m_estruc_1,   #K.1.2.  - modelo estructural
                                      inner_weights = path_weighting,  
                                      # path_weighting para path weighting (default) o path_factorial para factor weighting,
                                      missing = mean_replacement, 
                                      missing_value = '-99' )

summary_m_1 = summary(estimacion_model_1)

plot(estimacion_model_1)
```

### K.1.4. Evaluación del modelo de orden inferior

```{r}

plot(summary_m_1$reliability, title =  "Fig. : Fiabilidad orden inferior")

plot(summary_m_1$paths[,1], pch = 2, col = "red", main="Betas y R^2 (Exógenos)", 
     xlab = "Variables", ylab = "Valores estimados", xlim = c(0,length(row.names(summary_m_1$paths))+1)
     ) 
text(summary_m_1$paths[,1],labels = row.names(summary_m_1$paths) , pos = 4)

plot(summary_m_1$paths[,2], pch = 2, col = "red", main="Betas y R^2 (Endógenos)", 
     xlab = "Variables", ylab = "Valores estimados" , xlim = c(0,length(row.names(summary_m_1$paths))+1) )
text(summary_m_1$paths[,2],labels = row.names(summary_m_1$paths) , pos = 4)

summary_m_1$reliability
summary_m_1$loading
summary_m_1$validity$fl_criteria 
summary_m_1$validity$htmt 
summary_m_1$validity$vif_items 

```

## K.2. Constructo de orden superior

### K.2.1. Modelo de medida

*a. Modelo de medida Formativo*

```{r message = FALSE, warning=FALSE}
m_medida_3 <- constructs(
  composite('TRI_A', multi_items('TRI', 1:4), weights = mode_B),
  composite('TRI_B', multi_items('TRI', 5:8), weights = mode_B),
  composite('TRI_C', multi_items('TRI', 9:12), weights = mode_B),
  composite('TRI_D', multi_items('TRI', 13:16), weights = mode_B),
  higher_composite('TRI', c('TRI_A', 'TRI_B', 'TRI_C', 'TRI_D'), method ='two stage', weights = mode_B),
  composite('IU', multi_items('IU', 1:2)),
  composite('SNS', multi_items('U', 1:4)) 
 )
plot(m_medida_3) 
```

*b. Modelo de medida Reflectivo*

```{r message = FALSE, warning=FALSE}
m_medida_4 <- constructs(
  composite('TRI_A', multi_items('TRI', 1:4)),
  composite('TRI_B', multi_items('TRI', 5:8)),
  composite('TRI_C', multi_items('TRI', 9:12)),
  composite('TRI_D', multi_items('TRI', 13:16)),
  higher_composite('TRI', c('TRI_A', 'TRI_B', 'TRI_C', 'TRI_D'), method ='two stage', weights = mode_B),
  composite('IU', multi_items('IU', 1:2)),
  composite('SNS', multi_items('U', 1:4)) 
 )
plot(m_medida_4) 
```

### K.2.2. Modelo estructural

```{r message = FALSE, warning=FALSE}
m_estruc_2 <- relationships(
  paths(from = 'TRI', to = 'IU'), 
  paths(from = c("IU"), to = c('SNS'))) 
plot(m_estruc_2)
```

### K.2.3. Estimación modelo

*a. Estimación modelo Formativo*

```{r  message = FALSE, warning=FALSE}
estimacion_model_2 <- estimate_pls(data = pls_data2,
                                      measurement_model = m_medida_3,  #K.2.1. a
                                      structural_model = m_estruc_2,   # K.2.2.
                                      inner_weights = path_weighting,  
                                      # path_weighting para path weighting (default) o path_factorial para factor weighting,
                                      missing = mean_replacement, #Reemplazar los valores perdidos por defecto mean
                                      missing_value = '-99' ) #indicador de valores perdidos

plot(estimacion_model_2)

summary_m_2 = summary(estimacion_model_2)
```

*b. Estimación modelo Reflectivo*

```{r  message = FALSE, warning=FALSE}
estimacion_model_3 <- estimate_pls(data = pls_data2,
                                      measurement_model = m_medida_4,  #K.2.1. b
                                      structural_model = m_estruc_2,   # K.2.2.
                                      inner_weights = path_weighting,  
                                      # path_weighting para path weighting (default) o path_factorial para factor weighting,
                                      missing = mean_replacement, #Reemplazar los valores perdidos por defecto mean
                                      missing_value = '-99' ) #indicador de valores perdidos

plot(estimacion_model_3)

summary_m_3 = summary(estimacion_model_3)
```

### K.2.4. Evaluación modelo de 2do orden Formativo

```{r}
plot(summary_m_2$reliability, title =  "Fig. : Fiabilidad orden inferior")

plot(summary_m_2$paths[,1], pch = 2, col = "red", main="Betas y R^2 (Exógenos)", 
     xlab = "Variables", ylab = "Valores estimados", xlim = c(0,length(row.names(summary_m_2$paths))+1)
     ) 
text(summary_m_2$paths[,1],labels = row.names(summary_m_2$paths) , pos = 4)

plot(summary_m_2$paths[,2], pch = 2, col = "red", main="Betas y R^2 (Endógenos)", 
     xlab = "Variables", ylab = "Valores estimados" , xlim = c(0,length(row.names(summary_m_2$paths))+1) )
text(summary_m_2$paths[,2],labels = row.names(summary_m_2$paths) , pos = 4)

summary_m_2$reliability
summary_m_2$loading
summary_m_2$validity$fl_criteria 
summary_m_2$validity$htmt 
summary_m_2$validity$vif_items 
summary_m_2$validity$cross_loadings
```

### K.2.5. Bootstrap modelo de 2do orden Formativo

```{r  message = FALSE, warning=FALSE}
boot_m_2 <- bootstrap_model(seminr_model = estimacion_model_2 , #K.2.3. a
                nboot = 500,  ### N° Subsamples  5000<
                cores = parallel::detectCores(),                      #CPU cores -parallel processing
                seed = 123)    #Fijar la semilla 
plot(boot_m_2)
sum_boot_m_2 <- summary(boot_m_2, alpha=0.05 )  ### Intervalo de confianza, en este caso es dos colas 90%
```

### K.2.6. Evaluación Bootstrap modelo de 2do orden Formativo

```{r}
sum_boot_m_2$bootstrapped_weights    
summary_m_2$validity$vif_items 
sum_boot_m_2$bootstrapped_loadings 
```

*Significancia modelo segundo orden*

```{r}
specific_effect_significance(boot_seminr_model = boot_m_2,
                              from = 'TRI',
                              through = 'IU',
                              to = 'SNS',
                              alpha = 0.05)
sum_boot_m_2$bootstrapped_paths  
```

### K.2.7. Evaluación modelo de 2do orden Reflectivo

```{r}
plot(summary_m_3$reliability, title =  "Fig. : Fiabilidad orden inferior")

plot(summary_m_3$paths[,1], pch = 2, col = "red", main="Betas y R^2 (Exógenos)", 
     xlab = "Variables", ylab = "Valores estimados", xlim = c(0,length(row.names(summary_m_3$paths))+1)
     ) 
text(summary_m_3$paths[,1],labels = row.names(summary_m_3$paths) , pos = 4)

plot(summary_m_3$paths[,2], pch = 2, col = "red", main="Betas y R^2 (Endógenos)", 
     xlab = "Variables", ylab = "Valores estimados" , xlim = c(0,length(row.names(summary_m_3$paths))+1) )
text(summary_m_3$paths[,2],labels = row.names(summary_m_3$paths) , pos = 4)

summary_m_3$reliability
summary_m_3$loading
summary_m_3$validity$fl_criteria 
summary_m_3$validity$htmt 
summary_m_3$validity$vif_items 
summary_m_3$validity$cross_loadings
```

### K.2.8. Bootstrap modelo de 2do orden Reflectivo

```{r  message = FALSE, warning=FALSE}
boot_m_3 <- bootstrap_model(seminr_model = estimacion_model_3 , #K.2.3. b
                nboot = 500,  ### N° Subsamples  5000<
                cores = parallel::detectCores(),                      #CPU cores -parallel processing
                seed = 123)    
plot(boot_m_3)
sum_boot_m_3 <- summary(boot_m_3, alpha=0.05 )  ### Intervalo de confianza, en este caso es dos colas 90%
```

### K.2.9. Evaluación Bootstrap modelo de 2do orden Reflectivo

```{r}
sum_boot_m_3$bootstrapped_weights    
summary_m_3$validity$vif_items 
sum_boot_m_3$bootstrapped_loadings 
```

*Significancia modelo segundo orden*

```{r}
specific_effect_significance(boot_seminr_model = boot_m_3,
                              from = 'TRI',
                              through = 'IU',
                              to = 'SNS',
                              alpha = 0.05)
sum_boot_m_3$bootstrapped_paths  
```

# L. Análisis Pathmox

## L.1. Definición modelo

Definir modelo usando laavan syntax.

```{r  message = FALSE, warning=FALSE}
cSmodel <- "
# Structural model
SNS  ~ IU + FC + HA
IU  ~ FC + HA + SI + HM + PE + EE
#modelo de medida
PE =~ PE1 + PE2 + PE3 + PE4
EE =~ EE1 + EE2 + EE3
SI =~ SI1 + SI2 + SI3 + SI4
FC =~ FC1 + FC2 + FC3
HM =~ HM1 + HM2 + HM3
HA =~ HA1 + HA2 + HA3 + HA4 + HA5
IU =~ IU1 + IU2
SNS =~ U1 + U2+ U3 + U4 
"
```

## L.2. Análisis con cSEM

```{r  message = FALSE, warning=FALSE}
est_model <- csem(.data = pls_data2, .model = cSmodel)
bootstrap<- csem(.data = pls_data2, .model = cSmodel, .resample_method = "bootstrap", .R = 1000)
#summarize(bootstrap)
#summarize(est_model)
#valides <- assess(est_model)
#infer(est_model)
#predict(est_model)
#verify(est_model)
```

## L.3. Configurando las variables

*Nota* Variables deben estar como factor y no deben contener puntos "."


```{r}
pls_data2$GENDER2= as.factor(pls_data2$GENDER)
pls_data2$EDU2= as.factor(pls_data2$EDU)
pls_data2$RETIRED2= as.factor(pls_data2$RETIRED)
pls_data2$WSTATUS2= as.factor(pls_data2$WSTATUS)
pls_data2$GENERATION2= as.factor(pls_data2$GENERATION)
pls_data2$REGION2= as.factor(pls_data2$REGION)
pls_data2$EXP2= as.factor(pls_data2$EXP)
pls_data2$SOC2= as.factor(pls_data2$SOC)
```

En este caso creamos una variable de ejemplo en la cual se asigna a un tipo de TRI de acuerdo con el mayor valor presentado. 

```{r}
#Sumarizamos por TRI
pls_data2$TRI_A = pls_data2$TRI1 + pls_data2$TRI2 + pls_data2$TRI3 + pls_data2$TRI4
pls_data2$TRI_B = pls_data2$TRI5 + pls_data2$TRI6 + pls_data2$TRI7 + pls_data2$TRI8
pls_data2$TRI_C = pls_data2$TRI9 + pls_data2$TRI10 + pls_data2$TRI11 + pls_data2$TRI12
pls_data2$TRI_D = pls_data2$TRI13 + pls_data2$TRI14 + pls_data2$TRI15 + pls_data2$TRI16

#Asignamos a un tipo
pls_data2$TRI_T <- ifelse(pls_data2$TRI_B <= pls_data2$TRI_A & pls_data2$TRI_C <= pls_data2$TRI_A
        & pls_data2$TRI_D <= pls_data2$TRI_A, 1, 
        ifelse (pls_data2$TRI_A <= pls_data2$TRI_B & pls_data2$TRI_C <= pls_data2$TRI_B
          & pls_data2$TRI_D <= pls_data2$TRI_B, 2, 
          ifelse (pls_data2$TRI_A < pls_data2$TRI_C & pls_data2$TRI_B <= pls_data2$TRI_C
            & pls_data2$TRI_D <= pls_data2$TRI_C, 3, 4)))

#cambiamos el tipo a factor
pls_data2$TRI_T2= as.factor(pls_data2$TRI_T)
```

Genero grupo de categóricas

```{r}
categoricas2 <- c( #"EXP2",
  "EDU2", "SOC2" , "WSTATUS2", "RETIRED2"   ,"GENDER2" , 
  "GENERATION2",  "REGION2", 
  "TRI_T2")
```

Conjunto de datos con categóricas

```{r}
CSIcatvar <- pls_data2[, categoricas2]
```

## L.4. Generación modelo y resultado

Ejecutar análisis Phatmox (ver Lamberti et al., 2016; 2017)

```{r  message = FALSE, warning=FALSE}
csi.pathmox = pls.pathmox(
  .model = cSmodel ,
  .data  = pls_data2,   
  .catvar= CSIcatvar,  ## Variables categóricas a ser utilizadas 
  # .scheme= 'centroid', 'factorial', 'path' defecto Tipo de esquema de ponderación interna
  .size = 0.10, #mínimo de observaciones en porcentaje
  .size_candidate = 15, #mínimo de observaciones en cantidad  por defecto es 50
  # .consistent = TRUE, #defecto es TRUE
  .alpha = 0.05,   ### umbral mínimo de importancia por defecto 0.05
  .deep = 8        ### Máxima profundidad del arbol
  ) 

```

Ranking de importancia de las variables

```{r message=FALSE, warning=FALSE}
variables <-csi.pathmox[["var_imp"]][["variable"]]
ranking <- csi.pathmox[["var_imp"]][["ranking"]]

barplot(ranking, main = "Ranking de importancia de las variables",
      xlab = "variables",
       ylab = "Valor",
       col = rainbow(10), 
       names.arg = variables
      ) 
```

Resultados

```{r}
summary(csi.pathmox)
```
