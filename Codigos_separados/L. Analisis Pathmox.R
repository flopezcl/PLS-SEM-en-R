# L. Análisis Pathmox
#utilizamos librería específica para este tipo de análisis
library(genpathmox)

## L.1. Definición modelo

#Definir modelo usando laavan syntax


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


## L.2. Análisis con cSEM
#Debo estimar el modelo con la librería cSem 
library(cSEM)

est_model <- csem(.data = pls_data2, .model = cSmodel)
bootstrap<- csem(.data = pls_data2, .model = cSmodel, 
                 .resample_method = "bootstrap", 
                 .R = 1000)  #cantidad de iteraciones >5000


## L.3. Configurando las variables
#Configuro las variables que irán en el pathmox 
#estas deben ser del tipo factor (ver script D. )


pls_data2$GENDER2= as.factor(pls_data2$GENDER)
pls_data2$EDU2= as.factor(pls_data2$EDU)
pls_data2$RETIRED2= as.factor(pls_data2$RETIRED)
pls_data2$WSTATUS2= as.factor(pls_data2$WSTATUS)
pls_data2$GENERATION2= as.factor(pls_data2$GENERATION)
pls_data2$REGION2= as.factor(pls_data2$REGION)
pls_data2$EXP2= as.factor(pls_data2$EXP)
pls_data2$SOC2= as.factor(pls_data2$SOC)


# Creamos una variable TRI_T que señale al tipo dentro de TRI al que pertenece el usuario. 
#TRI define 4 indices y el mayor define la categoría del usuario

# Sumamos valores por tipo de TRI
pls_data2$TRI_A = pls_data2$TRI1 + pls_data2$TRI2 + pls_data2$TRI3 + pls_data2$TRI4
pls_data2$TRI_B = pls_data2$TRI5 + pls_data2$TRI6 + pls_data2$TRI7 + pls_data2$TRI8
pls_data2$TRI_C = pls_data2$TRI9 + pls_data2$TRI10 + pls_data2$TRI11 + pls_data2$TRI12
pls_data2$TRI_D = pls_data2$TRI13 + pls_data2$TRI14 + pls_data2$TRI15 + pls_data2$TRI16

#Asignamos cada muestra a un tipo 
pls_data2$TRI_T <- ifelse(pls_data2$TRI_B <= pls_data2$TRI_A & pls_data2$TRI_C <= pls_data2$TRI_A
                          & pls_data2$TRI_D <= pls_data2$TRI_A, 1, 
                          ifelse (pls_data2$TRI_A <= pls_data2$TRI_B & pls_data2$TRI_C <= pls_data2$TRI_B
                                  & pls_data2$TRI_D <= pls_data2$TRI_B, 2, 
                                  ifelse (pls_data2$TRI_A < pls_data2$TRI_C & pls_data2$TRI_B <= pls_data2$TRI_C
                                          & pls_data2$TRI_D <= pls_data2$TRI_C, 3, 4)))
#Convertimos a factor
pls_data2$TRI_T2= as.factor(pls_data2$TRI_T)


#Genero lista con las variables a incluir en el análisis 

categoricas2 <- c( #"EXP2",   #Esta variable se ha comentado ya que incluirla implica mayor tiempo de proceso.
  "EDU2", "SOC2" , "WSTATUS2", "RETIRED2"   ,"GENDER2" , "GENERATION2",  "REGION2", "TRI_T2")


#creación de conjunto de datos con categoricas

CSIcatvar <- pls_data2[, categoricas2]


## L.4. Generación modelo y resultado

#Ejecutar análisis Phatmox (ver Lamberti et al., 2016; 2017)


csi.pathmox <- pls.pathmox(
  .model = cSmodel ,  #Modelo de medida y estructural a utilizar 
  .data  = pls_data2,   #Data para estimar Modelo de medida y estructural
  .catvar= CSIcatvar,  ## Datos con variables categóricas a ser utilizadas 
  .size = 0.10, #mínimo de observaciones en porcentaje
  .size_candidate = 15, #mínimo de observaciones en cantidad  por defecto es 50
  .alpha = 0.05,   ### umbral mínimo de importancia  defecto 0.05
  .deep = 8        ### Máxima profundidad de los arboles
) 



plot(csi.pathmox) 



## L.5. Ranking de importancia de las variables

variables <-csi.pathmox[["var_imp"]][["variable"]]
ranking <- csi.pathmox[["var_imp"]][["ranking"]]

barplot(ranking, main = "Ranking de importancia de las variables",
        xlab = "variables",
        ylab = "Valor",
        col = rainbow(10), 
        names.arg = variables
) 


## L.6. Resultados análisis pathmox

summary(csi.pathmox)



