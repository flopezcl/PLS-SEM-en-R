# D. Crear una variable categórica desde otra variable

## D.1 Convertir en categóricas

str(pls_data2) # tipo de dato 
#copiamos variables ocuparemos si se requieren antes de modificar
pls_data2$EDU3=pls_data2$EDU  
pls_data2$SOC3= pls_data2$SOC
pls_data2$EXP3= pls_data2$EXP

#Convertimos en factor estas variables (utilizaremos en Phatmox)
pls_data2$EDU2= as.factor(pls_data2$EDU)
pls_data2$SOC2= as.factor(pls_data2$SOC)
pls_data2$EXP2= as.factor(pls_data2$EXP)


## D.2 Cambiar variables categóricas

pls_data2$GENERO= ifelse(pls_data2$GENDER=='Male', 1, 2) #hombres se dejan como 1 y mujeres como 2
pls_data2$REGION3= ifelse(pls_data2$REGION=='Coquimbo', 1, 2) #Coquimbo como 1 y Bío-Bío como 2
pls_data2$WSTATUS3= ifelse(pls_data2$WSTATUS=='N', 1, 2)
pls_data2$RETIRED3= ifelse(pls_data2$RETIRED=='N', 1, 2)
pls_data2$GENERATION3= ifelse(pls_data2$GENERATION=="Silent generation ", 1,pls_data2$GENERATION)
pls_data2$GENERATION3= ifelse(pls_data2$GENERATION=="Late Baby boomer ", 3, pls_data2$GENERATION3)
pls_data2$GENERATION3= ifelse(pls_data2$GENERATION=="Early Baby boomer ", 2,pls_data2$GENERATION3)
pls_data2$GENERATION3 <- as.numeric(pls_data2$GENERATION3) #La convertimos a númerica
pls_data2$GENERO3= pls_data2$GENERO  #copiamos la variable género creada anteriormente


