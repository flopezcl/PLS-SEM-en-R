# J. Análisis Multigrupo (MGA)

#Asumiremos que se desea crear multigrupo con la variable género y Región

#*NOTA:* Solo se puede hacer multigrupo de 2 grupos. Más grupos no es posible en esta versión.

#*NOTA2: En este ejemplo Cambiaremos el modelo estructural para que MGA sea significativo* 
  

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



## J.1. Preparación de variable

#En caso que no se haya convertido en D.2

#pls_data2$GENDER
#pls_data2$GENERO = ifelse(pls_data2$GENDER=='Male', 1, 2)
#pls_data2$GENDER
#pls_data2$REGION3= ifelse(pls_data2$REGION=='Coquimbo', 1, 2)  #59

# Revisamos frecuencia
sum(pls_data2$GENERO==1) #Male
sum(pls_data2$GENERO==2)


## J.2. Generamos el multigrupo

#En este caso probaremos 2 MGA uno con el Género y otro con la región
#Función estimate_pls_mga() es usada para crear el multigrupo, indicando el grupo inicial. 

pls_mga <- estimate_pls_mga(mga_esti, 
                            pls_data2$GENERO == 1,   #criterio de selección grupo. indicamos uno de los 2 grupos
                            nboot=500) ## sobre 2000

pls_mga_region <- estimate_pls_mga(mga_esti, 
                                   pls_data2$REGION3 == 1, 
                                   nboot=500) ## sobre 2000

## J.3. Análisis del Multigrupo

#Obtenemos los resultados del MGA
Desde <- pls_mga$source
Hasta <- pls_mga$target
Grupo_1 <- pls_mga$group1_beta
Grupo_2 <- pls_mga$group2_beta
p_value <- pls_mga$pls_mga_p

mga_1 <- data.frame(Desde, Hasta, "B Grupo1" = Grupo_1, "B Grupo2" = Grupo_2, p_value)

#Revisamos los resultados del MGA
mga_1
# p-values <0.05 significa que hay diferencia significativa, entre los grupos por cada Hipo.


#MGA Región
Desde <- pls_mga_region$source
Hasta <- pls_mga_region$target
Grupo_1 <- pls_mga_region$group1_beta
Grupo_2 <- pls_mga_region$group2_beta
p_value <- pls_mga_region$pls_mga_p

mga_2 <- data.frame(Desde, Hasta, "B Grupo1" = Grupo_1, "B Grupo2" = Grupo_2, p_value)
mga_2
# p-values <0.05 significa que hay diferencia significativa, entre los grupos por cada Hipo.

# Exportamos a Excel
write.xlsx2(x=pls_mga  , 
            'resumen.xlsx', 
            sheetName = "MGA", 
            col.names = TRUE,
            row.names = TRUE, 
            append = TRUE, 
            showNA = TRUE, 
            password = NULL)


## J.4. Análisis MICOM
# Seminr no incluye test MICOM pero si CSEM

#NOTA: Utilizaremos paquete cSEM y sentencia en Lavaan
library(cSEM)


#Modelo de medida y estructural, va todo junto, primero estructural, luego de medida

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

# Este paquete requiere separar las DATA de los grupos. 
#Generamos data y probamos los modelos

#1 Data Genero
g11 <- pls_data2[(pls_data2$GENERO==1),]
g12 <- pls_data2[(pls_data2$GENERO!=1 ),]

#2 Data región
g21 <- pls_data2[(pls_data2$REGION=='Coquimbo'),]
g22 <- pls_data2[(pls_data2$REGION!='Coquimbo' ),]

csem_results1 <- csem(.data = g11, cSmodel2) #en .data va la data separada cSmodel2 es el modelo generado en Lavaan
csem_results2 <- csem(.data = g12, cSmodel2)
#Nota: La data no debe contener el caracter "." ni en nombre de columnas ni en datos. 

#Si en Status da "not Ok", no se puede usar para MGA
verify(csem_results1) # impica corregir el modelo
verify(csem_results2)

## Analisis con cSEM
csem_results3 <- csem(.data = g21, cSmodel2)
csem_results4 <- csem(.data = g22, cSmodel2)

#Si en Status da "not Ok", no se puede usar para MGA
verify(csem_results3)
verify(csem_results4)


#Generamos Test MICOM
# Ejecutamos modelo y luego evaluamos significancia con testMICOM()
csem_results <- csem(.data = list("group1" = g11, "group2" = g12), # Data creada por grupo
                     cSmodel2, .resample_method = "bootstrap",  #modelo y metodo
                     .R = 500) ##Subir numero minimo 5000


testMICOM(csem_results, 
          .R = 500)  ##Subir numero no menos de 5000


# con función testMGD ejecutamos una bateria de Test de comparacion de MGA

testmgd <- testMGD(csem_results,  #Modelo ejecutado
                   .parameters_to_compare = NULL, #Podría compararse con otro modelo generado
                   .alpha = 0.05,
                   .approach_p_adjust = c("none", "bonferroni"),   ## Tipo de ajuste a los p
                   .R_permutation         = 60,
                   .R_bootstrap = 60,  #Subir numero a 2000
                   .saturated             = FALSE,
                   .approach_mgd = "all", #test a aplicar "all = todos"
                   .output_type           = "complete", #"c("complete", "structured"),
                   .eval_plan             = c("sequential", "multicore", "multisession"), 
                   .verbose = FALSE)
###Test no rechazarán sus respectivas H0, los grupos son prácticamente idénticos.

#Resultado de los test de comparación
testmgd

