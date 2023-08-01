# K. Análisis de constructos de Segundo Orden

#Data contiene la teoria TRI la cual está conformado por 4 constructos, asumiremos que corresponde a un constructo de segundo orden 
#el que afecta a IU

## K.1. Evaluar constructos de orden inferior

### K.1.1. Modelo de medida

#*K.1.1.a Modelo de medida Formativo*
  
m_medida_1 <- constructs(
  composite('TRI_A', multi_items('TRI', 1:4), weights = mode_B ), #Formativo de ejemplo
  composite('TRI_B', multi_items('TRI', 5:8), weights = mode_B ),
  composite('TRI_C', multi_items('TRI', 9:12), weights = mode_B ),
  composite('TRI_D', multi_items('TRI', 13:16), weights = mode_B ),
  composite('IU', multi_items('IU', 1:2)),
  composite('SNS', multi_items('U', 1:4)) 
)
plot(m_medida_1)


#*K.1.2. Modelo de medida Reflectivo*

m_medida_2 <- constructs(
  composite('TRI_A', multi_items('TRI', 1:4)), 
  composite('TRI_B', multi_items('TRI', 5:8)),
  composite('TRI_C', multi_items('TRI', 9:12)),
  composite('TRI_D', multi_items('TRI', 13:16) ),
  composite('IU', multi_items('IU', 1:2)),
  composite('SNS', multi_items('U', 1:4)) 
)

plot(m_medida_2)



### K.1.2. Modelo estructural
# En este caso no es de segundo orden lo evaluamos primero como si fuera de primer orden
m_estruc_1  <- relationships(
  paths(from = c('TRI_A', 'TRI_B', 'TRI_C', 'TRI_D'), to = c('IU')),
  paths(from = c("IU"), to = c('SNS'))
)
plot(m_estruc_1)


### K.1.3. Estimación modelo de orden inferior

estimacion_model_1 <- estimate_pls(data = pls_data2,
                                   measurement_model = m_medida_1,  #K.1.1. - modelo de medida
                                   structural_model = m_estruc_1,   #K.1.2.  - modelo estructural
                                   inner_weights = path_weighting,  
                                   # path_weighting para path weighting (default) o path_factorial para factor weighting,
                                   missing = mean_replacement, 
                                   missing_value = '-99' )

summary_m_1 = summary(estimacion_model_1)

plot(estimacion_model_1)


### K.1.4. Evaluación del modelo de orden inferior

plot(summary_m_1$reliability, title =  "Fig. : Fiabilidad orden inferior")

plot(summary_m_1$paths[,1], pch = 2, col = "red", main="Betas y R^2 (Exogenos)", 
     xlab = "Variables", ylab = "Valores estimados", xlim = c(0,length(row.names(summary_m_1$paths))+1)
) 
text(summary_m_1$paths[,1],labels = row.names(summary_m_1$paths) , pos = 4)

plot(summary_m_1$paths[,2], pch = 2, col = "red", main="Betas y R^2 (Endogenos)", 
     xlab = "Variables", ylab = "Valores estimados" , xlim = c(0,length(row.names(summary_m_1$paths))+1) )
text(summary_m_1$paths[,2],labels = row.names(summary_m_1$paths) , pos = 4)

summary_m_1$reliability
summary_m_1$loading
summary_m_1$validity$fl_criteria 
summary_m_1$validity$htmt 
summary_m_1$validity$vif_items 



## K.2. Constructo de orden superior

### K.2.1. Modelo de medida 2° orden

#*a. Modelo de medida Formativo-Formativo*

m_medida_3 <- constructs(
  composite('TRI_A', multi_items('TRI', 1:4), weights = mode_B),
  composite('TRI_B', multi_items('TRI', 5:8), weights = mode_B),
  composite('TRI_C', multi_items('TRI', 9:12), weights = mode_B),
  composite('TRI_D', multi_items('TRI', 13:16), weights = mode_B),
  higher_composite('TRI', c('TRI_A', 'TRI_B', 'TRI_C', 'TRI_D'),  #Señalamos el constructo de orden superior
                   method ='two stage',                           #Metodo de analisis 
                   weights = mode_B),                             # Formativo o reflectivo mode_b Formativo  mode_A reflectivo
  composite('IU', multi_items('IU', 1:2)),
  composite('SNS', multi_items('U', 1:4)) 
)
plot(m_medida_3) 


#*b. Modelo de medida Reflectivo - Formativo*
  
m_medida_4 <- constructs(
  composite('TRI_A', multi_items('TRI', 1:4)),
  composite('TRI_B', multi_items('TRI', 5:8)),
  composite('TRI_C', multi_items('TRI', 9:12)),
  composite('TRI_D', multi_items('TRI', 13:16)),
  higher_composite('TRI', c('TRI_A', 'TRI_B', 'TRI_C', 'TRI_D'),
                   method ='two stage',
                   weights = mode_B),
  composite('IU', multi_items('IU', 1:2)),
  composite('SNS', multi_items('U', 1:4)) 
)
plot(m_medida_4) 


### K.2.2. Modelo estructural

m_estruc_2 <- relationships(
  paths(from = 'TRI', to = 'IU'), 
  paths(from = c("IU", 'TRI'), to = c('SNS'))) 
plot(m_estruc_2)


### K.2.3. Estimación modelo

#*a. Estimación modelo Formativo - Formativo*
  
estimacion_model_2 <- estimate_pls(data = pls_data2,
                                   measurement_model = m_medida_3,  #K.2.1. a
                                   structural_model = m_estruc_2,   # K.2.2.
                                   inner_weights = path_weighting,  
                                   # path_weighting para path weighting (default) o path_factorial para factor weighting,
                                   missing = mean_replacement, #Reemplazar los valores perdido mean es default
                                   missing_value = '-99' )

plot(estimacion_model_2)

summary_m_2 = summary(estimacion_model_2)


#*b. Estimación modelo Reflectivo- Formativo*
  
estimacion_model_3 <- estimate_pls(data = pls_data2,
                                   measurement_model = m_medida_4,  #K.2.1. b
                                   structural_model = m_estruc_2,   # K.2.2.
                                   inner_weights = path_weighting,  
                                   # path_weighting para path weighting (default) o path_factorial para factor weighting,
                                   missing = mean_replacement, #Reemplazar los valores perdido mean es default
                                   missing_value = '-99' )

plot(estimacion_model_3)

summary_m_3 = summary(estimacion_model_3)


### K.2.4. Evaluación modelo de 2do orden Formativo


plot(summary_m_2$reliability, title =  "Fig. : Fiabilidad orden inferior")

plot(summary_m_2$paths[,1], pch = 2, col = "red", main="Betas y R^2 (Exogenos)", 
     xlab = "Variables", ylab = "Valores estimados", xlim = c(0,length(row.names(summary_m_2$paths))+1)
) 
text(summary_m_2$paths[,1],labels = row.names(summary_m_2$paths) , pos = 4)

plot(summary_m_2$paths[,2], pch = 2, col = "red", main="Betas y R^2 (Endogenos)", 
     xlab = "Variables", ylab = "Valores estimados" , xlim = c(0,length(row.names(summary_m_2$paths))+1) )
text(summary_m_2$paths[,2],labels = row.names(summary_m_2$paths) , pos = 4)

summary_m_2$reliability
summary_m_2$loading
summary_m_2$validity$fl_criteria 
summary_m_2$validity$htmt 
summary_m_2$validity$vif_items 
summary_m_2$validity$cross_loadings


### K.2.5. Bootstrap modelo de 2do orden Formativo

boot_m_2 <- bootstrap_model(seminr_model = estimacion_model_2 , #K.2.3. a
                            nboot = 500,  ### N° Subsamples  5000<
                            cores = parallel::detectCores(),                      #CPU cores -parallel processing
                            seed = 123)    
plot(boot_m_2)
sum_boot_m_2 <- summary(boot_m_2, alpha=0.05 )  ### Intervalo de confianza, en este caso es dos colas 90%


### K.2.6. Evaluación Bootstrap modelo de 2do orden Formativo

sum_boot_m_2$bootstrapped_weights    
summary_m_2$validity$vif_items 
sum_boot_m_2$bootstrapped_loadings 


#*Significancia modelo segundo orden*
#Se realiza con función specific_effect_significance()
 
specific_effect_significance(boot_seminr_model = boot_m_2,  #Modelo boostrap estimado
                             from = 'TRI',
                             through = 'IU',
                             to = 'SNS',
                             alpha = 0.05)
sum_boot_m_2$bootstrapped_paths  


### K.2.7. Evaluación modelo de 2do orden Reflectivo


plot(summary_m_3$reliability, title =  "Fig. : Fiabilidad orden inferior")

plot(summary_m_3$paths[,1], pch = 2, col = "red", main="Betas y R^2 (Exogenos)", 
     xlab = "Variables", ylab = "Valores estimados", xlim = c(0,length(row.names(summary_m_3$paths))+1)
) 
text(summary_m_3$paths[,1],labels = row.names(summary_m_3$paths) , pos = 4)

plot(summary_m_3$paths[,2], pch = 2, col = "red", main="Betas y R^2 (Endogenos)", 
     xlab = "Variables", ylab = "Valores estimados" , xlim = c(0,length(row.names(summary_m_3$paths))+1) )
text(summary_m_3$paths[,2],labels = row.names(summary_m_3$paths) , pos = 4)

summary_m_3$reliability
summary_m_3$loading
summary_m_3$validity$fl_criteria 
summary_m_3$validity$htmt 
summary_m_3$validity$vif_items 
summary_m_3$validity$cross_loadings


### K.2.8. Bootstrap modelo de 2do orden Reflectivo

boot_m_3 <- bootstrap_model(seminr_model = estimacion_model_3 , #K.2.3. b
                            nboot = 500,  ### N° Subsamples  5000<
                            cores = parallel::detectCores(),                      #CPU cores -parallel processing
                            seed = 123)    
plot(boot_m_3)
sum_boot_m_3 <- summary(boot_m_3, alpha=0.05 )  ### Intervalo de confianza, en este caso es dos colas 90%

### K.2.9. Evaluación Bootstrap modelo de 2do orden Reflectivo


sum_boot_m_3$bootstrapped_weights    
summary_m_3$validity$vif_items 
sum_boot_m_3$bootstrapped_loadings 


 #*Significancia modelo segundo orden*
  
specific_effect_significance(boot_seminr_model = boot_m_3,
                             from = 'TRI',
                             through = 'IU',
                             to = 'SNS',
                             alpha = 0.05)
sum_boot_m_3$bootstrapped_paths  

