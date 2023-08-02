# H. Análisis de Moderadores

## H.1. Modelo de medida Moderadores

#Creamos un modelo de medida con moderadores
#Asumimos que el constructo TRI_A y TRI_B media al moderador IU y FC respectivamente
# Incluye el concepto interaction_term()

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


## H.2. Modelo Estructural Moderadores
#El termino 'IU*TRI_A'
#La relaciones se generan con la función relationships()

modelo_estruc_mod <- relationships(
  paths(from = c('PE', 'EE', 'SI', 'HM','FC', "HA"), to = c('IU')),  #Cada constructo con sus relaciones
  paths(from = c('HA'), to = c('SNS')),
  paths(from = c('IU', 'TRI_A', 'IU*TRI_A'), to = c('SNS')),
  paths(from = c('FC', 'TRI_B', 'FC*TRI_B'), to = c('SNS'))
)

plot(modelo_estruc_mod)


## H.3. Ejecución modelo

pls_model_mod_med <- estimate_pls(data = pls_data2,
                                  measurement_model = modelo_medida_mod,
                                  structural_model = modelo_estruc_mod,
                                  missing = mean_replacement,
                                  missing_value = '-99'
)

boot_pls_model_mod_med <- bootstrap_model(seminr_model = pls_model_mod_med,
                                          nboot = 500)   #Cambiar al menos a 5000

#Generamos los resultados
sum_pls_model_mod_med <- summary(pls_model_mod_med)
sum_boot_pls_model_mod <- summary(boot_pls_model_mod_med, alpha = 0.05)

plot(pls_model_mod_med, title =  "Fig. 6: Bootstrap Modelo Estimado Moderador")
save_plot("fig 6.Bootstrap Modelo Estimado Moderador.pdf")



## H.4. Evaluar el modelo Moderador

### H.4.1. R^2 Exógenos

sum_pls_model_mod_med$paths  

#Exógenos

plot(sum_pls_model_mod_med$paths[,1], pch = 2, col = "red", main="Betas y R^2 moderador (Exógenos)", 
     xlab = "Variables", ylab = "Valores estimados", xlim = c(0,length(row.names(sum_pls_model_mod_med$paths))+1)) 
text(sum_pls_model_mod_med$paths[,1],labels = row.names(sum_pls_model_mod_med$paths) , pos = 4)


#Endógenos

plot(sum_pls_model_mod_med$paths[,2], pch = 2, col = "red", main="Betas y R^2 moderador (Endógenos)", 
     xlab = "Variables", ylab = "Valores estimados" , xlim = c(0,length(row.names(sum_pls_model_mod_med$paths))+1))
text(sum_pls_model_mod_med$paths[,2],labels = row.names(sum_pls_model_mod_med$paths) , pos = 4)



#Exportar Excel

write.xlsx2(x=sum_pls_model_mod_med$paths, 
            'resumen.xlsx', 
            sheetName = "BetasyR_Moderador", 
            col.names = TRUE,
            row.names = TRUE, 
            append = TRUE, 
            showNA = TRUE, 
            password = NULL)


### H.4.2. Fiabilidad Cronbach's alpha (alpha), composite reliability (rhoC), average variance extracted (AVE),

sum_pls_model_mod_med$reliability 

plot(sum_pls_model_mod_med$reliability)


#Exportar a Excel


write.xlsx2(x=sum_pls_model_mod_med$reliability, 
            'resumen.xlsx', 
            sheetName = "reliability_moderador", 
            col.names = TRUE,
            row.names = TRUE, 
            append = TRUE, 
            showNA = TRUE, 
            password = NULL)

### H.4.3. Cargas

sum_pls_model_mod_med$loadings # Cargas -> reflectivas mayor a 0.70
sum_pls_model_mod_med$weights  # Pesos -> Formativos


#Exportar a Excel

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


### H.4.4. Cargas Cruzadas

sum_pls_model_mod_med$validity$cross_loadings

write.xlsx2(x=sum_pls_model_mod_med$validity$cross_loadings, 
            'resumen.xlsx', 
            sheetName = "cross_loadings_moderador", 
            col.names = TRUE,
            row.names = TRUE, 
            append = TRUE, 
            showNA = TRUE, 
            password = NULL)



### H.4.5. VIF

sum_pls_model_mod_med$vif_antecedents

sum_pls_model_mod_med$validity$vif_items 


### H.4.6. Fornell-Larcker

sum_pls_model_mod_med$validity$fl_criteria

write.xlsx2(x=sum_pls_model_mod_med$validity$fl_criteria, 
            'resumen.xlsx', 
            sheetName = "Fornell-Larcker_moderador", 
            col.names = TRUE,
            row.names = TRUE, 
            append = TRUE, 
            showNA = TRUE, 
            password = NULL)


### H.4.7. fSquare


sum_pls_model_mod_med$fSquare 

write.xlsx2(x=sum_pls_model_mod_med$fSquare, 
            'resumen.xlsx', 
            sheetName = "fSquare_moderador", 
            col.names = TRUE,
            row.names = TRUE, 
            append = TRUE, 
            showNA = TRUE, 
            password = NULL)


### H.4.8.HTMT

sum_pls_model_mod_med$validity$htmt 

write.xlsx2(x=sum_pls_model_mod_med$validity$htmt , 
            'resumen.xlsx', 
            sheetName = "htmt_moderador", 
            col.names = TRUE,
            row.names = TRUE, 
            append = TRUE, 
            showNA = TRUE, 
            password = NULL)


### H.4.9. Tabla de correlaciones

sum_pls_model_mod_med$descriptives$correlations$constructs 

write.xlsx2(x=sum_pls_model_mod_med$descriptives$correlations$constructs, 
            'resumen.xlsx', 
            sheetName = "Correl_constructos_moderador", 
            col.names = TRUE,
            row.names = TRUE, 
            append = TRUE, 
            showNA = TRUE, 
            password = NULL)

### H.4.10. Otros

#b\) Efectos totales

#c\) Efectos indirectos

#d\) Puntuaciones estimadas para los constructos

#e\) Selección de modelo BIC, AIC

sum_pls_model_mod_med$total_effects              ## b)
sum_pls_model_mod_med$total_indirect_effects     ## c)
sum_pls_model_mod_med$composite_scores           ## d) 
sum_pls_model_mod_med$it_criteria                ## e)


## H.5. Evaluar Boot Moderador

sum_boot_pls_model_mod$bootstrapped_paths

sum_boot$bootstrapped_HTMT    

summary_estimacion_model$validity$htmt


## H.6. Análisis de slope plot

slope_analysis(
  moderated_model = pls_model_mod_med,
  dv = 'SNS',
  moderator = 'TRI_A',
  iv = 'IU',
  leg_place = 'bottomright')

#plot_interaction(pls_model_mod_med, 'IU*TRI_A', 'SNS')

slope_analysis(
  moderated_model = pls_model_mod_med,
  dv = 'SNS',
  moderator = 'TRI_B',
  iv = 'FC',
  leg_place = 'bottomright')

#plot_interaction(pls_model_mod_med, 'FC*TRI_B', 'SNS')

