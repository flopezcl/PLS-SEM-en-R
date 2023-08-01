# E. Modelo de ecuaciones estructuales (semir)

## E.1 Crear el modelo de medida

#Por defecto se crean como reflectivo, para crear formativo agregar "weights = mode_B"


### E.1.1 PLS Normal

# Utilizamos función constructs() e indicamos cada constructos y sus items.
# en el primer caso corresponde al constructo PE que lo conforman 4 items PE1, PE2, PE3 y PE4

library(seminr) #Utilizamos libreria semir 

# 
#modelo_medida <- constructs(
#  composite('PE', multi_items('PE', 1:4), weights = mode_A),
#  composite('EE', multi_items('EE', 1:3)),
#  composite('SI', multi_items('SI', 1:4)),
#  composite('FC', multi_items('FC', 1:3)),
#  composite('HM', multi_items('HM', 1:3)),
#  composite('HA', multi_items('HA', 1:5)),
#  #composite('CUSA', single_item('cusa')),  # Si solo un item dejar como single_item Ej. CUSA lo forma cusa
#  composite('IU', multi_items('IU', 1:2)),
#  composite('SNS', multi_items('U', 1:4)) 
#  #composite('SNS', c('U1', 'U2', 'U3', 'U4')) #Forma alternativa de agregar item especificos
#)

## Reflectivo = mode_A  (default)
## Formativo  = mode_B  (weights = mode_B)

#Gráfico Modelo de medida
#plot(modelo_medida)

#Guardamos en pdf el plot
#save_plot("modelo_medida.pdf")


### E.1.2 PLS Consistente  
#NOta: Modelo PLS Consistente trabaja solo con reflectivos e implica una forma distinta de calcular el algoritmo. 


modelo_medida <- constructs(
  reflective('PE', multi_items('PE', 1:4)),
  reflective('EE', multi_items('EE', 1:3)),
  reflective('SI', multi_items('SI', 1:4)),
  reflective('FC', multi_items('FC', 1:3)),
  reflective('HM', multi_items('HM', 1:3)),
  reflective('HA', multi_items('HA', 1:5)),
  # composite('CUSA', single_item('cusa')), # Si solo un item dejar como single_item Ej. CUSA lo forma cusa
  reflective('IU', multi_items('IU', 1:2)),
  reflective('SNS', multi_items('U', 1:4)) 
  #composite('SNS', c('U1', 'U2', 'U3', 'U4')) #Forma alternativa de agregar item especificos
)

plot(modelo_medida)   #Ver el modelo 
save_plot("fig1.modelo_medida.pdf") #Guardar como pdf  


## E.2 Crear Modelo estructural


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


save_plot("fig2.Modelo_Estructural.pdf")


## E.3 Estimación del Modelo
# función estimate_pls() estima el modelo.

estimacion_model <- estimate_pls(data = pls_data2,   #Conjunto de datos a trabajar
                                 measurement_model = modelo_medida,  # Modelo de medida
                                 structural_model = modelo_estruc,   # Modelo estructural
                                 inner_weights = path_weighting,     #calculo de los path
                                 # path_weighting para path weighting (default) o path_factorial para factor weighting,
                                 missing = mean_replacement, #Reemplazar los valores perdido mean es default
                                 missing_value = '-99' ) # Valores perdidos
#obtención de resultados
summary_estimacion_model <- summary(estimacion_model)                                      


plot(estimacion_model, title =  "Fig. 3: Modelo Estimado")
save_plot("fig3.Modelo_Estimado.pdf")


## E.4 Reportes modelo

### E.4.1. Valores perdidos y estadisticas de cada variable

summary_estimacion_model$descriptives$statistics  ## Valores perdidos y estadisticas variables 

#Exportar datos a Excel
x <- summary_estimacion_model$descriptives$statistics

#Exportar datos de los ítems
write.xlsx2(x=x["items"], 
            'resumen.xlsx',                   #Nombre archivo excel a generar
            sheetName = "resumen_hor",        #Nombre de hoja 
            col.names = TRUE,                 # Incluir nombre de columnas
            row.names = TRUE,                 #incluir nombre de filas si existe
            append = TRUE,                    # agregar al archivo ya existente
            showNA = TRUE,                    # Exportar los NA 
            password = NULL)                  #clave al excel

#Exportar datos de los constructos
write.xlsx2(x=x["constructs"], 
            'resumen.xlsx', 
            sheetName = "resumen_hor_const", 
            col.names = TRUE,
            row.names = TRUE, 
            append = TRUE, 
            showNA = TRUE, 
            password = NULL)



### E.4.2. Número de iteraciones

#Obtenemos número de iteraciones del algoritmo

#**Nota :* Si es mayor a 300 significa que no converge

summary_estimacion_model$iterations  

### E.4.3. R^2
#Valores R^2 y Betas

summary_estimacion_model$paths  

# Gráficos Exogenos
  

plot(summary_estimacion_model$paths[,1], pch = 2, col = "red", main="Betas y R^2 (Exogenos)", 
     xlab = "Variables", ylab = "Valores estimados", xlim = c(0,length(row.names(summary_estimacion_model$paths))+1)) 
text(summary_estimacion_model$paths[,1],labels = row.names(summary_estimacion_model$paths) , pos = 4)


# Gráficos Endogenos
  
plot(summary_estimacion_model$paths[,2], pch = 2, col = "red", main="Betas y R^2 (Endogenos)", 
     xlab = "Variables", ylab = "Valores estimados" , xlim = c(0,length(row.names(summary_estimacion_model$paths))+1))
text(summary_estimacion_model$paths[,2],labels = row.names(summary_estimacion_model$paths) , pos = 4)

# Exportar Excel

write.xlsx2(x=summary_estimacion_model$paths, 
            'resumen.xlsx', 
            sheetName = "BetasyR", 
            col.names = TRUE,
            row.names = TRUE, 
            append = TRUE, 
            showNA = TRUE, 
            password = NULL)


### E.4.4. Fiabilidad

# Cronbach's alpha (alpha), composite reliability (rhoC), average variance extracted (AVE),

summary_estimacion_model$reliability 


#Gráfico Fiabilidad
plot(summary_estimacion_model$reliability, title =  "Fig. 3: Fiabilidad")


#Gráfico Alpha


plot(summary_estimacion_model$reliability[,1], pch = 1, col = "red", main="Alpha ", 
     xlab = "Variables", ylab = "Valor estimados",  ylim = c(0, 1))
text(summary_estimacion_model$reliability[,1],labels = row.names(summary_estimacion_model$reliability) , pos = 3)
abline(h=0.7,col="red",lty=2,lwd=2) 


#Gráfico AVE


plot(summary_estimacion_model$reliability[,3], pch = 2, col = "red", main="AVE ",
     xlab = "Variables", ylab = "Valor estimados",  ylim = c(0, 1))
text(summary_estimacion_model$reliability[,1],labels = row.names(summary_estimacion_model$reliability) , pos = 3)
abline(h=0.5,col="red",lty=2,lwd=2) 


#Exportar a Excel
write.xlsx2(x=summary_estimacion_model$reliability, 
            'resumen.xlsx', 
            sheetName = "reliability", 
            col.names = TRUE,
            row.names = TRUE, 
            append = TRUE, 
            showNA = TRUE, 
            password = NULL)


### E.4.5. Cargas

summary_estimacion_model$loadings # Cargas -> reflectivas mayor a 0.70
summary_estimacion_model$loadings^2  # Cargas ^2 -> reflectivas mayor a 0.70
summary_estimacion_model$weights  # Pesos -> Formativos


#Exportar a Excel

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


### E.4.6. Cargas Cruzadas


summary_estimacion_model$validity$cross_loadings

#Exportar a Excel
write.xlsx2(x=summary_estimacion_model$validity$cross_loadings, 
            'resumen.xlsx', 
            sheetName = "cross_loadings", 
            col.names = TRUE,
            row.names = TRUE, 
            append = TRUE, 
            showNA = TRUE, 
            password = NULL)


### E.4.7. VIF

summary_estimacion_model$vif_antecedents

summary_estimacion_model$validity$vif_items 


### E.4.8. Fornell-Larcker


summary_estimacion_model$validity$fl_criteria

#Llevar a excel el FL
write.xlsx2(x=summary_estimacion_model$validity$fl_criteria, 
            'resumen.xlsx', 
            sheetName = "Fornell-Larcker", 
            col.names = TRUE,
            row.names = TRUE, 
            append = TRUE, 
            showNA = TRUE, 
            password = NULL)

### E.4.9. fSquare


summary_estimacion_model$fSquare 

#Exportar excel
write.xlsx2(x=summary_estimacion_model$fSquare, 
            'resumen.xlsx', 
            sheetName = "fSquare", 
            col.names = TRUE,
            row.names = TRUE, 
            append = TRUE, 
            showNA = TRUE, 
            password = NULL)


### E.4.10. HTMT

summary_estimacion_model$validity$htmt 

write.xlsx2(x=summary_estimacion_model$validity$htmt , 
            'resumen.xlsx', 
            sheetName = "htmt", 
            col.names = TRUE,
            row.names = TRUE, 
            append = TRUE, 
            showNA = TRUE, 
            password = NULL)


### E.4.11. Tabla de correlaciones

summary_estimacion_model$descriptives$correlations$constructs 

write.xlsx2(x=summary_estimacion_model$descriptives$correlations$constructs  , 
            'resumen.xlsx', 
            sheetName = "Correl_constructos", 
            col.names = TRUE,
            row.names = TRUE, 
            append = TRUE, 
            showNA = TRUE, 
            password = NULL)


### E.4.12. Otros

# b\) Efectos totales

# c\) Efectos indirectos

# d\) Puntuaciones estimadas para los constructos

# e\) seleccion de modelo BIC, AIC


summary_estimacion_model$total_effects              ## b)
summary_estimacion_model$total_indirect_effects     ## c)
summary_estimacion_model$composite_scores           ## d) 
summary_estimacion_model$it_criteria                ## e)


## E.5. Estimación Bootstrap

# La estimación Bootstrap se hace con bootstrap_model()
boot_estimacion <- bootstrap_model(seminr_model = estimacion_model , #modelo estimado E.3  estimate_pls()
                nboot = 500,                            ### N° Subsamples  >5000
                cores = parallel::detectCores(),        #CPU cores -parallel processing - no obligatorio
                seed = 123)                             #Semilla inicial - no obligatorio

#Obtención de significancia del Bootstrap
sum_boot <- summary(boot_estimacion,
                    alpha=0.05   ### Intervalo de confianza, en este caso es dos colas 90%
                                    ) 

#Gráfico del Bootstrap
plot(boot_estimacion, title = "Fig. 4 Bootstrapped Model")

#Exportar a PDF
save_plot("fig4.Bootstrapped_Modelo.pdf")

## E.6. Reportes Bootstrapped

### E.6.1. Paths

sum_boot$bootstrapped_paths 

write.xlsx2(x=sum_boot$bootstrapped_paths   , 
            'resumen.xlsx', 
            sheetName = "bootstrapped_Coef_Path", 
            col.names = TRUE,
            row.names = TRUE, 
            append = TRUE, 
            showNA = TRUE, 
            password = NULL)


### E.6.2. Cargas, pesos y efectos totales del modelo

sum_boot$bootstrapped_loadings  #Cargas
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


### E.6.3. HTMT CI

sum_boot$bootstrapped_HTMT    #Intervalos de confianza y estimadores HTMT

summary_estimacion_model$validity$htmt  ### HTMT modelo estructural ( <0.85 )


write.xlsx2(x=sum_boot$bootstrapped_HTMT   , 
            'resumen.xlsx', 
            sheetName = "bootstrapped_HTMT", 
            col.names = TRUE,
            row.names = TRUE, 
            append = TRUE, 
            showNA = TRUE, 
            password = NULL)

