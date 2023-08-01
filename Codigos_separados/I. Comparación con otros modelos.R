# I. Comparación con otros modelos

## I.1 Creamos los modelos adicionales.

#*Nota:* No se modificará el modelo de medida. Comparación es a nivel de modelo estructural

# Creamos 3 modelos adicionales
# modelo 0 #modelo inicial creado en E.2
# modelo_estruc <- relationships(
#  paths(from = c('PE', 'EE', 'SI', 'FC', 'HM', "HA"), to = c('IU')),
#  paths(from = c('FC', 'HA', "IU"), to = c('SNS'))
#  )


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


plot(modelo_estruc) # Modelo inicial 
plot(structural_model1)
plot(structural_model2)
plot(structural_model3)


## I.2 Estimamos los modelos

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


## I.3 Comparamos los modelos

# Menor BIC  tiene mejor poder predictivo
summary_estimacion_model$it_criteria
sum_model1$it_criteria
sum_model2$it_criteria
sum_model3$it_criteria

# Recogemos los valores BIC de cada modelo. 
#Nos centramos en este ya que es el que intermedia, el que esta cambiando los modelos 

itcriteria_vector <- c(summary_estimacion_model$it_criteria['BIC', 'IU'], 
                       sum_model1$it_criteria['BIC', 'IU'],
                       sum_model2$it_criteria['BIC', 'IU'],
                       sum_model3$it_criteria['BIC', 'IU'])

itcriteria_vector2 <- c(summary_estimacion_model$it_criteria['BIC', 'SNS'], 
                        sum_model1$it_criteria['BIC', 'SNS'],
                        sum_model2$it_criteria['BIC', 'SNS'],
                        sum_model3$it_criteria['BIC', 'SNS'])

# Asigna los nombres a las columnas
names(itcriteria_vector) <- c('Original','Model1', 'Model2', 'Model3')
names(itcriteria_vector2) <- c('Original','Model1', 'Model2', 'Model3')



# Valores BIC por modelos # El menor BIC seleccionamos - IU
itcriteria_vector

# Calcula BIC Akaike # Mayor implica  mejor poder predictivo - IU
compute_itcriteria_weights(itcriteria_vector)

# Valores BIC para SNS en distintos modelos - SNS
itcriteria_vector2

# Calcula BIC Akaike # Mayor implica  mejor poder predictivo -SNS
compute_itcriteria_weights(itcriteria_vector2)
