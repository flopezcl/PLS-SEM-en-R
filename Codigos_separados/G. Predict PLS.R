# G. Predict PLS

## G.1. Generar la predicción del modelo
#Se realiza con funcion predic_pls()

predict_modelo <- predict_pls(
  model = estimacion_model,   ### modelo estimado (no Boot)  E.3
  technique = predict_DA,           
  # direct antecedent (predict_DA) consideraría tanto el antecedente como el mediador pedictor del constructo
  # earliest antecedent (predict_EA) mediador se excluiría del análisis
  noFolds = 10,                     ### Folds a generar
  reps = 10)                        ### Numero de repeticiones CV

#sum_predict_modelo
sum_predict_modelo <- summary(predict_modelo)


#Comparamos los RMSE de PLS out-of-sample metrics v/s LM out-of-sample metrics. Si PLS<LM Ok

#sum_predict_modelo$PLS_out_of_sample
#sum_predict_modelo$LM_out_of_sample

predict_dif <- sum_predict_modelo$PLS_out_of_sample-sum_predict_modelo$LM_out_of_sample 

predict_dif 
# Si todos los items son negativos ==> Alta predicción (PLS<LM)
# Si es la mayoría ==> Baja predicción
# Si ninguno ==> No poder de predicción


write.xlsx2(x=predict_dif, 
            'resumen.xlsx', 
            sheetName = "Predict_dif (PLS-LM)", 
            col.names = TRUE,
            row.names = TRUE, 
            append = TRUE, 
            showNA = TRUE, 
            password = NULL)

# Errores de predicción
sum_predict_modelo$prediction_error

write.xlsx2(x=sum_predict_modelo$prediction_error, 
            'resumen.xlsx', 
            sheetName = "Predict_erro", 
            col.names = TRUE,
            row.names = TRUE, 
            append = TRUE, 
            showNA = TRUE, 
            password = NULL)


## G.2. Analizar la distribución del error (indicador en específico)

#Análisis de los items IU1 e IU2
par(mfrow=c(1,2))
plot(sum_predict_modelo,
     indicator = 'IU1')
plot(sum_predict_modelo,
     indicator = 'IU2')
par(mfrow=c(1,1))

#Análisis de los items U1 al U4
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
