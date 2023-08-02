# F. Análisis de Mediación

## F.1 Efectos Indirectos

#*Efectos totales indirectos*
  
summary_estimacion_model$total_indirect_effects

#Evaluación de la significancia de los efectos indirectos. p1 * p2 es significante 
#Evauación se realiza con specific_effect_significance()

specific_effect_significance(boot_estimacion,  ###Boot
                             from ='FC',
                             through = 'IU',  ### Mediador, podría ser un vector del tipo c('construct1', 'construct2')).
                             to = 'SNS',
                             alpha = 0.05)

specific_effect_significance(boot_estimacion,  ###Boot
                             from ='HA',
                             through = 'IU',
                             to = 'SNS',
                             alpha = 0.05)
#En este caso: 
#FC ==> SNS  No significativo ==> Evaluar si p3 es Directo o no efecto
#HA ==> SNS  Significativo  ==> Efecto Complementario/Competitivo o Indirecto solo   F.2.2                    

sum_boot$total_indirect_effects #Null implica no efecto


## F.2 Efecto directo

### F.2.1 Paso 1: Significancia

#Evaluar la significancia y luego para ver si es mediación full o parcial se revisan los path directos.

summary_estimacion_model$paths
sum_boot$bootstrapped_paths 

#FC ==> SNS  No significativo ==> No efecto
#HA ==> SNS  Significativo  ==> Evaluar si es complementario (0<) o competitivo (0>)

### F.2.2 Paso 2: tipo de mediación


## Calcula el signo de ESE CAMINO p1*p2*p3 complementario (0<) o competitivo (0>)
summary_estimacion_model$paths['HA', 'SNS'] *
  summary_estimacion_model$paths['HA', 'IU'] *
  summary_estimacion_model$paths['IU', 'SNS'] 


summary_estimacion_model$paths['FC', 'SNS'] *
  summary_estimacion_model$paths['FC', 'IU'] *
  summary_estimacion_model$paths['IU', 'SNS'] 

