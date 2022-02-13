# ------------------------------------------------------------------------------------- #
# ---- Script para detectar missing --------------------------------------------------- #
# ---- by O.Rojas 12.2019 - U.Chile --------------------------------------------------- #
# ------------------------------------------------------------------------------------- #
dmDetectMissing <- function(datos, varClases){
    # Deteccion de clases primero    
    # Mejor que sea todo incremental
    
    # Función de detección  var <- 'Anemia'
    detectar <- function(var, datos){
        
        # Antecendentes
        data <- datos[[var]]
        clase <- varClases[variables == var, clase]
        advice <- varClases[variables == var, advise]
        
        # Funcion que devuelve N de miss
        nMiss <- function(data){
            return(sum(is.na(data)))
        }
        
        originalClass <- class(data)
        
        # ---- Numero -----------------------------------------------
        # Si es clase número solo no puede venir de otra cosa en forma natural
        if (clase == 'numeric'){
            oficialStatus <- clase
            oficialMiss <- nMiss(data)
            
            if (advice == 'Ok'){
                advicedStatus <- 'Clase de variable correcto'
                advicedMiss <- NA
                
            } else if (advice == 'fromFactor'){
                advicedStatus <- 'Numérico, original Factor con muchas categorías'
                advicedMiss <- as.numeric(data)
                advicedMiss <- sum(is.na(advicedMiss))
                
            } else if (advice == 'fromCharacter'){
                advicedStatus <- 'Numérico, original números guardados como texto'
                advicedMiss <- as.numeric(data)
                advicedMiss <- sum(is.na(advicedMiss))
                
            } else {
                stop('clase: Numeric')
            }
            
        # ---- Characeter ------------------------------------------- 
        } else if (clase == 'character'){
            oficialStatus <- 'Texto'
            oficialMiss <- nMiss(data)
            
            if (advice == 'Ok'){
                advicedStatus <- 'Clase de variable correcta'
                advicedMiss <- NA
                
            } else if (advice == 'fromFactor'){
                advicedStatus <- 'Texto, original Factor con muchas categorías'
                advicedMiss <- nMiss(data)
                
            } else {
                stop('clase: Character')
            }
            
        # ---- Factor -----------------------------------------------  
        } else if (clase == 'factor'){
            oficialStatus <- 'Factor'
            oficialMiss <- nMiss(data)
            
            if (advice == 'Ok'){
                advicedStatus <- 'Clase de variable correcta'
                advicedMiss <- NA
                
            } else if (advice == 'fromCharacter'){
                advicedStatus <- 'Factor, original Texto con pocas categorías'
                advicedMiss <- nMiss(data)
                
            } else if (advice == 'fromNumeric'){
                advicedStatus <- 'Factor, original Numérico con pocas categorías'
                advicedMiss <- nMiss(data)
                
            } else {
                stop('clase: Factor')
            }
            
            
        # ---- Otros ------------------------------------------------    
        } else if (clase == 'null'){
            oficialStatus <- 'NoData'
            oficialMiss <- nMiss(data)
            advicedStatus <- 'NoData, Variable vacía'
            advicedMiss <- nMiss(data)
            
            
        # ---- error ------------------------------------------------    
        } else {
            stop('Algo pasó no se detectó ninguna clase')
        }
        
        # Devuelve
        resultado <- c('var' = var,
                       'originalClass' = originalClass, 
                       'oficialStatus' = oficialStatus, 
                       'oficialMiss' = oficialMiss,
                       'advicedStatus' = advicedStatus, 
                       'advicedMiss' = advicedMiss)
        return(resultado)
    }   

    # Tirar en toda la base de datos
    variables <- names(datos)
    
    resultado <- NULL
    for (var in variables){
        # print(var)
        detection <- detectar(var, datos)
        detection <- data.frame(t(detection))
        resultado <- rbind(resultado, detection)
    }
    
    return(resultado)
}
