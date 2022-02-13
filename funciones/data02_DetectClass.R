# ------------------------------------------------------------------------------------- #
# ---- Script para detectar clase de una variables según estructura ------------------- #
# ---- by O.Rojas 12.2019 - U.Chile... fix en 06.2021 --------------------------------- #
# ------------------------------------------------------------------------------------- #

# varname <- 'foreign'; cat = 5
dmDetectClass <- function(datos, ncat = 5){
    # Funcion de deteccion para una variable
    detectar <- function(datos, varname, ncat){
        
        # Antecedentes
        options(warn = -1)
        variable <- datos[[varname]]
        tabla <- table(variable)
        
        # Por si está en blanco la variable
        if (length(tabla) == 0){
            tipo <- c('null', 'null')
            
        } else {
            # ----- Si es factor --------------------------------------------
            if (class(variable) == 'factor'){
                
                # Viene como factor y tiene pocos niveles
                if (length(tabla) <= ncat){
                    tipo <- c(varname, 'factor', 'ok')
                 
                # Si tiene mas categorias   
                } else {
                    # % de NA en el transformado a número
                    N <- length(variable) - sum(is.na(variable))
                    numVar <- as.numeric(as.character(variable))
                    Nnum <- length(numVar) - sum(is.na(numVar))
                    
                    # Si al menos el 50% es texto -porque yo digo-
                    if (Nnum / N <= 0.5){
                        tipo <- c(varname, 'character', 'fromFactor')
                        
                    # O número    
                    } else {
                        tipo <- c(varname, 'numeric', 'fromFactor')
                    }
                }
                
            # ----- Si es numero --------------------------------------------
            } else if (class(variable) == 'numeric'){
                # Si son pocas clases un factor
                if (length(tabla) <= ncat){
                    tipo <- c(varname, 'factor', 'fromNumeric')
                    
                # Sería numero
                } else {
                    tipo <- c(varname, 'numeric', 'Ok')
                }
                
            # ----- Si es texto ---------------------------------------------
            } else if (class(variable) == 'character'){
                # podría ser factor
                if (length(tabla) <= ncat){
                    tipo <- c(varname, 'factor', 'fromCharacter')
                    
                } else {
                    # podría ser texto
                    N <- length(variable) - sum(is.na(variable))
                    numVar <- as.numeric(as.character(variable))
                    Nnum <- length(numVar) - sum(is.na(numVar))
                    
                    if (Nnum / N <= 0.5){
                        tipo <- c(varname, 'character', 'Ok')
                        
                        # podría ser numero
                    } else {
                        tipo <- c(varname, 'numeric', 'fromCharacter')
                    }
                }
            }
        }
        
        return(tipo)
    }

    # Calcular el data frame de detección
    varClases <- list()
    for (var in names(datos)){
        # print(var)
        temp <- detectar(datos, var, ncat)
        temp <- data.frame(clase = temp[2], advise = temp[3])
        varClases[[var]] <- temp
    }
    varClases <- rbindlist(varClases, idcol = "variables")

    return(varClases)
}

