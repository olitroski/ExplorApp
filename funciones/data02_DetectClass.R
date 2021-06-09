# ------------------------------------------------------------------------------------- #
# ---- Script para detectar clase de una variables según estructura ------------------- #
# ---- by O.Rojas 12.2019 - U.Chile... fix en 06.2021 --------------------------------- #
# ------------------------------------------------------------------------------------- #

# varname <- 'foreign'; cat = 5
dmDetectClass <- function(datos, varname, cat = 5){

    # Antecedentes
    options(warn = -1)
    variable <- datos[[varname]]
    tabla <- table(variable)
    
    # Por si está en blanco la variable
    if (length(tabla) == 0){
        tipo <- c('SinDatos', 'NA')
        
    } else {
        # ----- Si es factor --------------------------------------------
        if (class(variable) == 'factor'){
            
            # Puede ser factor ok
            if (length(tabla) <= cat){
                tipo <- c('factor', 'ok')
                
            } else {
                # ----- Puede ser texto
                # test <- c('1', '2', 'a', 's', 'd', 'f', 'g', 't', 'h', 'j', 'NA', 'na', NA)
                # N <- length(test) - sum(is.na(test))
                # test <- as.numeric(test)
                # Nnum <- length(test) - sum(is.na(test))
                # Nnum / N

                # Si al menos el 50% es texto -porque yo digo-
                N <- length(variable) - sum(is.na(variable))
                numVar <- as.numeric(as.character(variable))
                Nnum <- length(numVar) - sum(is.na(numVar))
                
                if (Nnum / N <= 0.5){
                    tipo <- c('character', 'fromFactor')
                
                # ----- O número    
                } else {
                    tipo <- c('Numeric', 'fromFactor')
                }
            }
            
        # ----- Si es numero --------------------------------------------
        } else if (class(variable) == 'numeric'){
            # Si son pocas clases un factor
            if (length(tabla) <= cat){
                tipo <- c('factor', 'fromNumeric')
                
            # Sería numero
            } else {
                tipo <- c('numeric', 'Ok')
            }
            
        # ----- Si es texto ---------------------------------------------
        } else if (class(variable) == 'character'){
            # podría ser factor
            if (length(tabla) <= cat){
                tipo <- c('factor', 'fromCharacter')
                
            } else {
                # podría ser texto
                N <- length(variable) - sum(is.na(variable))
                numVar <- as.numeric(as.character(variable))
                Nnum <- length(numVar) - sum(is.na(numVar))
                
                if (Nnum / N <= 0.5){
                    tipo <- c('character', 'Ok')
                    
                # podría ser numero
                } else {
                    tipo <- c('Numeric', 'fromCharacter')
                }
            }
        }
    }
        
        
    return(tipo)
}
