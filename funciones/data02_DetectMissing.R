# ------------------------------------------------------------------------------------- #
# ---- Script para detectar missing --------------------------------------------------- #
# ---- by O.Rojas 12.2019 - U.Chile --------------------------------------------------- #
# ------------------------------------------------------------------------------------- #


dmDetectMissing <- function(var, df){
    # Datos
    data <- df[[var]]    
    
    # Si es numerica o lógica solo ver NA
    if (class(data) == "numeric" | class(data) == "logical"){
        naCount <- sum(is.na(data))
        naIndx <- grep(TRUE, is.na(data))
        naPct <- naCount / length(data)
        
        naList <- list(naCount = naCount, naIndx = naIndx, naPct = naPct)
        return(naList)
    
    # Si es string pudiera tener espacios en blanco no etiquetados como NA
    } else if (class(data) == "character"){
        naCount <- sum(is.na(data))
        naIndx <- grep(TRUE, is.na(data))
        naPct <- naCount / length(data)
        
        # Tirar los espacios
        spcCount <- sum(grepl("[ ]+", data))
        data <- sub("[ ]+", "", data)
        data <- ifelse(data == "", NA, data)
        naCount2 <- sum(is.na(data))
        naIndx2 <- grep(TRUE, is.na(data))
        naPct2 <- naCount2 / length(data)
        
        naList <- list(naCount = naCount, naCount2 = naCount2,
                       naIndx = naIndx, naIndx2 = naIndx2,
                       naPct = naPct, naPct2 = naPct2,
                       spcCout = spcCount)
        return(naList)
      
    # Alguna otra cosa  
    } else {
        cat("Class no permitida (detectMissing)\n")
        stop("Algo pasó")
    }
    

}
