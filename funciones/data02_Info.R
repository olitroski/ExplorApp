info <- function(){
    infoList <- list()
    
    # tipos en detección de clases
    tp <- c('null', 'null', 'factor', 'Ok', 'character', 'fromFactor', 'numeric', 
            'fromFactor', 'factor', 'fromNumeric', 'numeric', 'Ok', 'factor', 
            'fromCharacter', 'character', 'Ok', 'numeric', 'fromCharacter')
    tp <- matrix(tp, ncol = 2, byrow = TRUE)
    tp <- as.data.table(tp)
    names(tp) <- c('clase', 'advice')
    tp <- tp[order(clase, advice)]
    
    infoList[['infoClass']] <- tp
    
    # pa juera
    return(infoList)
}

