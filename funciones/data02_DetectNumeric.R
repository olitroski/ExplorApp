# ------------------------------------------------------------------------------------- #
# ---- Script para detectar variables continuas seg?n varios criterios ---------------- #
# ---- by O.Rojas 12.2019 - U.Chile --------------------------------------------------- #
# ---- Re: en corona valdivia 04.02.2021 ---------------------------------------------- #
# ------------------------------------------------------------------------------------- #
# df <- datos
# var <- "mg"

data02_DetectNumeric <- function(df, var){
    # Capturar datos
    options(warn = -1)
    variable <- df[[var]]
    indice <- 1:length(variable)

    # Si fuera factor capturar los variable
    if (class(variable) == "factor"){
        variable <- as.character(variable)
    }
    
    # Evaluar cantidad de categorias
    ncat <- length(table(variable))
    
    # Capturar los NA
    indxNA <- indice[is.na(variable)]
    
    # Capturar numero
    indxNum <- as.numeric(variable)
    indxNum <- !(is.na(indxNum))
    indxNum <- indice[indxNum]
    
    # notNumber o notNA
    indxTemp <- indice[-c(indxNA, indxNum)]
    varTemp <- variable[indxTemp]

    # quitamos espacios y chequeamos que sean NA de texto ""
    varTemp <- gsub(" ", "", varTemp)
    varTemp <- ifelse(varTemp == "", TRUE, FALSE)
    
    # incluir los NA nuevos al vector de NA
    indxNA <- c(indxNA, indxTemp[varTemp])

    # Capturar texto 
    indxText <- indxTemp[!varTemp]
    # length(variable) == length(indxNA) + length(indxNum) + length(indxText)
    
    # determinar percent of each
    N <- length(variable)
    n <- length(variable) - length(indxNA)
    
    pctNA  <- round(length(indxNA) /N, 3)
    pctNum <- round(length(indxNum)/n, 3)
    pctTxt <- round(length(indxText)/n, 3)

    # ----- Desicions ------
    # Type
    if (pctNum > 0.8 & ncat > 5){
        type <- "numeric"

    } else if (pctNum > 0.8 & ncat <= 5){
        type <- "level"
        
    } else if (pctTxt > 0.8 & ncat > 5){
        type <- "character"
        
    } else if (pctTxt > 0.8 & ncat <= 5){
        type <- "factor"
        
    } else {
        type <- "missmatch"
    }

    # Missing
    return(list(type = type, missing = pctNA, cat = ncat, num = pctNum, char = pctTxt))
}