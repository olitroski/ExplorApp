# --------------------------------------------------------------------------- #
# ----- Flujo de trabajo en ExplorApp --------------------------------------- #
# ----- Para Caro, 08.06.2021 (en Fase2, Valdivia) Oliver.Rojas ------------- #
# --------------------------------------------------------------------------- #
setwd("D:/GoogleDrive/R/ExplorApp")
dmCompile <- function(dataList){
    datos <- as.data.table(dataList[['datos']])
    
    # Detecciones
    varClases <- dmDetectClass(datos)
    varMissing <- dmDetectMissing(datos, varClases)
    varTablas <- dmDetectTab(datos, varClases)
    varPlots <- dmDetectOutlier(datos, varClases)

    variables <- names(datos)
    v <- 'Nombre'
    clase <- varClases[variables == v, clase]
    
    # Datos de missing ---------------------------------------------------------
    miss <- varMissing[varMissing$var == v, ]
    
    nombre <- miss[['var']]
    oclass <- miss[['originalClass']]
    suggest <- miss[['oficialStatus']]
    obs <- miss[['advicedStatus']]
    omiss <- miss[['oficialMiss']]
    suggmiss <- miss[['advicedMiss']]
    
    # Markdown
    mdMiss <- c(glue('## Variable {v}'),
                'An�lisis exploratorio',
                glue('- Nombre de la variable en la base de datos: **{nombre}**'),
                glue('- Tipo de variable original: **{oclass}**'),
                glue('- Clase sugerida basado en los datos: **{suggest}**'),
                glue('    - **Observaciones: {obs}**'),
                '| Perdidos | Cantidad |',
                '| --- | --- |',
                glue('| Originales | {omiss} |'),
                glue('| En clase sugerida | {suggmiss} |'))
    
    writeLines(md, 'test.md')
    
    # Tablas -------------------------------------------------------------------
    # Funcion para dejar bonitas las tablas en txt
    df2raw <- function(df){
        for (var in names(df)){
            print(var)
            substrRight <- function(x, n){
                substr(x, nchar(x)-n+1, nchar(x))
            }
            
            # Calcular el ancho maximo
            lenName <- nchar(var)
            minVar <- min(nchar(as.character(df[[var]])))
            maxVar <- max(nchar(as.character(df[[var]])))
            
            values <- c(lenName, minVar, maxVar)
            maximo <- values[which(values == max(values, na.rm = TRUE))]
            maximo <- maximo[1]
            
            # Agregar espacios a la izquierda y recortar
            espacios <- '                       '
            df[[var]] <- paste0(espacios, df[[var]])
            
            if (sum(is.na(df[[var]])) == nrow(df)){
                df[[var]] <- "NA"
            }
            df[[var]] <- substrRight(df[[var]], maximo)
            
            # Nombre de variable
            indx <- which(names(df) == var)
            temp <- paste0(espacios, names(df)[indx])
            temp <- substrRight(temp, maximo)
            names(df)[indx] <- temp
            
            
        }
        return(df)
    }

    # ------ Frecuencias
    tablas <- varTablas[[v]]
    if (clase == 'factor'){
        freq <- tablas[['tabla']]
    } else {
        freq <- tablas[['tabla']]
        freq <- head(freq, 10)
    }
    
    # tabla al md
    freq <- df2raw(freq)
    write.table(freq, "clipboard-128", sep="  ", row.names=FALSE, quote = FALSE)
    freq <- readClipboard()
    
    # ----- Header y minimos
    headmin <- tablas[['headtail']]
    headmin$sep <- rep('   |   ', 10)
    headmin <- cbind(headmin, tablas[['minmax']])
    
    headmin <- df2raw(headmin)
    write.table(headmin, "clipboard-128", sep="  ", row.names=FALSE, quote = FALSE)
    headmin <- readClipboard()
    
    headmin[1] <- sub("sep", "   ", headmin[1])
    
    # markdown
    mdTabla <- c('### Descripción de los datos',
                 'La tabla de frecuencia muestra las 10 primeras ocurrencias cuando la variable es **numerica** o **texto**',
                 '```',
                 freq, 
                 '```',
                 'La siguiente tabla muestra:',
                 '* Derecha: Primeros (head) y Ultimos (tail) observaciones, además de su posición (indx)',
                 '* Izquierda: Primeros 10 mínimos (min.data) y su índice (min.indx) al igual que los 10 máximos',
                 '```',
                 headmin,
                 '```'
                 )
    
    
    
    writeLines(mdTabla, 'test.md')
    
    
    
}    
    