# ------------------------------------------------------------------------------------- #
# ---- Script para tabla de frecuencias ----------------------------------------------- #
# ---- by O.Rojas 06.2021 - Valdivia fix en 06.2021 ----------------------------------- #
# ------------------------------------------------------------------------------------- #
# Si es factor su tabla y ya, si es char: su tabla hasta 10, si es num tabla hasta 10
# el head y un tail tabmién. Va todo a una Lista

dmDetectTab <- function(datos, varClases){
    # var <- 'Edad'
    # clase <- varClases[variables == var, clase]
    
    # Data segun clase ----
    dataVar <- function(datos, var, clase){
        df <- as.data.frame(datos)
        df <- df[var]

        if (clase == 'numeric'){
            df[[var]] <- as.numeric(df[[var]])
        } else if (clase == 'character'){
            df[[var]] <- as.character(df[[var]])
        } else if (clase == 'factor'){
            df[[var]] <- as.factor(df[[var]])
        } else {
            df[[var]] <- NA
        }

        return(as.data.table(df))
    }

    
    # Tabla de frecuencias -----
    # var <- 'Edad'; df <- data(datos, var, clase)
    frec <- function(var, df){
        tab <- datos[, .(conteo = .N), by = var]
        n <- tab[, sum(conteo)]
        tab[, porcentaje := round((conteo/n)*100, 1)]
        tab[, porcentaje := paste0(porcentaje, ' %')]
        
        return(as.data.frame(tab))
    }
    

    # Head y tail -----
    head.tail <- function(var, df){
        df[, indx := row.names(df)]
        h <- head(df, 10)
        t <- tail(df, 10)
            
        temp <- cbind(h, t)
        names(temp) <- c('head.data', 'head.indx', 'tail.data', 'tail.indx')
            
        return(temp)
    }
    
    
    # 10 minimo y maximo -----
    minmax <- function(var, df){
        df[, indx := row.names(df)]
        
        min <- df[order(get(var))]
        min <- min[1:10, ]
        
        max <- df[order(-get(var))]
        max <- max[1:10]
        
        temp <- cbind(min, max)
        names(temp) <- c('min.data', 'min.indx', 'max.data', 'max.indx')
        return(temp)
    }

    
    
    # Cuartiles ----
    cuartiles <- function(var, df, clase){
        if (clase == 'character' | clase == 'factor'){
            return(data.frame(cuartil = 'Variable', percentil = 'No numérica', valor = 'NO_CALC'))
            
        } else {
            q <- quantile(df[[var]], c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
            q <- data.frame(q)
            q[['percentil']] <- row.names(q)
            q[['cuartil']] <- c('Minimo', 'Q1', 'Q2 Mediana', 'Q3', 'Maximo')
            q <- data.table(q)
            q <- q[, .(cuartil, percentil, valor = q)]
            return(q)
        }
    }
    
    # Iterar la base
    variables <- names(datos)
    result <- list()
    for (var in variables){
        # print(var)
        clase <- varClases[variables == var, clase]
        df <- dataVar(datos, var, clase)
        
        result[[var]] <- list(tabla = frec(var, df),
                              headtail = head.tail(var, df),
                              minmax = minmax(var, df),
                              cuartil = cuartiles(var, df, clase))
    }
    
    return(result)
}
