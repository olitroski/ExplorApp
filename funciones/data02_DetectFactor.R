# ------------------------------------------------------------------------------------- #
# ---- Script para detectar variables categoricas seg?n varios criterios -------------- #
# ---- by O.Rojas 12.2019 - U.Chile --------------------------------------------------- #
# ------------------------------------------------------------------------------------- #

dmDetectFactor <- function(datos, var, cat = 5){
    # Variable
    options(warn = -1)
    varname <- var
    var <- df[[var]]
    nvar <- length(var)


    
    
    # Si viene como --número--
    if (class(var) == "numeric"){
        # Revisar si supera ncat
        temp <- table(var)
        if (length(temp) > cat){
            cat(paste("Categorías en la variable:", length(temp), "\n"))
            stop(paste(varname, "(numeric): excede categorías =", cat))
        } else {
            var <- factor(var)
            return(var)
        }

        
    # Viene como --texto--
    } else if (class(var) == "character"){
        # Se checa sum(NA) comprobar si son numeros (saca los NA originales)
        asnum <- as.numeric(var)[!(is.na(var))]
        
        if (sum(is.na(asnum)) == 0){
            # test num de categorias
            temp <- table(var)
            if (length(temp) > cat){
                cat(paste("Categorías en al variable:", length(temp), "\n"))
                stop(paste(varname, "(char/num): excede categor?as =", cat))
            } else {
                var <- factor(var)
                return(var)
            }
            

        # Si no pasa la prueba num debe ser texto
        } else {
            # Checa que no sobrepase el ncat y pasa a factor
            temp <- table(as.numeric(var))
            if (length(temp) > cat){
                cat(paste("Categorías en al variable:", length(temp), "\n"))
                stop(paste(varname, "(character): excede categorías =", cat))
            } else {
                var <- factor(var)
                return(var)
            }
        }
    
    
    # Viene como vector --logico--
    } else if (class(var) == "logical") {
        # Lo pasa a factor
        var <- as.character(var)
        var <- factor(var)
        return(var)
    
    
    # viene como --factor--
    } else if (class(var) == "factor") {
        # Producto el charAsFactor pueden superar categorias
        temp <- table(var)
        if (length(temp) > cat){
            cat(paste("Categorías en al variable:", length(temp), "\n"))
            stop(paste(varname, "(factor): excede categorías =", cat))
        } else {
            return("okvar")
        }
    
            
    # Una salida por si
    } else {
        cat(paste("Variable:", varname, "\n"))
        stop("Algo raro pasó al revisar la variable")
    }
}

