# ------------------------------------------------------------------------------------- #
# ---- Script para detectar variables categoricas seg?n varios criterios -------------- #
# ---- by O.Rojas 12.2019 - U.Chile --------------------------------------------------- #
# ------------------------------------------------------------------------------------- #
# Test

# Función para mirar los factores    
# factorInfo <- function(var){
    # if (class(var) == "factor"){
        # lab_lev <- paste("level:", as.numeric(var), "|",
                         # "label:", as.character(var))
        # lab_lev <- group_by(data.frame(etiquetas = lab_lev), etiquetas) %>% 
            # summarize(count = n())
        # return(lab_lev)
    # } else {
        # stop("La variable no es de class(factor)")
    # }
# }
# 
# names(data[['datos']])
# dmDetectFactor(data, 'Anemia')
# df <- data
# var <- 'Anemia'

dmDetectFactor <- function(df, var, cat = 10){
    # Variable
    options(warn = -1)
    varname <- var
    var <- df[[var]]
    nvar <- length(var)

    # Si viniera como --haven-- pero es independiente del resto de test
    # porque haven puede tener problemas despues de transformado a factor
    if (class(var) == "haven_labelled"){
        var <- as_factor(var)
        temp <- table(var)
        if (length(temp) > cat){
            cat(paste("Categor?as en al variable:", length(temp), "\n"))
            stop(paste(varname, "(factor): excede categor?as =", cat))
        } else {
            return(var)
        }
    }
    
    
    # Si viene como --n?mero--
    if (class(var) == "numeric"){
        # Revisar si supera ncat
        temp <- table(var)
        if (length(temp) > cat){
            cat(paste("Categor?as en la variable:", length(temp), "\n"))
            stop(paste(varname, "(numeric): excede categor?as =", cat))
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
                cat(paste("Categor?as en al variable:", length(temp), "\n"))
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
                cat(paste("Categor?as en al variable:", length(temp), "\n"))
                stop(paste(varname, "(character): excede categor?as =", cat))
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
            cat(paste("Categor?as en al variable:", length(temp), "\n"))
            stop(paste(varname, "(factor): excede categor?as =", cat))
        } else {
            return("okvar")
        }
    
            
    # Una salida por si
    } else {
        cat(paste("Variable:", varname, "\n"))
        stop("Algo raro pas? al revisar la variable")
    }
}

