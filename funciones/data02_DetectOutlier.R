# ---------------------------------------------------------------------------------- #
# ----- Deteccion de outliers por sd y pct - 12.2019 - O.Rojas --------------------- #
# ---------------------------------------------------------------------------------- #
# Mucho mas simple, un plot y ya... algunas marcas en 5% para lo factores y lineas
# para los histogramas

dmDetectOutlier <- function(datos, varClases){
    
    mono <- function(var, datos){
        clase <- varClases[variables == var, clase]
        
        # Histograma
        if (clase == 'numeric'){
            gdata <- as.numeric(datos[[var]])
            mu <- mean(gdata, na.rm = TRUE)
            sd <- sd(gdata, na.rm = TRUE)
            zdata <- scale(gdata)
            
            g <- ggplot(datos, aes(x = gdata)) + theme_bw() +
                geom_histogram(bins = 15, fill = "grey", color = "black") +
                theme(panel.grid.minor.y = element_blank(), 
                      panel.grid.minor.x = element_blank(),
                      panel.grid.major.x = element_blank()) +
                labs(title = glue("Distribucion {var} +/- 2 sd"), y = "Conteo", x = var) + 
                geom_vline(xintercept = mu, color = 'red') + 
                geom_vline(xintercept = mu - 2*sd, color = 'blue') + 
                geom_vline(xintercept = mu + 2*sd, color = 'blue')
            # g
        
        # Barras
        } else if (clase == 'factor'){
            gdata <- data.table(var = as.factor(datos[[var]]))
            gdata <- gdata[, .(.N), by = var]
            n <- gdata[, sum(N)]
            gdata[, pct := round((N/n)*100, 1)]
            
            g <- ggplot(gdata, aes(var, pct)) + theme_bw() + geom_col() +
                geom_hline(yintercept = 5, color = 'red') +
                labs(title = glue("Distribucion {var} linea en 5%"), y = "Porcentaje", x = var)
            
        # Cualquier otra
        } else {
            g <- ggplot(mtcars, aes(x = wt)) + theme_bw()+ geom_blank() +
                labs(title = glue("Distribucion {var}"), y = "Variable", x = var) +
                annotate("text", label = 'No se puede graficar esta variable', x = 3, y = 20)
                
            g
        }
        
        
        
        
        
    }
    
    # Lista con graficos
    gList <- list()
    
    for (var in names(datos)){
        # print(var)
        gList[[var]] <- mono(var, datos)
    }
    
    return(gList)
    
}

