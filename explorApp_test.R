# --------------------------------------------------------------------------- #
# ----- Flujo de trabajo en ExplorApp --------------------------------------- #
# ----- Para Caro, 08.06.2021 (en Fase2, Valdivia) Oliver.Rojas ------------- #
# --------------------------------------------------------------------------- #
# La idea es que este sea el flujo de trabajo, se cargan las funciones y se 
# comienza trabajar los datos. 

# Setear directorio de trabajo y cargar funciones desde GitHub
rm(list=ls())
setwd('D:/GoogleDrive/R/ExplorApp')
source('gitLoader.R')
explorAppLoader(local = TRUE)

# Leer los datos
setwd('D:/GoogleDrive/R/ExplorApp/testData')
dataList <- dataLoader("excel_input.xlsx")
datos <- as.data.table(datos[['datos']])

# Detectar clases
varClases <- dmDetectClass(datos)
varMissing <- dmDetectMissing(datos, varClases)
varTablas <- dmDetectTab(datos, varClases)
varPlots <- dmDetectOutlier(datos, varClases)


info()