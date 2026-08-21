# En tu máquina (necesitás R + shinylive instalado)
# Actualizar rlang
# install.packages("rlang")
# install.packages("shinylive")
library(shinylive)


shinylive::export(
  appdir = "H:/Mi unidad/Analisis Mercado Ganadero/app modelos recria",
  destdir = "H:/Mi unidad/Analisis Mercado Ganadero/app modelos recria/docs"
)

# si se crea la carpeta y algo no sale bien:

# Borrar docs
unlink("H:/Mi unidad/Analisis Mercado Ganadero/app modelos recria/docs", recursive = TRUE)

# Re-exportar
shinylive::export(
  appdir = "H:/Mi unidad/Analisis Mercado Ganadero/app modelos recria",
  destdir = "H:/Mi unidad/Analisis Mercado Ganadero/app modelos recria/docs"
)

# luego ir al archivo git en terminal

### probar antes de git
#install.packages("httpuv")
httpuv::runStaticServer("H:/Mi unidad/Analisis Mercado Ganadero/app modelos recria/docs")

# voy por otra opcion con shinyapp

#install.packages("rsconnect")
# # conectar cuenta
# rsconnect::setAccountInfo(name='1ickro-patricia-acetta',
#                           token='476E5A924278836257CBAEE4D68D7012',
#                           secret='FqGTfjj5fV50Y/N6gCvIVeXiwDcRdPGPdbrke6uQ')
# 
# # deployar
# rsconnect::deployApp(
#   appDir = "H:/Mi unidad/Analisis Mercado Ganadero/app modelos recria",
#   appName = "modelos-recria",
#   account = "1ickro-patricia-acetta"
# )
# 
# 
# # ver web https://pmacetta.shinyapps.io/modelos-recria/
