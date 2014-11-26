library(FactoMineR)

# Los datos
folder <- paste0(getwd(),"/data/Datos.csv")
datos <- read.csv(folder, sep=";", stringsAsFactors=FALSE)
datos <- na.omit(datos) # listwise deletion of missing

# Vemos las variables
keeps <- c("images","texts","videos","sounds","urls","no_media")
nodos <- datos[keeps]

# Estimamos el número de componentes
estim_ncp(nodos, ncp.min = 0, ncp.max = NULL, scale = FALSE, method = "Smooth")
estim_ncp(nodos, ncp.min = 0, ncp.max = NULL, scale = FALSE, method = "GCV")

# Armamos el grafico
res.pca1 = PCA(nodos, scale.unit = FALSE, ncp = 2, graph = TRUE)
res.pca2 = PCA(nodos, scale.unit = TRUE, ncp = 2, graph = TRUE)

# Vemos la significancia
dimdesc(res.pca1, axes = c(1, 2))

# Resumen
summary(res.pca2)
