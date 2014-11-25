library(Hmisc)
folder <- paste0(getwd(),"/data/DatosCompletos.csv")

datos <- read.csv(folder, sep=";", stringsAsFactors=FALSE)
datos <- na.omit(datos) # listwise deletion of missing
datos <- datos[datos$year<2013, ]

# Vemos las variables
keeps <- c("imagenes","textos","videos","sonidos","urls","no_media")
nodos <- datos[keeps]

# Correlations/covariances among numeric variables in 
# data frame mtcars. Use listwise deletion of missing data. 
cor(nodos, method="pearson") 
cov(nodos) 

# Correlations with significance levels
rcorr(as.matrix(nodos), type="pearson") 

# Multiple Linear Regression Example 
fit <- lm(nodos ~ imagenes + videos + sonidos + textos + urls, data=datos)
summary(fit) # show results
plot(fit)
