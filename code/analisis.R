Datos <- read.csv("C:/R/NIDEA/data/Datos.csv", sep=";")
mydata <- na.omit(Datos) # listwise deletion of missing

# Multiple Linear Regression Example 
fit <- lm(nodos ~ imagenes + videos + sonidos + textos + urls, data=mydata)
summary(fit) # show results
plot(fit)