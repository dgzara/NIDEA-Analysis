library(psych)
library(GPArotation)
library(nFactors)
library(FactoMineR)

folder <- paste0(getwd(),"/data/Datos.csv")
datos <- read.csv(folder, sep=";", stringsAsFactors=FALSE)
datos <- na.omit(datos) # listwise deletion of missing

keeps <- c("imagenes","textos", "sonidos","urls","no.media")
mydata <- datos[keeps]
#mydata <- scale(mydata)

# Matriz de correlación
r <- cor(mydata)

# KMO Analysis
kmo <- KMO(r)
print(kmo)

# Pricipal Components Analysis
# entering raw data and extracting PCs 
# from the correlation matrix 
fit <- princomp(mydata, cor=TRUE)
summary(fit) # print variance accounted for 
loadings(fit) # pc loadings 
plot(fit,type="lines") # scree plot 
fit$scores # the principal components
biplot(fit)

# Determine Number of Factors to Extract
ev <- eigen(cor(mydata)) # get eigenvalues
ap <- parallel(subject=nrow(mydata),var=ncol(mydata),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS) 

# PCA Variable Factor Map 
result <- PCA(mydata) # graphs generated automatically 

# Varimax Rotated Principal Components
# retaining 5 components 
fit <- principal(mydata, nfactors=2, rotate="varimax")
print(fit) # print results 

# Maximum Likelihood Factor Analysis
# entering raw data and extracting 2 factors, 
# with varimax rotation 
fit <- factanal(mydata, 2, rotation="varimax")
print(fit, digits=2, cutoff=.2, sort=TRUE)
# plot factor 1 by factor 2 
load <- fit$loadings[,1:2] 
plot(load,type="n") # set up plot 
text(load,labels=names(mydata),cex=.7) # add variable names 

# Principal Axis Factor Analysis
fit <- fa(mydata,fm="pa", nfactors=2, rotate="varimax")
print(fit) # print results 