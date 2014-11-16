datos <- read.csv("C:/R/NIDEA/data/Datos.csv", sep=";", stringsAsFactors=FALSE)
datos <- na.omit(datos) # listwise deletion of missing

# Prepare Data
keeps <- c("imagenes","textos","videos")
mydata <- datos[keeps]
mydata <- scale(mydata)

# Determine number of clusters
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:20) wss[i] <- sum(kmeans(mydata,centers=i)$withinss)
plot(1:20, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares") 

# Number of clusters
num = 6

# K-Means Cluster Analysis
fit <- kmeans(mydata, num) # cluster solution
# get cluster means 
aggregate(mydata,by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata.fit <- data.frame(mydata, fit$cluster)

# Model Based Clustering
library(mclust)
fit <- Mclust(mydata)
plot(fit) # plot results 
summary(fit) # display the best model 

# Ward Hierarchical Clustering
d <- dist(mydata, method = "euclidean") # distance matrix
fit <- hclust(d)
plot(fit,labels=datos$id,main='Default from hclust') # display dendogram
groups <- cutree(fit, k=num) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit, k=num, border="red") 

# K-Means Clustering with 5 clusters
fit <- kmeans(mydata, num)
plot(mydata, col = fit$cluster)
points(fit$centers)
print(fit)

# vary parameters for most readable graph
library(cluster) 
clusplot(mydata, fit$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

# Centroid Plot against 1st 2 discriminant functions
library(fpc)
plotcluster(mydata, fit$cluster) 