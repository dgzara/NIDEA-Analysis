library(MASS) 

# Vemos las variables
nodos <- c(305, 198, 74, 201, 147, 98)

chisq.test(nodos) 

# Vemos por topicos
folder <- paste0(getwd(),"/data/porTopicos.csv")
topicos <- read.csv(folder, sep=";", stringsAsFactors=FALSE, row.names = 1)
topicos <- na.omit(topicos) # listwise deletion of missing
chisq.test(topicos)

# Vemos por autores
folder2 <- paste0(getwd(),"/data/porAutor.csv")
autores <- read.csv(folder2, sep=";", stringsAsFactors=FALSE, row.names = 1)
autores <- na.omit(autores) # listwise deletion of missing
autores <- autores[-c(3, 9, 10), ]
chisq.test(autores)
