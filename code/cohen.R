library(effsize)

folder <- paste0(getwd(),"/data/Datos.csv")
datos <- read.csv(folder, sep=";", stringsAsFactors=FALSE)
datos <- na.omit(datos) # listwise deletion of missing

keeps <- c("images","texts", "sounds","videos", "urls","no_media")
mydata <- datos[keeps]

cohen.d(mydata$images,mydata$texts)
cohen.d(mydata$images,mydata$videos)
cohen.d(mydata$images,mydata$urls)
cohen.d(mydata$urls,mydata$sounds)
