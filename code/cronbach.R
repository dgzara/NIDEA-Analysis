library("psy")

folder <- paste0(getwd(),"/data/Usability.csv")
datos <- read.csv(folder, sep=";", stringsAsFactors=FALSE)

cronbach(datos)
