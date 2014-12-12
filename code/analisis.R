library(Hmisc)
library(bootstrap)
library(relaimpo)
library(MASS)
library(leaps)
library(car)

folder <- paste0(getwd(),"/data/DatosCompletos.csv")

datos <- read.csv(folder, sep=";", stringsAsFactors=FALSE)
datos <- na.omit(datos) # listwise deletion of missing
datos <- datos[datos$year<2013, ]

# Vemos las variables
keeps <- c("images","texts","videos","sounds","urls","no_media")
nodos <- datos[keeps]

# Correlations/covariances among numeric variables in 
# data frame mtcars. Use listwise deletion of missing data. 
cor(nodos, method="spearman") 
cov(nodos) 

# Correlations with significance levels
rcorr(as.matrix(nodos), type="pearson") 

# Multiple Linear Regression Example 
fit <- lm(nodes ~ images + videos + sounds + texts + urls, data=datos)
summary(fit) # show results
#layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(fit)

# Other useful functions 
coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters 
fitted(fit) # predicted values
residuals(fit) # residuals
anova(fit) # anova table 
vcov(fit) # covariance matrix for model parameters 
influence(fit) # regression diagnostics 

# define functions 
theta.fit <- function(x,y){lsfit(x,y)}
theta.predict <- function(fit,x){cbind(1,x)%*%fit$coef} 

# matrix of predictors
X <- as.matrix(datos[c("images","texts","videos","sounds","urls")])
# vector of predicted values
y <- as.matrix(datos[c("nodes")]) 

results <- crossval(X,y,theta.fit,theta.predict,ngroup=10)
cor(y, fit$fitted.values)**2 # raw R2 
cor(y,results$cv.fit)**2 # cross-validated R2 

# Stepwise Regression
step <- stepAIC(fit, direction="both")
step$anova # display results 

# All Subsets Regression
attach(datos)
leaps<-regsubsets(nodos~imagenes+videos+sonidos+textos+urls,data=datos,nbest=10)
# view results 
summary(leaps)
# plot a table of models showing variables in each model.
# models are ordered by the selection statistic.
plot(leaps,scale="r2")
# plot statistic by subset size 

subsets(leaps, statistic="rsq") 

# Calculate Relative Importance for Each Predictor
calc.relimp(fit,type=c("lmg","last","first","pratt"),rela=TRUE)

# Bootstrap Measures of Relative Importance (1000 samples) 
boot <- boot.relimp(fit, b = 1000, type = c("lmg", "last", "first", "pratt"), rank = TRUE, diff = TRUE, rela = TRUE)
booteval.relimp(boot) # print result
plot(booteval.relimp(boot,sort=TRUE)) # plot result 
