# Charger les bibs
library(mlbench)
library(FactoMineR)
library(factoextra)
library(FSelector)
library(cluster)
library(MASS)
library(glmnet)

data(BostonHousing)
BostonHousing
dataset = BostonHousing

# Caracteristiques du dataset
summary(dataset)
dim(dataset)
names(dataset)
dataset$chas <- as.numeric(as.character(dataset$chas))
cor(dataset)
str(dataset)

# Regression lineaire
reg = lm(medv~.,dataset)
summary(reg)

# Calculer l'erreur
mse <- mean(resid(reg)^2)
mse
rmse = sqrt(mse)
rmse

# Regression-ENL
# Suppression variables qui n'ont pas d'effet lineaire
dataset$indus <- NULL
dataset$age <- NULL

# Refaire la regression lineaire
reg2 = lm(medv~.,dataset)
summary(reg2)

# Calculer l'erreur
mse <- mean(resid(reg2)^2)
mse
rmse = sqrt(mse)
rmse

# Regression + ACP



# ACP
dataset2 = BostonHousing
dataset2 <- dataset2[0:13]
dataset2
dataset2$chas <- as.numeric(as.character(dataset2$chas))
resultats_acp <- PCA(dataset2, scale = TRUE)
summary(resultats_acp)
dataset2 = resultats_acp$ind$coord[,1:5]
head(dataset2)
medv = BostonHousing[14]
medv
data = cbind(dataset2,medv)
head(data)

# Refaire la regression lineaire
reg3 = lm(medv~.,data)
summary(reg3)

# Calculer l'erreur
mse <- mean(resid(reg3)^2)
mse
rmse = sqrt(mse)
rmse

# Reg + Ridge
mod.ridge = lm.ridge(medv~.,dataset4,lambda=seq(0,20,0.1)) # Grid search
par(mfrow=c(2,1))
plot(mod.ridge)
matplot(t(mod.ridge$coef),lty=1:3,type='l',col=1:10)
legend("top",legend=rownames(mod.ridge$coef), col=1:10,lty=1:3)
# cross validation (ici val min lambda)
plot(mod.ridge$lambda,mod.ridge$GCV)
summary(mod.ridge$lambda,mod.ridge$GCV)
select(mod.ridge)
# refaire avec lambda optimal
mod.ridge$coef
# comparer avec mse
mod.ridge=lm.ridge(medv~crim+zn+chas+nox+rm+dis+rad+tax+ptratio+b+lstat,dataset4,lambda=4.3)
X.matrix <-cbind(rep(1,length=length(dataset4$medv)),dataset4$crim,dataset4$zn,dataset4$chas,dataset4$nox,dataset4$rm,dataset4$dis,dataset4$rad,dataset4$tax,dataset4$ptratio,dataset4$b,dataset4$lstat)
X.matrix1 <- as.matrix(dataset4[,-9])
head(X.matrix)
head(BostonHousing)
fitted.vals<-X.matrix %*%c(32.918471472,-0.104315656,0.043678727,2.747912270,-17.683126171,3.851764685,-1.426638997,0.272261020,-0.010629921,-0.935344659,0.009278324,-0.516975577)
mse.ridge=mean((dataset4$medv-fitted.vals)^2)
mse.ridge
rse.ridge = sqrt(mse.ridge)

# Reg + LASSO

# Donnees matrices
matrixx = model.matrix(medv~.,data)
# vars de sortie
Y = data$medv 
# Faire la regréssion Lasso (varier lambda)
lasso = cv.glmnet(matrixx,Y, alpha = 1, lambda = seq(0,1000, 10), grouped = FALSE, nfolds = nrow(data))
# Afficher le graphe 
plot(lasso,main = "Choix de lambda")
# Calculer la valeur optimal de lambda
lambda_optimal = lasso$lambda.min
lambda_optimal
# VOir les coeff
m_lasso = glmnet(matrixx,Y, alpha = 1, lambda = lambda_optimal)
coef(m_lasso)

# Afficher l'erreur (MSE)
MSE = min(lasso$cvm)
MSE 
RMSE = sqrt(MSE)
RMSE  
