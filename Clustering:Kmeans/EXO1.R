# Le code suivant ne contient que les parties les plus importantes de l'EXO 


library(FactoMineR)
library(factoextra)
library(mlbench)
library(FSelector)
library(cluster)

data(Glass)



# Voir caracteristiques Data
str(Glass)
summary(Glass)
dim(Glass)

# Separer labels et features
glass_features <- Glass[, 1:9]
glass_labels <- as.factor(Glass[, 10])

# ACP
resultats_acp <- PCA(glass_features, scale =  TRUE)

# Voir resultats
print(resultats_acp)


# PLot coude 1
fviz_nbclust(x = glass_features,FUNcluster = kmeans, method = 'wss' )

# PLot coude 2
k.max <- 10
data<- scale(glass_features)
wss <- sapply(1:k.max, function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss, type="b", pch = 19, frame = FALSE, xlab="Number of clusters K", ylab="Total
within-clusters sum of squares")
data1 = resultats_acp$ind$coord[,1:5]


# AVEC ACP 
T1 <- Sys.time()
C1 = kmeans(data1,6)
T2 <- Sys.time()
T2 - T1


# SANS ACP
T1 <- Sys.time()
C = kmeans(data,6)
T2 <- Sys.time()
T2 - T1


# Feature Selection preparation
data = Glass
result <- cfs(Type~ ., data)
result 
f <- as.simple.formula(result, "Type")
data$RI = NULL 
data$Na = NULL
data$Si = NULL
data$Fe = NULL 
data$Type = NULL
head(data)

# Plot le coude
k.max <- 10
data<- scale(data)
wss <- sapply(1:k.max, function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss, type="b", pch = 19, frame = FALSE, xlab="Number of clusters K", ylab="Total
within-clusters sum of squares")

# Avec FS
T1 <- Sys.time()
C2 = kmeans(data,6)
T2 <- Sys.time()
T2 - T1
C2
