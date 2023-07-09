# Importer dataset: # voir rappel + precision dans ce cas voir si il y a overfitting
library(mlbench)
library(rpart)
library(rpart.plot)

data("Shuttle")

# Afficher dataset
data = Shuttle
head(data)

# Partitionner le dataset en échantillon d'apprentissage et échantillon test:
n=dim(data)[1]
index = sample(n, 0.7 * n)
Appren = data[index, ]
Test = data[-index, ]

# Nature des données
lapply(data,class)

# Arbre de decision
T1 <- Sys.time()
Tree<-rpart(Class~.,data=Appren)
#par classe
pred=predict(Tree,Test, type="class")
#Performance: table de confusion
tab=table(Test$Class, pred)
T2 <- Sys.time()
Te = T2 - T1
Tree
prp(Tree)

# Calculer les metriques de performances
accuracy <- sum(diag(tab))/sum(tab)
recall <- mean(diag(tab)/rowSums(tab))
precision <- ifelse(colSums(tab)==0, 0, diag(tab)/colSums(tab))
fscore <- mean(2*recall*diag(tab)/(rowSums(tab)+diag(tab)))

# Affichage
cat("Accuracy1: ", accuracy, "\n")
cat("Recall1: ", mean(recall), "\n")
cat("F-score1: ", mean(fscore), "\n")
cat("Execution time1: ", Te, "\n")
cat("Precision1: ", mean(precision), "\n")
# Par validation croisée l'erreur en fonction du nombre de feuilles
plotcp(Tree)

# Obtention de l'arbre simplifié
TreeSimple<-prune(Tree,cp=0.051)

# Arbre optimal
prp(TreeSimple,extra=1)
T1 <- Sys.time()
# prevision
predict(TreeSimple,data[1:9])
predict(Tree,data)


# par classe
pred=predict(Tree,Test, type="class")
# Performance: table de confusion
tab2 = table(Test$Class, pred)
# predict data
predict(Tree,data)
T2 <- Sys.time()
# elagage
#prune(arbre,0.02)
Te2<- T2-T1
# Calculer les metriques de performances
accuracy2 <- sum(diag(tab2))/sum(tab2)
recall2 <- diag(tab2)/rowSums(tab2)
fscore2 <- 2*recall*diag(tab2)/(rowSums(tab2)+diag(tab2))
precision2 <- ifelse(colSums(tab2)==0, 0, diag(tab2)/colSums(tab2))

# Affichage
cat("Accuracy2: ", accuracy2, "\n")
cat("Recall2: ", mean(recall2), "\n")
cat("F-score2: ", mean(fscore2), "\n")
cat("Execution time2: ", Te2, "\n")
cat("Precision2: ", mean(precision2), "\n")

# Random Forest
library(randomForest)
RF=randomForest(Class~., data=Shuttle)
# Changer nombre d'arbres (Greed search)
RF


