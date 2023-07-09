library(nnet)
library(mlbench)
library(rpart)
library(rpart.plot)
library(e1071)
library(randomForest)

# Importer dataset
data(Glass)
data = Glass

# Partitionner en train et test set
n=dim(Glass)[1]
index = sample(n, 0.7 * n)
Appren = Glass[index, ]
Test = Glass[-index, ]

# Preparer le reseau de neurones et effectuer l'apprentissage
RN<- nnet(Type~., Appren, size = 9, decay = )
summary(RN)

# Prediction dans la partie du test
pred=predict(RN,Test, type="class")

# Calcul de la table de confusion
Confusion = table(Test$Type,pred)

# Calcul de l'erreur
err<- 1-sum(diag(Confusion))/sum(Confusion)
err

# On tune le model en prennant le size entre 4 et 9 
results<-tune.nnet(Type~.,data=Glass,size = 4:9)
best_size <- results$best.parameters$size # avoir le best size 

cat("best size: ", best_size, "\n")
cat("best decay: ", best_decay, "\n")
T1 <- Sys.time()
model <- nnet(Type ~ ., data = Appren, size = best_size, decay =0.4, maxit = 500)
pred <- predict(model, Test, type = "class")
T2 <- Sys.time()
confusion <- table(Test$Type, pred)
accuracy <- sum(diag(confusion)) / sum(confusion)
Te <- T2-T1
accuracy
cat("Accuracy Nnet: ", accuracy, "\n")
cat("Exec Time Nnet: ", Te, "\n")


# Debut de comparaison des algorithmes: 

# Arbre de decision 
T1 <- Sys.time()
Tree<-rpart(Type~.,data=Appren)
#par classe
pred2=predict(Tree,Test, type="class")
#Performance: table de confusion
tab=table(Test$Type, pred2)
T2 <- Sys.time()
Te2 = T2 - T1

# Calculer les metriques de performances
accuracy2 <- sum(diag(tab))/sum(tab)
cat("Accuracy Decision tree: ", accuracy2, "\n")
cat("Exec time Decision Tree: ", Te2, "\n")



# Random Forest
T1 <- Sys.time()
model <- randomForest(Type ~ ., data = Appren)

# Predire dans le test set
pred <- predict(model, Test)
T2 <- Sys.time()
# Calculer accuracy
accuracy3 <- sum(pred == Test$Type) / nrow(Test)
Te3 <- T2-T1

cat("Accuracy Random Forest: ", accuracy3, "\n")
cat("Exec time Random Forest: ", Te3, "\n")


# SVM

# Training
T1 <- Sys.time()
svm_model <- svm(Type ~ ., data = Appren)

# Prediction dans test set
svm_pred <- predict(svm_model, Test)
T2 <- Sys.time()
Te4 <- T2-T1
# Calculer accuracy
svm_acc <- sum(svm_pred == Test$Type) / nrow(Test)
cat("Accuracy SVM: ", svm_acc, "\n")
cat("Exec Time SVM: ", Te4, "\n")

# SVM N EST PAS PENALISEE PAR LA Dimensionalite


