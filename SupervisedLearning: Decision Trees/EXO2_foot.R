# Librairies 
library(FSelector)
library(rpart)

# Specifier le chemain du fichier .txt
setwd("/home/ludmila/")

# Lire les donnees 
data <- read.table("data2.txt", header = TRUE)

# Afficher les donnees
data

# Question 1: Quelle est la variable la plus pertinente en utilisant l'entropie?

# Calculer l'entropie pour chaque variable:
entropy <- information.gain(Apte ~ ., data)

# voir la valeur de chacune d'elles
entropy

# Selectionner la variable ayant la valeur d'entropie la plus elevée
most_relevant <- rownames(entropy)[which.max(entropy[, 1])]

# Afficher la variable
print(paste("L'entropie la plus elevee => la variable avec le gain le  plus important est : ", most_relevant))




# Question 2: Construire un arbre de décision:
lapply(data,class)

#arbre de décision
Tree <- rpart(Apte~.,data=data)
print(Tree)

# Dessiner l'arbre
#plot(Tree)


#par classe
pred=predict(Tree,data, type="class")

# Performance: table de confusion
tab=table(data$Apte, pred)

# Affichage
pred
tab

# Résultat
print(paste("Le fait que notre jeu de données et petit, l'apprentissage donc n'a pas été achevé et c'est pour cela qu'on remarque que le modele a confondu 4 yes comme étant no"))




# Question 3: Est ce que l'individu 9(Modéré,Non,Non) est apte a jouer? (Ici elle sera évidemment classée 'Non'...)
# On cree un dataframe pour l'individu
individual <- data.frame(Entrainement = "Modéré", Régime = "Non", Motivée = "Non")

# Prédiction
prediction <- predict(Tree, individual, type = "class")

# Voir la prédiction
prediction



