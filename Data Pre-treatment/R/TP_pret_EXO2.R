library(FSelector)
dataset <- iris
dataset
is.factor(dataset$Sepal.Length)

#dataset$Sepal.Length <- factor(dataset$Sepal.Length, levels = c(0, 1))
dataset
factor_vars <- c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width")
dataset[factor_vars] <- lapply(dataset[factor_vars], function(x) as.factor(x))
for(unique_value in unique(dataset$Species)){  dataset[paste("Species", unique_value, sep = ".")] <- ifelse(dataset$Species == unique_value, 1, 0)}
dataset
dataset$Species <- NULL

dataset

dataset$Sepal.Length <- as.numeric(as.character(dataset$Sepal.Length))
dataset$Sepal.Width <- as.numeric(as.character(dataset$Sepal.Width))
dataset$Petal.Length <- as.numeric(as.character(dataset$Petal.Length))
dataset$Petal.Width <- as.numeric(as.character(dataset$Petal.Width))
dataset
class(dataset$Sepal.Length)
# import the library
library(caret)
# function to implement min max scaling
minMax <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
normalisedData <- as.data.frame(lapply(dataset, minMax))
head(normalisedData)


weights <- information.gain(Petal.Length~., normalisedData)
print(weights)
subset <- cutoff.k(weights, 2)
f <- as.simple.formula(subset, "Petal.Length")
print(f)

