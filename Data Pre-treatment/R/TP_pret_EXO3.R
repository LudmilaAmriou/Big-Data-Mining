install.packages("FSelector")
library(FSelector)

# Load the CSV file data
dataset <- read.csv("/home/ludmila/Downloads/archive/glass.csv")
dataset
# View the first few rows of the data
weights <- information.gain(Type~., dataset)
print(weights)
subset <- cutoff.k(weights, 2)
subset1 <- cutoff.k.percent(weights,0.5)
f <- as.simple.formula(subset, "Type")
print(f)

weights_gr <- gain.ratio(Type ~ ., dataset)
print(weights_gr)
subset_gr <- cutoff.k(weights_gr, 2)
f1 <- as.simple.formula(subset_gr, "Type")
print(f1)

