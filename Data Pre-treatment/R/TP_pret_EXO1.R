
dataset <- airquality
sapply(dataset, function(x) length(unique(x)))
sapply(dataset,function(x) sum(is.na(x)))

sum(is.na(dataset$Ozone))
dataset[is.na(dataset$Ozone),]
dataset_opt1 <- airquality
dataset_opt1$Ozone <- NULL
missmap(dataset_opt1, main = "Missing values vs observed")

ds_option2 <- subset(dataset, !is.na(dataset$Ozone))
ds_option2 <- subset(ds_option2, !is.na(ds_option2$Solar.R))
ds_option2
missmap(ds_option2, main = "Missing values vs observed")

dataset_opt3 <- airquality
dataset_opt3$Ozone[is.na(dataset_opt3$Ozone)] <- mean(dataset_opt3$Ozone,na.rm=T)
dataset_opt3$Solar.R[is.na(dataset_opt3$Solar.R)] <- mean(dataset_opt3$Solar.R,na.rm=T)
missmap(dataset_opt3, main = "Missing values vs observed")

ds_option4 <- airquality 
ds_option4$Ozone[is.na(ds_option4$Ozone)] <- median(ds_option4$Ozone,na.rm=T)
ds_option4$Solar.R[is.na(ds_option4$Solar.R)] <- median(ds_option4$Solar.R,na.rm=T)
ds_option4
missmap(ds_option4, main = "Missing values vs observed")

reg = lm(Ozone~.,dataset)
summary(reg)
dataset[is.na(dataset$Ozone), "Ozone"] <- predict(reg, newdata = dataset[is.na(dataset$Ozone), ])
dataset

boxplot(airquality$Ozone)
boxplot(airquality$Wind)
boxplot.stats(airquality$Ozone)
