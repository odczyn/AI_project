library(ggplot2)
library(cowplot)
library(randomForest)

data = read.csv("D:\\szkulka\\semestr V\\AI\\projekt\\cmc.data")

colnames(data) <- c("wife.age",
                    "wife.education",
                    "husband.education",
                    "NO.children",
                    "wife.religion",
                    "wife.work",
                    "husband.occupation",
                    "living.standard",
                    "media.exposure",
                    "CM")
data$CM <- as.factor(data$CM)

#deklaracja zmiennych potrzebnych do obliczenia średniej i odchylenia standardowego ze 100 prob
total_accuracy = 0
accuracy_array = matrix(data=NA, nrow=1, ncol=100)


set.seed(3)
 

for(i in 1:100){
  
  model <- randomForest(CM~., data=data, ntree=80, proximity=TRUE)
  
  #confusion matrix wchodzi w skład funkcji randomForest
  accuracy_array[1,i] <- sum(diag(model$confusion))/sum(model$confusion) #pobieranie danych do obliczenia odchylenia
  total_accuracy <- total_accuracy + sum(diag(model$confusion))/sum(model$confusion) #obliczenia do sredniej ze 100 prob
}

#obliczanie średniej trafności dla stu prób (total_accuracy)
total_accuracy <- total_accuracy/100

#obliczanie odchylenia standardowego (sd)
tmp = 0
for(i in 1:100){
  tmp <- tmp + (accuracy_array[1,i] - total_accuracy)
}

odchylenie <- sqrt((tmp * tmp)/100)

total_accuracy
odchylenie
model
plot(model)

#PCA
str(data)
data$CM <- as.numeric(data$CM)

pca.data <- data[,1:9]
pca <- princomp(pca.data, cor=TRUE)
pca
summary(pca)