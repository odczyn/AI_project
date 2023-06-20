library(class)

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

#deklaracja i definicja mnożnika proporcji
p <- 0.7 

#deklaracja zmiennych potrzebnych do późniejszej analizy trafności
total_accuracy = 0
accuracy_array = matrix(data=NA, nrow=1, ncol=100)

set.seed(1)

for(i in 1:100){
  
  train.index <- sample.int(nrow(data),nrow(data)*p)
  data.train <- data[train.index,]
  data.test <- data[-train.index,]
  
  pred_knn = knn(data.train[,1:9],
                data.test[,1:9],
                data.train$CM,
                k = 87)
  
  #confusion matrix
  cf <- table(data.test$CM,pred_knn) 
  #pobieranie danych do obliczenia odchylenia
  accuracy_array[1,i] <- sum(diag(cf))/sum(cf) 
  #obliczenia do sredniej ze 100 prob
  total_accuracy <- total_accuracy + sum(diag(cf))/sum(cf) 

}

#obliczanie średniej trafności dla stu prób (total_accuracy)
total_accuracy <- total_accuracy/100

#obliczanie odchylenia standardowego (sd)
tmp = 0
for(i in 1:100){
  tmp <- tmp + (accuracy_array[1,i] - total_accuracy)
}
sd <- sqrt((tmp * tmp)/100)

total_accuracy
sd
cf