setwd("C:/Users/Максим/Desktop/Science/МГУ/6/Количественные методы")
df <- read.csv("WHI2019.csv", sep=";", dec=",", header=TRUE)

library(ggplot2)
library(FNN)
install.packages("dplyr")
library(dplyr)
library(stats)
install.packages("tictoc")
library(tictoc)
#Этот пакет достаточно большой, функция из него нарисует график
install.packages("car")
library(car)

n = 4
country = 'Russia'
fact = c(5,7)

#Функция для предсказывания классов
predict <- function(data, env_knn, threshold, var_col){
  pred_adv <- numeric(length(data[,1]))
  for (i in 1:length(data[,1])) {
    pred_adv[i] <- ifelse(sum(as.numeric(data[env_knn$nn.index[i,], var_col][-1]) * (1/as.numeric(env_knn$nn.dist[i,][-1]))^2/sum((1/as.numeric(env_knn$nn.dist[i,][-1]))^2)) > threshold, 1, 0)
  }
  return(pred_adv)
}

#Преобразую значения в листе для дальнейшей работы
for (i in 4:9) {
    df[, i] = as.numeric(df[, i])
}

summary(as.data.frame(df))

#Поиск 5 ближайших соседей для России с использованием алгоритма k-d дерево + таймер
tic("kd_tree_time")
kd_three_ru <- get.knnx(df[1:156, 4:9], query = df[df[2] == country, 4:9], k = 6, algorithm = "kd_tree")
toc()

print(df[kd_three_ru$nn.index[-1], 2])
print(df[kd_three_ru$nn.index[-1], c(2, 10)])
advanced_neighbors_ru <- df[kd_three_ru$nn.index[-1][df[kd_three_ru$nn.index[-1], 10] == 1], 2]
print(levels(advanced_neighbors_ru)[advanced_neighbors_ru])

#Поиск 5 ближайших соседей для России полным перебором + таймер
tic("brute_time")
brute_ru <- get.knnx(df[1:156, 4:9], query = df[df[2] == country, 4:9], k = 6, algorithm = "brute")
toc()

#Предсказание класса для всех стран 
kd_three <- get.knn(df[1:156, 4:9], k = 6, algorithm = "kd_tree")
predicted_level <- predict(df, kd_three, 0.5, 10)
#Вывод предсказанного значения класса для России
print(predicted_level[df[, 2] == country])

#Рассчёт accuracy rate для выбранной модели по выборке без России
accuracy_0 <- 1-sum(abs(predicted_level[df[2] != country] - as.numeric(df[df[2] != country,10])))/155

#Рассчёт accuracy rate для выбранной модели по выборке без России для разного количества соседей+ график
accuracy <- numeric(39)

for (i in 2:40) {
  iterator <- get.knn(df[df[2] != country, 4:9], k = i+1, algorithm = "kd_tree")
  predicted_advanced <- predict(df[df[2] != country,], iterator, 0.5, 10)
  #for (j in 1:155) {
  #  predicted_level[j] <- sum(as.numeric(df[iterator$nn.index[j,], 10][-1]) * (1/as.numeric(iterator$nn.dist[j,][-1]))^2/sum((1/as.numeric(iterator$nn.dist[j,][-1]))^2)) 
  #  predicted_advanced[j] <- ifelse(predicted_level[j] > 0.5, 1, 0)
  #}
  accuracy[i-1] <- 1-sum(abs(predicted_advanced - as.numeric(df[df[2] != country,10])))/155
}

#Тот самый график
accuracy
plot(accuracy, type = "b", col = "dodgerblue", cex = 0.5, pch = 20, 
     xlab = "k, number of neighbours", ylab = "classification accuracy",
     main = "Accuracy Rate of Class Prediction Regarding Number of Neighbours")

# Кластеризация по двум компонентам

clusters <- kmeans( df[, fact], n, nstart = 20, iter.max = 20, algorithm = "Lloyd")
clusters
near_centers <- get.knnx(df[1:156, fact], query = clusters$centers, k = 5, algorithm = "kd_tree")
for (i in 1:4) {
    print(paste("Cluster", toString(i, width = 1), "center nearest 5 countries:", paste(toString(levels(df[near_centers$nn.index[i,], 2])[df[near_centers$nn.index[i,], 2]]))))
}
print(paste(country, "belongs to cluster", clusters$cluster[df[2] == country]))
scatterplot(Social.support ~ Freedom.to.make.life.choices, data = df[, fact], groups=clusters$cluster, main = "Characteristics by Clusters", regLine=FALSE, smooth=FALSE, )

# Кластеризация по всем компонентам
clusters_all <- kmeans( df[,4:9], 4, nstart = 20, iter.max = 20, algorithm = "Lloyd")
clusters_all
print(paste(country, "belongs to cluster", clusters_all$cluster[df[2] == country]))

#Минимазация суммарного межгруппового расстояния
k_try <- 1:156
tot_within_ss <- numeric(39)
for (i in k_try) {
  clustering <- kmeans(df[,4:9], i, nstart = 20, iter.max = 20, algorithm = "Lloyd")
  tot_within_ss[i-1] <- clustering$tot.withinss
}
plot(tot_within_ss, type = "b", col = "dodgerblue", cex = 0.5, pch = 20, 
     xlab = "k, number of clusters", ylab = "Total Within Clusters Sum of Squares",
     main = "TWCSS Regarding Number of Clusters")
?plot
?scatterplot
?toString


