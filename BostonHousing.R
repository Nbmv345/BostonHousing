library(caret)
library(class)
housing.df = read.csv("BostonHousing.csv")
set.seed(123)
entrenamiento.index <- sample(row.names(housing.df), 0.6*dim(housing.df)[1])  
validar.index <- setdiff(row.names(housing.df), entrenamiento.index)  
entrenamiento.df <- housing.df[entrenamiento.index, -14]
validar.df <- housing.df[validar.index, -14]

#Inciso A
#normalizamos los datos
entrenamiento.norm.df <- entrenamiento.df
validar.norm.df <- validar.df
housing.norm.df <-housing.df

norm.values <- preProcess(entrenamiento.df, method=c("center", "scale"))
entrenamiento.norm.df <- as.data.frame(predict(norm.values, entrenamiento.df))
validar.norm.df <- as.data.frame(predict(norm.values, validar.df))
housing.norm.df <- as.data.frame(predict(norm.values, housing.df))

precision.df <- data.frame(k = seq(1, 5, 1), RMSE = rep(0, 5))


for(i in 1:5){
  knn.pred<-class::knn(train = entrenamiento.norm.df[,-13],                          
                         test = validar.norm.df[,-13],                          
                         cl = entrenamiento.df[,13], k = i)
  precision.df[i,2]<-RMSE(as.numeric(as.character(knn.pred)),validar.df[,13])
}

precision.df

#Inciso B

nuevo.df<-data.frame(0.2,0,7,0,0.538,6,62,4.7,4,307,21,10)
names(nuevo.df)<-names(entrenamiento.norm.df)[-13]


nuevo.norm.values <- preProcess(nuevo.df, method=c("center", "scale"))


nuevo.norm.df <- predict(nuevo.norm.values, newdata = nuevo.df)

nuevo.knn.pred <- class::knn(train = entrenamiento.norm.df[,-13],
                       test = nuevo.norm.df,
                       cl = entrenamiento.df$MEDV, k = 2)
nuevo.knn.pred

#Inciso C
nueva.precision.df<-RMSE(as.numeric(as.character(nuevo.knn.pred)),validar.df[,13])
nueva.precision.df
