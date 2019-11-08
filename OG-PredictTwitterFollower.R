rm(list = ls())
dir_path <-"C:/Users/Onontsatsal/Dropbox/UIC/20181Spring/IDS575/Project"
setwd(dir_path)

data <-read.csv("BigDataGal_combo.csv", header = TRUE)
dupes <- duplicated(data$screenName)
data <- data[-which(dupes),]
str(data)

df <- data[,3:9]
df$createdDate <- as.numeric(as.Date(as.POSIXct(df$createdDate, origin='1970-1-1')))

set.seed(2017)			
train <- sample(nrow(df),nrow(df)*.7)		
train.df <- df[train,]	# Training Data
test.df <- df[-train,]	# Test Data

#Experiment kmeans clustering
train.scaled <- scale(train.df)
wss <- (nrow(train.scaled)-1)*sum(apply(train.scaled,2,var))
for (i in 1:15) wss[i] <- sum(kmeans(train.scaled, centers=i, iter.max = 20)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares",
     main="Clusters", pch=20, cex=2)

km.out <- kmeans(train.df,7, nstart = 20)
head(km.out$cluster)
km.out$withinss

plot(train.scaled, col =(km.out$cluster) , main="K-Means result with 7 clusters", pch=20, cex=2)

plot(km.out$centers[2,],type = "o",col = "red", lwd=2, main = "KMeans 7 centers")
lines(km.out$centers[1,], type = "o", col = "blue", lwd=2)
lines(km.out$centers[3,], type = "o", col = "green", lwd=2)
lines(km.out$centers[4], type = "o", col = "cyan", lwd=3)
lines(km.out$centers[5,], type = "o", col = "yellow", lwd=3)
lines(km.out$centers[6,], type = "o", col = "gray", lwd=3)
lines(km.out$centers[7,], type = "o", col = "orange", lwd=3)

#dividing data into 7 clusters / dataframes
clus_1 <- train.df[km.out$cluster ==1,]
clus_2 <- train.df[km.out$cluster ==2,]
clus_3 <- train.df[km.out$cluster ==3,]
clus_4 <- train.df[km.out$cluster ==4,]
clus_5 <- train.df[km.out$cluster ==5,]
clus_6 <- train.df[km.out$cluster ==6,]
clus_7 <- train.df[km.out$cluster ==7,]

#finding mean of follower count of each cluster
mean_1 <- mean(clus_1$folCount)
mean_2 <- mean(clus_2$folCount)
mean_3 <- mean(clus_3$folCount)
mean_4 <- mean(clus_4$folCount)
mean_5 <- mean(clus_5$folCount)
mean_6 <- mean(clus_6$folCount)
mean_7 <- mean(clus_7$folCount)

#classification on kmeans cluster
train.class <- cbind(train.df[1:5], cluster = as.factor(km.out$cluster))
train.class <- train.class[, -c(2)]
library(randomForest)
rf <- randomForest(cluster~., data = train.class, ntree = 1000, replace= TRUE)

#custom functions
rmse <- function(actual_val, pred_val) mean(sqrt((actual_val-pred_val)^2))
log.plus1 <- function(baseval) log(baseval+1) 

#randomforest regression for clusters
rf_fit1 <- randomForest(folCount~., data = clus_1, ntree = 500)
rf_fit2 <- randomForest(folCount~., data = clus_2, ntree = 500)
rf_fit3 <- randomForest(folCount~., data = clus_3, ntree = 500)
rf_fit4 <- randomForest(folCount~., data = clus_4, ntree = 500)
rf_fit5 <- randomForest(folCount~., data = clus_5, ntree = 500)
rf_fit6 <- randomForest(folCount~., data = clus_6, ntree = 500)
rf_fit7 <- randomForest(folCount~., data = clus_7, ntree = 500)

#clus_fit1 <- getBestRegression(clus_1)
#clus_fit2 <- getBestRegression(clus_2)
#clus_fit3 <- getBestRegression(clus_3)
#clus_fit4 <- getBestRegression(clus_4)
#clus_fit5 <- getBestRegression(clus_5)
#clus_fit6 <- getBestRegression(clus_6)
#clus_fit7 <- getBestRegression(clus_7)

#running whole thing on test data
#test.df
test_clusters <- predict(rf, newdata = test.df)

t.mydata <- cbind(test.df, cluster = as.factor(test_clusters))

test_clus1 <- t.mydata[t.mydata$cluster ==1,]
test_clus1 <- test_clus1[order(test_clus1$folCount),]

test_clus2 <- t.mydata[t.mydata$cluster ==2,]
test_clus2 <- test_clus2[order(test_clus2$folCount),]

test_clus3 <- t.mydata[t.mydata$cluster ==3,]
test_clus3 <- test_clus3[order(test_clus3$folCount),]

test_clus4 <- t.mydata[t.mydata$cluster ==4,]
test_clus4 <- test_clus4[order(test_clus4$folCount),]

test_clus5 <- t.mydata[t.mydata$cluster ==5,]
test_clus5 <- test_clus5[order(test_clus5$folCount),]

test_clus6 <- t.mydata[t.mydata$cluster ==6,]
test_clus6 <- test_clus6[order(test_clus6$folCount),]

test_clus7 <- t.mydata[t.mydata$cluster ==7,]
test_clus7 <- test_clus7[order(test_clus7$folCount),]

test_pred_FC1<- predict(rf_fit1, test_clus1)
test_pred_FC2<- predict(rf_fit2, test_clus2)
test_pred_FC3<- predict(rf_fit3, test_clus3)
test_pred_FC4<- predict(rf_fit4, test_clus4)
test_pred_FC5<- predict(rf_fit5, test_clus5)
test_pred_FC6<- predict(rf_fit6, test_clus6)
test_pred_FC7<- predict(rf_fit7, test_clus7)

# test_pred_FC1<- predict(clus_fit1, test_clus1)
# test_pred_FC2<- predict(clus_fit2, test_clus2)
# test_pred_FC3<- predict(clus_fit3, test_clus3)
# test_pred_FC4<- predict(clus_fit4, test_clus4)
# test_pred_FC5<- predict(clus_fit5, test_clus5)
# test_pred_FC6<- predict(clus_fit6, test_clus6)
# test_pred_FC7<- predict(clus_fit7, test_clus7)

#install.packages("dplyr")
library(dplyr)

df.pred <- as.data.frame(rbind(cbind(actual = test_clus1$folCount, pred = test_pred_FC1), 
                               cbind(actual = test_clus2$folCount, pred = test_pred_FC2),
                               cbind(actual = test_clus3$folCount, pred = test_pred_FC3),
                               cbind(actual = test_clus4$folCount, pred = test_pred_FC4),
                               cbind(actual = test_clus5$folCount, pred = test_pred_FC5),
                               cbind(actual = test_clus6$folCount, pred = test_pred_FC6),
                               cbind(actual = test_clus7$folCount, pred = test_pred_FC7)))

df.sorted <- arrange(df.pred, df.pred$actual)

plot(1:nrow(df.sorted), log.plus1(df.sorted$actual), type="l", col = "black", 
     main = "Log Transformation of Follower count and Predictions")
lines(1:nrow(df.sorted), log.plus1(df.sorted$pred), col = "red")
lines(1:nrow(df.sorted), log.plus1(df.sorted$actual), col = "black", lwd = 3)
legend("topleft", lty = 1, col = c("black", "red"), 
       legend = c("Original", "Kmean class + RF reg"), cex=.8)

rfRegError <- rmse(df.pred$actual, df.pred$pred)
#multiRegError <- rmse(df.pred$actual, df.pred$pred)

