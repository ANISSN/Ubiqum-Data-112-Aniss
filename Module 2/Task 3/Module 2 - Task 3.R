library(caret)
library(ggplot2)
library(dplyr)
library(reshape2)
library(corrplot)

#Importing the data
Eproducts <- read.csv("C:\\Users\\nisso\\Desktop\\Ubiqum\\Module 2 - Task 3\\existingproductattributes2017.csv")
summary(Eproducts)
str(Eproducts)
sum(is.na(Eproducts))

#Converting to the right data type
Eproducts$BestSellersRank <- factor(Eproducts$BestSellersRank)
Eproducts$ProductNum <- factor(Eproducts$ProductNum)

#Selecting only numeric attributes
Eproducts_num <- Eproducts %>% select_if(is.numeric)

#Creating the correlation matrix
cormat <- Eproducts_num %>% select_if(is.numeric) %>% cor(.) %>% round(.,2)
corrplot(cormat) #display showing corrplot
head(cormat)
melted_cormat <- melt(cormat)
head(melted_cormat)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value))+ #display showing ggplot
  geom_tile()+
  scale_fill_gradient2(low = "blue", high = "red4", mid = "white", midpoint = 0, limit = c(-1,1))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Remonving outliers
Poutliers <- boxplot(Eproducts$Volume, plot=TRUE)$out
Eproducts <- Eproducts[-which(Eproducts$Volume %in% Poutliers),]
Poutliers <- max(boxplot(Eproducts$x1StarReviews, plot=TRUE)$out)
Eproducts <- Eproducts[-which(Eproducts$x1StarReviews %in% Poutliers),]

#Removing attributes with high correlation
Eproducts_NC <- Eproducts %>% 
  select("Volume","x4StarReviews","x2StarReviews","PositiveServiceReview")

#Removing duplicates
Eproducts_NC <- distinct(Eproducts_NC)

#-----------------------------------------------------------------------------

#Training models (Linear regression)
#---
set.seed(321)
#indTrain <- createDataPartition(y=Eproducts_NC$Volume, p=0.75, list=FALSE)
#trainProducts <- Eproducts_NC[indTrain,]
#testProducts <- Eproducts_NC[-indTrain,]
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
LRmodel <- train(Volume~.,data=Eproducts_NC,method="lm",trControl=fitControl,tuneLength=1,preProc=c("center","scale"))
LRmodel
LRtestVolumes <- predict(LRmodel,newdata=Eproducts_NC)
ggplot(mapping=aes(x=Eproducts_NC$Volume,y=LRtestVolumes)) +
  geom_point() + geom_smooth(method = "lm",se=FALSE)

#Training models (KNN)
#---
set.seed(321)
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
rfGrid <- expand.grid(k=1:20)
KNNmodel <- train(Volume~.,data=Eproducts_NC,method="knn",trControl=fitControl,tuneGrid=rfGrid,preProc=c("center","scale"))
KNNmodel
KNNtestVolumes <- predict(KNNmodel,newdata=Eproducts_NC)
ggplot(mapping=aes(x=Eproducts_NC$Volume,y=KNNtestVolumes)) +
  geom_point() + geom_smooth(method = "lm",se=FALSE)

#Training models (Random Forest)
#---
set.seed(321)
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
RFmodel <- train(Volume~.,data=Eproducts_NC,method="rf",trControl=fitControl,tuneGrid=expand.grid(.mtry = (10:30)),preProc=c("center","scale"))
RFmodel
RFtestVolumes <- predict(RFmodel,newdata=Eproducts_NC)
ggplot(mapping=aes(x=Eproducts_NC$Volume,y=RFtestVolumes)) +
  geom_point() + geom_smooth(method = "lm",se=FALSE)

#Training models (SVM)
#---
set.seed(321)
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
SVMmodel <- train(Volume~.,data=Eproducts_NC,method="svmLinear",trControl=fitControl,tuneLength=10,preProc=c("center","scale"))
SVMmodel
SVMtestVolumes <- predict(SVMmodel,newdata=Eproducts_NC)
ggplot(mapping=aes(x=Eproducts_NC$Volume,y=SVMtestVolumes)) +
  geom_point() + geom_smooth(method = "lm",se=FALSE)

#Recap of models
#---
model_names <- c("LR","KNN","RF","SVM (linear)")
RMSE <- c(min(LRmodel$results$RMSE),
          min(KNNmodel$results$RMSE),
          min(RFmodel$results$RMSE),
          min(SVMmodel$results$RMSE))
Rsquared <- c(LRmodel$results$Rsquared[match(RMSE[1],LRmodel$results$RMSE)],
              KNNmodel$results$Rsquared[match(RMSE[2],KNNmodel$results$RMSE)],
              RFmodel$results$Rsquared[match(RMSE[3],RFmodel$results$RMSE)],
              SVMmodel$results$Rsquared[match(RMSE[4],SVMmodel$results$RMSE)])
Recap <- data.frame(model_names,RMSE,Rsquared)

#Showing "RMSE" of all models
ggplot(data=Recap,mapping=aes(x=model_names,y=RMSE,fill=model_names,
                                  label=sprintf("%0.1f", round(RMSE, digits = 1)))) +
  geom_bar(stat='identity')+
  theme(legend.position="none",axis.title.x = element_blank())+
  geom_text(size = 3,vjust=-0.5)+
  scale_fill_manual("legend", values = c("LR"="orangered3",
                                         "KNN" = "royalblue3",
                                         "RF" = "lightpink2",
                                         "SVM (linear)" = "darkgreen"))

#Showing "Rsquared" of all models
ggplot(data=Recap,mapping=aes(x=model_names,y=Rsquared,fill=model_names,
                              label=sprintf("%0.3f", round(Rsquared, digits = 3)))) +
  geom_bar(stat='identity')+
  theme(legend.position="none",axis.title.x = element_blank())+
  geom_text(size = 3,vjust=-0.5)+
  scale_fill_manual("legend", values = c("LR"="orangered3",
                                         "KNN" = "royalblue3",
                                         "RF" = "lightpink2",
                                         "SVM (linear)" = "darkgreen"))

#-----------------------------------------------------------------------------

#Importing the data
Nproducts <- read.csv("C:\\Users\\nisso\\Desktop\\Ubiqum\\Module 2 - Task 3\\newproductattributes2017.csv")

#Pre-processing
Nproducts$BestSellersRank <- factor(Nproducts$BestSellersRank)
Nproducts$ProductNum <- factor(Nproducts$ProductNum)

#Predicting the volume of new products
NewVolumes <- predict(RFmodel,newdata=Nproducts)
Nproducts$VolumePredictions <- round(NewVolumes,digit=0)

#Creation of a CSV file
write.csv(Nproducts, file="C:\\Users\\nisso\\Desktop\\Ubiqum\\Module 2 - Task 3\\newproductattributes2017_predictions.csv", row.names = TRUE)

