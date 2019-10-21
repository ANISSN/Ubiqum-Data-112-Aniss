# ############################################################################
# GOAL: Predict volume of future products
# DESCRIPTION: Training models (LR/KNN/RF/SVM) and use them ti predict volume
# AUTHOR: ANISS N
# ############################################################################

# Libraries ---------------------------------------------------------------
library(caret)
library(ggplot2)
library(dplyr)
library(reshape2)
library(corrplot)
library(fastDummies)


# Importing the data ------------------------------------------------------
Eproducts <- read.csv(
  paste0("C:\\Users\\nisso\\Desktop\\Ubiqum\\Module 2 - Task 3\\",
  "existingproductattributes2017.csv"))


# Preprocessing -----------------------------------------------------------
#Converting to the right data type
Eproducts$BestSellersRank <- factor(Eproducts$BestSellersRank)
Eproducts$ProductNum <- factor(Eproducts$ProductNum)

#Creating the correlation matrix
cormat <- Eproducts %>% select_if(is.numeric) %>% cor(.) %>% round(.,2)
corrplot(cormat)
head(cormat)
melted_cormat <- melt(cormat)
head(melted_cormat)
saveRDS(melted_cormat,file=
          paste0("C:\\Users\\nisso\\Desktop\\Ubiqum\\Module 2 - Task 3\\Data\\",
                 "correlation_matrix.rds"))
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value))+
  geom_tile()+
  scale_fill_gradient2(low = "blue", high = "red4", mid = "white", midpoint = 0,
                       limit = c(-1,1))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

#Remonving outliers
Poutliers <- boxplot(Eproducts$Volume, plot=TRUE)$out
Eproducts <- Eproducts[-which(Eproducts$Volume %in% Poutliers),]
Poutliers <- max(boxplot(Eproducts$x1StarReviews, plot=TRUE)$out)
Eproducts <- Eproducts[-which(Eproducts$x1StarReviews %in% Poutliers),]

#Removing attributes with high correlation
Eproducts_PP <- Eproducts %>% 
  select("Volume","x4StarReviews","x2StarReviews","PositiveServiceReview",starts_with("ProductType_"))
Eproducts_PP <- distinct(Eproducts_PP)
saveRDS(Eproducts_PP,file=
          paste0("C:\\Users\\nisso\\Desktop\\Ubiqum\\Module 2 - Task 3\\Data\\",
                 "Eproducts_PP.rds"))

# Creating dummies
Eproducts <- dummy_cols(Eproducts, select_columns = "ProductType")
#Removing attributes with high correlation but keeping Product Type
Eproducts_PP_PT <- Eproducts %>% 
  select("Volume","x4StarReviews","x2StarReviews","PositiveServiceReview",starts_with("ProductType_"))
#Removing duplicates
Eproducts_PP_PT <- distinct(Eproducts_PP_PT)
saveRDS(Eproducts_PP_PT,file=
          paste0("C:\\Users\\nisso\\Desktop\\Ubiqum\\Module 2 - Task 3\\Data\\",
                 "Eproducts_PP_PT.rds"))

# Functions ---------------------------------------------------------------
# Function : display reality vs prediction (reg)
reg_r_vs_p <- function(xx, yy){
  ggplot(mapping = aes(x = xx, y = yy)) +
    geom_point() + 
    geom_smooth(method = "lm", se = FALSE) +
    labs(x = "Reality", y = "Prediction")
}
# Function: training 4 models
get_models <- function(dataf){
  fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
  set.seed(321)
  LRmodel <- train(Volume~.,data=dataf,method="lm",
                   trControl=fitControl,tuneLength=1,preProc=c("center","scale"))
  set.seed(321)
  knnGrid <- expand.grid(k=1:20)
  KNNmodel <- train(Volume~.,data=dataf,method="knn",
                    trControl=fitControl,tuneGrid=knnGrid,
                    preProc=c("center","scale"))
  set.seed(321)
  RFmodel <- train(Volume~.,data=dataf,method="rf",
                   trControl=fitControl,tuneGrid=expand.grid(.mtry = (10:30)),
                   preProc=c("center","scale"))
  set.seed(321)
  SVMmodel <- train(Volume~.,data=dataf,method="svmLinear",
                    trControl=fitControl,tuneLength=10,
                    preProc=c("center","scale"))
  
  return(list(lr=LRmodel,knn=KNNmodel,rf=RFmodel,svm=SVMmodel))
}


# Training models ---------------------------------------------------------
mods_PP <- get_models(Eproducts_PP)
saveRDS(mods_PP,file=
          paste0("C:\\Users\\nisso\\Desktop\\Ubiqum\\Module 2 - Task 3\\Data\\",
                 "mods_PP.rds"))
mods_PP_PT <- get_models(Eproducts_PP_PT)
saveRDS(mods_PP_PT,file=
          paste0("C:\\Users\\nisso\\Desktop\\Ubiqum\\Module 2 - Task 3\\Data\\",
                 "mods_PP_PT.rds"))

#Recap of models
#---
model_names <- c("LR","KNN","RF","SVM (linear)")
RMSE <- c(min(mods[["lr"]]$results$RMSE),
          min(mods[["knn"]]$results$RMSE),
          min(mods[["rf"]]$results$RMSE),
          min(mods[["svm"]]$results$RMSE))
Rsquared <- c(mods[["lr"]]$results$Rsquared[match(RMSE[1],mods[["lr"]]$results$RMSE)],
              mods[["knn"]]$results$Rsquared[match(RMSE[2],mods[["knn"]]$results$RMSE)],
              mods[["rf"]]$results$Rsquared[match(RMSE[3],mods[["rf"]]$results$RMSE)],
              mods[["svm"]]$results$Rsquared[match(RMSE[4],mods[["svm"]]$results$RMSE)])
Recap <- data.frame(model_names,RMSE,Rsquared)

#Showing "RMSE" of all models
ggplot(data=Recap,mapping=aes(x=model_names,y=RMSE,fill=model_names,
                                  label=sprintf("%0.1f", round(RMSE,
                                                               digits = 1)))) +
  geom_bar(stat='identity')+
  theme(legend.position="none",axis.title.x = element_blank())+
  geom_text(size = 3,vjust=-0.5)+
  scale_fill_manual("legend", values = c("LR"="orangered3",
                                         "KNN" = "royalblue3",
                                         "RF" = "lightpink2",
                                         "SVM (linear)" = "darkgreen"))

#Showing "Rsquared" of all models
ggplot(data=Recap,mapping=aes(x=model_names,y=Rsquared,fill=model_names,
                              label=sprintf("%0.3f", round(Rsquared,
                                                           digits = 3)))) +
  geom_bar(stat='identity')+
  theme(legend.position="none",axis.title.x = element_blank())+
  geom_text(size = 3,vjust=-0.5)+
  scale_fill_manual("legend", values = c("LR"="orangered3",
                                         "KNN" = "royalblue3",
                                         "RF" = "lightpink2",
                                         "SVM (linear)" = "darkgreen"))


# Predicting volume for future products -----------------------------------
#Importing the data
Nproducts <- read.csv(
  paste0("C:\\Users\\nisso\\Desktop\\Ubiqum\\Module 2 - Task 3\\",
         "newproductattributes2017.csv"))

#Pre-processing
Nproducts$BestSellersRank <- factor(Nproducts$BestSellersRank)
Nproducts$ProductNum <- factor(Nproducts$ProductNum)

#Predicting the volume of new products
NewVolumes <- predict(RFmodel,newdata=Nproducts)
Nproducts$VolumePredictions <- round(NewVolumes,digit=0)


# Exporting the results into file -----------------------------------------
#Creation of a CSV file
write.csv(Nproducts, file=
            paste0("C:\\Users\\nisso\\Desktop\\Ubiqum\\Module 2 - Task 3\\",
            "newproductattributes2017_predictions.csv"), row.names = TRUE)

