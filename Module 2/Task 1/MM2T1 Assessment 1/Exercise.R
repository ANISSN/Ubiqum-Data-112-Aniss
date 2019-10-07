library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)

Plants <- read.csv("C:\\Users\\nisso\\Desktop\\Ubiqum\\Module 2 - Task 1\\Exercise\\exercise.csv")

#Removing rows with missing species
#Plants <- drop_na(Plants,Species)
Plants <- na.omit(Plants,cols="Species")
is.na(Plants$Species)

#Replacing missing values with median
Plants$Sepal.Length[is.na(Plants$Sepal.Length)] <- median(Plants$Sepal.Length,na.rm = TRUE)
Plants$Sepal.Width[is.na(Plants$Sepal.Width)] <- median(Plants$Sepal.Width,na.rm = TRUE)
Plants$Petal.Length[is.na(Plants$Petal.Length)] <- median(Plants$Petal.Length,na.rm = TRUE)
Plants$Petal.Width[is.na(Plants$Petal.Width)] <- median(Plants$Petal.Width,na.rm = TRUE)

#Removing the outliers
outliers_SL <- boxplot(Plants$Sepal.Length, plot=TRUE)$out
Plants <- Plants[-which(Plants$Sepal.Length %in% outliers_SL),]
outliers_SW <- boxplot(Plants$Sepal.Width, plot=TRUE)$out
Plants <- Plants[-which(Plants$Sepal.Width %in% outliers_SW),]
outliers_PL <- boxplot(Plants$Petal.Length, plot=TRUE)$out
Plants <- Plants[-which(Plants$Petal.Length %in% outliers_PL),]
outliers_PW <- boxplot(Plants$Petal.Width, plot=TRUE)$out
Plants <- Plants[-which(Plants$Petal.Width %in% outliers_PW),]

#Rename "Species" to "Plants"
names(Plants)[names(Plants) == "Species"] <- "Plants"

Plants$Petal.Area <- Plants$Petal.Length*Plants$Petal.Width

#Correlation matrix
cormat <- Plants %>% select_if(is.numeric) %>% cor(.) %>% round(.,2)
head(cormat)

#Heatmap of the correlation matrix
melted_cormat <- melt(cormat)
head(melted_cormat)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + geom_tile() + scale_fill_gradient2(low = "blue", high = "red4", mid = "white", midpoint = 0, limit = c(-1,1))

#Plot petal width vs petal length
ggplot(data = Plants, aes(x=Petal.Width, y=Petal.Length, color=Plants)) + geom_point()

#Histogram of petal width variable
ggplot(data = Plants, aes(x=Petal.Width)) +
  geom_histogram(binwidth=0.1,color="black", fill="red4") +
  stat_bin(aes(y=..count.., label=..count..), binwidth=0.1, geom="text", vjust=-0.5)+
  scale_x_continuous(breaks=seq(0,max(Plants$Petal.Width), 0.1))


