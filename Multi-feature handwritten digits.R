##############################Reading of the different features#######################################
#setwd("~/Documents/Advanced data science - Lecture/mfeat")

setwd("C:/Users/Deborah Adigun/Desktop/AIMS2223/REVIEW/BLOCK 5/Advanced Data Science/Assignments/Group 4/mfeat")
mfeat_fac <- read.csv("mfeat-fac",sep ="",header = FALSE,col.names = paste0("fac_", seq_len(216)))
View(mfeat_fac)
mfeat_fou <- read.csv("mfeat-fou",sep="",header=FALSE,col.names = paste0("fou_", seq_len(76)))
View(mfeat_fou)
mfeat_kar <- read.csv("mfeat-kar",sep="",header=FALSE,col.names = paste0("kar_", seq_len(64)))
View(mfeat_kar)
mfeat_pix <- read.csv("mfeat-pix",sep="",header=FALSE,col.names = paste0("pix_", seq_len(240)))
View(mfeat_pix)
mfeat_zer<-read.csv("mfeat-zer",sep="",header=FALSE,col.names = paste0("zer_", seq_len(47)))
View(mfeat_zer)
mfeat_mor <- read.csv("mfeat-mor",sep="",header=FALSE,col.names = paste0("mor_", seq_len(6)))
View(mfeat_mor)

###################################merging all data frames together#####################################

mfeat <- cbind(mfeat_fou,mfeat_fac,mfeat_kar,mfeat_mor,mfeat_pix,mfeat_zer)
View(mfeat)
attach(mfeat)

################################Correlation plot#######################################################
library(corrplot)
mfeat_cor = cor(mfeat_mor)
corrplot(mfeat_cor, method="color", order="alphabet") #correlation plot of morphological features

corrplot(cor(mfeat),method = "color",order='alphabet') #correlation plot of the whole data set

#####################################PCA on the whole data set####################################3
pc <- prcomp(mfeat,
             center = TRUE,
             scale = TRUE)
attributes(pc)
pc$center
pc$rotation
pc$x
print(pc)
summary(pc)
###################k-means on PCA result########################
#Optimal number of cluster
mfeat_transform <- as.data.frame(-pc$x[,1:2])
fviz_nbclust(mfeat_transform,kmeans,method="wss")
#plot of cluster
kmeans_mfeat <- kmeans(mfeat_transform,centers=3,nstart=50)
fviz_cluster(kmeans_mfeat,data=mfeat_transform)

############################### Adding the target in our data set####################################
b <- rep(c(0, 1, 2,3,4,5,6,7,8,9), each = 200)
mfeat$digits <- as.factor(b)
View(mfeat)
table(mfeat$digits) #check of proportion of different classes
str(mfeat$digits) #check of type of the target
##############################biplot of our data set############################################
library(devtools)
library(ggbiplot)
g <- ggbiplot(pc,
              obs.scale = 1,
              var.scale = 1,
              groups = mfeat$digits,
              ellipse = TRUE,
              circle = TRUE,
              ellipse.prob = 0.68)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal',
               legend.position = 'top')
print(g)

############################Check of multicollinearity by using the correlation plot#################### 
corrplot(cor(pc$x),method = "color",order='alphabet')

############################Check of normality################################################
hist(pc$x, probability = TRUE)
lines(density(pc$x), col = 'green', lwd = 3)


##########################Splitting of the data set############################################
library(caret)
test_index <- createDataPartition(mfeat$digits, p=0.75, list=FALSE)
# select 25% of the data for validation
test <- mfeat[-test_index,]
# use the remaining 75% of data to training and testing the models
training <- mfeat[test_index,]
dim(test)
dim(training)

########################splitting inputs and output############################################
x <- training[,1:649] #training features
y <- training[,650] #training target

##########################################PCA on the training set####################################
pc1 <- prcomp(x,
             center = TRUE,
             scale. = TRUE)
attributes(pc1)
pc1$center
pc1$rotation
pc1$x
print(pc1)
summary(pc1)
######################  variables of the train set in terms of principal components################
trg <- predict(pc1,x)
dim(trg)
#We consider the 76th principal components and add the two the train set
trg <- data.frame(trg[,1:76],training[,650])
table(trg$training...650.) #proportion of classes in the train set
######################  variables of the test set in terms of principal components of train set################
tst <- predict(pc1,test[,1:649])
dim(tst)
tst <- data.frame(tst[,1:76],test[,650]) #We consider the first 76th principal components after expressing the test set in terms of principal components
dim(tst)
table(tst$test...650.) #proportion of classes in the train set

####################### Multinomial logistic regression #########################
library(nnet)
trg$training...650. <- relevel(trg$training...650., ref = "0")
mymodel <- multinom(training...650.~., data = trg)
summary(mymodel)

###########################Confusion matrix error training####################################
p <- predict(mymodel,trg)
tab <- table(p,trg$training...650.)
tab
1-sum(diag(tab))/sum(tab) #misclassification error on the training
sum(diag(tab))/sum(tab) #Accuracy during on training

#Confusion matrix error testing 
p1 <- predict(mymodel,tst)
tab1 <- table(p1,tst$test...650.)
tab1
1-sum(diag(tab1))/sum(tab1) #misclassification error on the testing
sum(diag((tab1)))/sum(tab1) #Accuracy on the test test
View(tst)

