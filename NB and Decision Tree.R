## LIBRARIES
install.packages("naivebayes")
library(naivebayes)
install.packages("rpart")
library(rpart)
install.packages('rpart.plot')
library(rpart.plot)
install.packages("ggplot2")
library(ggplot2)



getwd()
setwd("C:/Desktop/gitprojects/Naive Bayes and Decision Tree")
#read in files
test_digit <- read.csv("Kaggle-digit-test-sample1000.csv", 
                          header = TRUE, sep = ",", stringsAsFactors = FALSE)
train_digit <- read.csv("Kaggle-digit-train-sample-small-1400.csv",
                        header = TRUE, sep = ",", stringsAsFactors = FALSE)

#Check test data
head(test_digit)
dim(test_digit)
colnames(test_digit)
str(test_digit)
#Check train data
head(train_digit)
dim(train_digit)
colnames(train_digit)
str(train_digit)

######################################## Data Cleaning and Prep ------------------------

## Change class label into a factor
test_digit$label<- as.factor(test_digit$label)
train_digit$label<- as.factor(train_digit$label)
str(test_digit)
dim(test_digit)
str(train_digit)
dim(train_digit)
## Step 1: Create a new variable
train_digit$pixel97rank <- NULL
## Step 2: Fill in the values using the cut function
## We will need to know the min and max to do this
(max(train_digit$pixel97))
(min(train_digit$pixel97))

#descretize training set
train_digit$pixel97rank <- 
  cut(train_digit$pixel97, breaks=c(-Inf, 50, 100, 150, 200, Inf))
      labels=c("1","2","3","4","5")

## Step 1: Create a new variable
test_digit$pixel97rank <- NULL
## Step 2: Fill in the values using the cut function
## We will need to know the min and max to do this
(max(test_digit$pixel97))
(min(test_digit$pixel97))
#descretize test set
test_digit$pixel97rank <- 
     cut(test_digit$pixel97, breaks=c(-Inf, 50, 100, 150, 200, Inf))
     labels=c("1","2","3","4","5")
## Let's check out progress
(str(train_digit))
(str(test_digit))

#remove column pixel 97
(head(train_digitrank <- train_digit[,-c(785)]))
(head(test_digitrank <- test_digit[,-c(785)]))
     
##  Run the Naive Bayes using kernel and laplace
Bayes_digit <- naive_bayes(label ~., data=train_digitrank, laplace = 1)
Bayes_digit1 <- naive_bayes(label ~., data=train_digitrank, usekernel = T)

#Print
print(Bayes_digit)
print(Bayes_digit1)

## Visualizations for naive_bayes objects
plot(Bayes_digit)
plot(Bayes_digit1)

## See the prediction probabilities...
pred_digit1<-predict(Bayes_digit, test_digitrank, type="class")
Pred_TABLE1 <- table(Predicted=pred_digit1, test_digit$label)
Pred_TABLE1

#model accuracy
(sum(diag(Pred_TABLE1)/sum(Pred_TABLE1)))



###################DECISION TREE#############
Treefit <- rpart(train_digitrank$label~., data = train_digitrank, 
                 method="class")

printcp(Treefit)
predicted= predict(Treefit,test_digitrank, type="class")
(Results <- data.frame(Predicted=predicted,Actual=test_digitrank))
fancyRpartPlot(Treefit, uniform=TRUE,
               main="Classification Tree", 
               cex=.6, tweak = .7)
text(Treefit, use.n=TRUE, all=TRUE, cex=1)

#Prune the tree
library(caret)
Treefit$cptable[which.min(Treefit$cptable[,"xerror"]),"CP"]
plotcp(Treefit)
ptree <- prune(Treefit, cp = Treefit$cptable[which.min(Treefit$cptable[,"xerror"]),"CP"])
fancyRpartPlot(ptree, uniform=TRUE, main = "Pruned Classification Tree")
test_accuracy <- predict(ptree,test_digitrank,type="class")
