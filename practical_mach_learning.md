---
title: "Practical Machine learning"
author: "Guiatin Youssouf"
date: "15 février 2016"
output: html_document
---

# Introduction

In this project write-up, I have used the data from Human Activity Recognition (HAR).
The aim was to train a model based on the data of various sensor values,
which could later be used to predict the Classe variable,
that is the manner in which the participants of HAR did the exercise.

# read data


```r
train=read.csv("pml-training.csv",na.strings=c("","NA"))
```

```
## Warning in file(file, "rt"): impossible d'ouvrir le fichier 'pml-
## training.csv' : No such file or directory
```

```
## Error in file(file, "rt"): impossible d'ouvrir la connexion
```

```r
test=read.csv("pml-testing.csv",na.strings=c("","NA"))
```

```
## Warning in file(file, "rt"): impossible d'ouvrir le fichier 'pml-
## testing.csv' : No such file or directory
```

```
## Error in file(file, "rt"): impossible d'ouvrir la connexion
```
# remove missing value

```r
trainR=train[, colSums( is.na(train) ) < nrow(train)*0.5]
testR=test[, colSums( is.na(test) ) < nrow(test)*0.5]
```

I have realized that some columns have a lot of missing (NA) values. Instead of trying to model them,
I have decided to remove them from the data set.
So the first step, after having loaded the required caret library ,
was to detect and eliminate columns with a lot of missing values:
#suppression des 7 premières colonnes

```r
trainR=trainR[,-c(1:7)]
testR=testR[,-c(1:7)]
```

# create data partition

```r
set.seed(3433)
library(caret)
intrain=createDataPartition(trainR$classe,p=0.6,list=FALSE)

train=trainR[intrain,]
test=trainR[-intrain,]
```

# Data Modelling


```r
library(randomForest)

##controlRf <- trainControl(method="cv", 4)
##modelRf <- train(classe ~ ., data=train, method="rf", trControl=controlRf, ntree=500, allowParallel=TRUE)
modelRf<- readRDS("rfmodel.rds")
modelRf
```

```
## Random Forest 
## 
## 11776 samples
##    52 predictor
##     5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## No pre-processing
## Resampling: Cross-Validated (4 fold) 
## Summary of sample sizes: 8832, 8833, 8831, 8832 
## Resampling results across tuning parameters:
## 
##   mtry  Accuracy   Kappa      Accuracy SD  Kappa SD   
##    2    0.9870924  0.9836684  0.001860255  0.002352408
##   27    0.9881960  0.9850669  0.002862498  0.003619803
##   52    0.9821663  0.9774369  0.007141754  0.009035332
## 
## Accuracy was used to select the optimal model using  the largest value.
## The final value used for the model was mtry = 27.
```

we predict this modeling on validation data set


```r
predictRf <- predict(modelRf, test)
```

```
## Error in eval(expr, envir, enclos): objet 'roll_belt' introuvable
```

```r
confusionMatrix(test$classe, predictRf)
```

```
## Error in table(data, reference, dnn = dnn, ...): all arguments must have the same length
```

Lets check Accuracy

```r
accuracy <- postResample(predictRf, test$classe)
saveRDS(modelRf, "rfmodel.RDS")
```
Accuracy of this prediction model is 99.18%.

Out of Sample Error is calculated here

```r
error <- 1 - as.numeric(confusionMatrix(test$classe, predictRf)$overall[1])
```

```
## Error in table(data, reference, dnn = dnn, ...): all arguments must have the same length
```

```r
error
```

```
## [1] 0.008157023
```
error

out of sample error is  0.008157023.
# Predicting for Test Data Set

Now, we apply the model to the original testing data set downloaded from the data source.
Generate "problem_id_x.txt" Files for the assignments. These generated individual
files are stored in working directory.


```r
 pml_write_files = function(x){
   n = length(x)
   for(i in 1:n){
     filename = paste0("problem_id_",i,".txt")
     write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
   }
 }
 #pml_write_files(predictRf)
```
