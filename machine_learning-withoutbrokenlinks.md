Introduction
------------

#### Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: <http://groupware.les.inf.puc-rio.br/har> (see the section on the Weight Lifting Exercise Dataset).

#### Data comes from <http://groupware.les.inf.puc-rio.br/har>

Source of Data
--------------

#### The training data for this project are available here:

#### <https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv>

#### The test data are available here:

#### <https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv>

#### The data for this project come from this source: <http://groupware.les.inf.puc-rio.br/har>. If you use the document you create for this class for any purpose please cite them as they have been very generous in allowing their data to be used for this kind of assignment.

    ##Load relevant Libraries
    library(caret)
    library(rpart)
    library(rpart.plot)
    library(RColorBrewer)
    library(rattle)
    library(randomForest)
    library(knitr)
    opts_chunk$set(echo = TRUE)

### Downloading Data set

    set.seed(12345)
    trainingset <-"http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
    testingset<-"http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
    train <- read.csv(url(trainingset),na.strings=c("NA","#DIV/0!",""))
    testing <- read.csv(url(testingset),na.strings=c("NA","#DIV/0!",""))

### Susbet training and test sets and assessing structure

    Train <- createDataPartition(train$classe, p=0.6, list=FALSE)
    myTraining <- train[Train, ]
    myTesting <- train[-Train, ]
    dim(myTraining); dim(myTesting)

    ## [1] 11776   160

    ## [1] 7846  160

### Cleaning up structure of traing set removing any non zero variance variables that are useless predictors.

    n <- nearZeroVar(myTraining, saveMetrics=TRUE)
    myTraining <- myTraining[,n$nzv==FALSE]

    n<- nearZeroVar(myTesting,saveMetrics=TRUE)
    myTesting <- myTesting[,n$nzv==FALSE]

    myTraining <- myTraining[c(-1)]

##### Eliminate training set data that has data that has more than 60% NA in it.

    trainingV3 <- myTraining
    for(i in 1:length(myTraining)) {
        if( sum( is.na( myTraining[, i] ) ) /nrow(myTraining) >= .7) {
            for(j in 1:length(trainingV3)) {
                if( length( grep(names(myTraining[i]), names(trainingV3)[j]) ) == 1)  {
                    trainingV3 <- trainingV3[ , -j]
                }   
            } 
        }
    }

    myTraining <- trainingV3
    rm(trainingV3)

    clean1 <- colnames(myTraining) ##Make sure predictors in training and testing are the same 
    clean2 <- colnames(myTraining[,-58])###Isolate classe column  
    myTesting <- myTesting[clean1]###Enusre only columns in data are the same as the training 
    testing <- testing[clean2]             
    dim(myTesting)

    ## [1] 7846   58

    dim(testing)

    ## [1] 20 57

#### Make sure class in testing set is the same as that in thre training set

    for (i in 1:length(testing) ) {
        for(j in 1:length(myTraining)) {
            if( length( grep(names(myTraining[i]), names(testing)[j]) ) == 1)  {
                class(testing[j]) <- class(myTraining[i])
            }      
        }      
    }
    testing <- rbind(myTraining[2, -58] , testing)
    testing <- testing[-1,]

### Prediction RUles using Decision Trees

    set.seed(12345)
    modA <- rpart(classe ~ ., data=myTraining, method="class")
    table(myTraining$classe)

    ## 
    ##    A    B    C    D    E 
    ## 3348 2279 2054 1930 2165

    fancyRpartPlot(modA)

![](https://github.com/ChristopherPapanicolas87/Practical-Machine-Learning-/blob/master/unnamed-chunk-7-1.png)
\#\#\#Confusion matrix of Random Forest analysis

    A1 <- predict(modA, myTesting, type = "class")
    tree <- confusionMatrix(A1, myTesting$classe)
    tree

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    A    B    C    D    E
    ##          A 2150   60    7    1    0
    ##          B   61 1260   69   64    0
    ##          C   21  188 1269  143    4
    ##          D    0   10   14  857   78
    ##          E    0    0    9  221 1360
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.8789          
    ##                  95% CI : (0.8715, 0.8861)
    ##     No Information Rate : 0.2845          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.8468          
    ##  Mcnemar's Test P-Value : NA              
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: A Class: B Class: C Class: D Class: E
    ## Sensitivity            0.9633   0.8300   0.9276   0.6664   0.9431
    ## Specificity            0.9879   0.9693   0.9450   0.9845   0.9641
    ## Pos Pred Value         0.9693   0.8666   0.7809   0.8936   0.8553
    ## Neg Pred Value         0.9854   0.9596   0.9841   0.9377   0.9869
    ## Prevalence             0.2845   0.1935   0.1744   0.1639   0.1838
    ## Detection Rate         0.2740   0.1606   0.1617   0.1092   0.1733
    ## Detection Prevalence   0.2827   0.1853   0.2071   0.1222   0.2027
    ## Balanced Accuracy      0.9756   0.8997   0.9363   0.8254   0.9536

### Plot table of random forest predictions using the classe as a reference.

    plot(tree$table, col = tree$byClass, main = paste("Analysis by Decision Tree: Accuracy =", round(tree$overall['Accuracy'], 4)))

![](machine_learning-md_files/figure-markdown_strict/unnamed-chunk-9-1.png)
\#\#\#Prediction of Data using Random Forest

    set.seed(12345)
    modFitB1 <- randomForest(classe ~ ., data=myTraining)
    predB1 <- predict(modFitB1, myTesting, type = "class")
    cmf <- confusionMatrix(predB1, myTesting$classe)
    cmf

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    A    B    C    D    E
    ##          A 2231    2    0    0    0
    ##          B    1 1516    0    0    0
    ##          C    0    0 1367    3    0
    ##          D    0    0    1 1282    1
    ##          E    0    0    0    1 1441
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.9989          
    ##                  95% CI : (0.9978, 0.9995)
    ##     No Information Rate : 0.2845          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.9985          
    ##  Mcnemar's Test P-Value : NA              
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: A Class: B Class: C Class: D Class: E
    ## Sensitivity            0.9996   0.9987   0.9993   0.9969   0.9993
    ## Specificity            0.9996   0.9998   0.9995   0.9997   0.9998
    ## Pos Pred Value         0.9991   0.9993   0.9978   0.9984   0.9993
    ## Neg Pred Value         0.9998   0.9997   0.9998   0.9994   0.9998
    ## Prevalence             0.2845   0.1935   0.1744   0.1639   0.1838
    ## Detection Rate         0.2843   0.1932   0.1742   0.1634   0.1837
    ## Detection Prevalence   0.2846   0.1933   0.1746   0.1637   0.1838
    ## Balanced Accuracy      0.9996   0.9993   0.9994   0.9983   0.9996

### Plot of of Random FOrest analysis of class vs other predictors

    plot(modFitB1)

![](machine_learning-md_files/figure-markdown_strict/unnamed-chunk-11-1.png)
\#\#\#Plot of Confusion matrix using random forest

    plot(cmf$table, col = cmf$byClass, main = paste("Random Forest Confusion Matrix: Accuracy =", round(cmf$overall['Accuracy'], 4)))

![](machine_learning-md_files/figure-markdown_strict/unnamed-chunk-12-1.png)
\#\#\#The results show that the random forest prediction method is more
accurate than the the decision tree method and so the predicition
analysis for the test samples will be assessed using the Random forest
method.

### Prediction Using Boosting Method

    set.seed(12345)
    fitControl <- trainControl(method = "repeatedcv",
                               number = 5,
                               repeats = 1)

    gbmFit1 <- train(classe ~ ., data=myTraining, method = "gbm",
                     trControl = fitControl,
                     verbose = FALSE)


    gbmFinMod1 <- gbmFit1$finalModel

    gbmPredTest <- predict(gbmFit1, newdata=myTesting)
    gbmAccuracyTest <- confusionMatrix(gbmPredTest, myTesting$classe)
    gbmAccuracyTest

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    A    B    C    D    E
    ##          A 2231    4    0    0    0
    ##          B    1 1512    1    0    0
    ##          C    0    2 1361    3    0
    ##          D    0    0    6 1274    1
    ##          E    0    0    0    9 1441
    ## 
    ## Overall Statistics
    ##                                          
    ##                Accuracy : 0.9966         
    ##                  95% CI : (0.995, 0.9977)
    ##     No Information Rate : 0.2845         
    ##     P-Value [Acc > NIR] : < 2.2e-16      
    ##                                          
    ##                   Kappa : 0.9956         
    ##  Mcnemar's Test P-Value : NA             
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: A Class: B Class: C Class: D Class: E
    ## Sensitivity            0.9996   0.9960   0.9949   0.9907   0.9993
    ## Specificity            0.9993   0.9997   0.9992   0.9989   0.9986
    ## Pos Pred Value         0.9982   0.9987   0.9963   0.9945   0.9938
    ## Neg Pred Value         0.9998   0.9991   0.9989   0.9982   0.9998
    ## Prevalence             0.2845   0.1935   0.1744   0.1639   0.1838
    ## Detection Rate         0.2843   0.1927   0.1735   0.1624   0.1837
    ## Detection Prevalence   0.2849   0.1930   0.1741   0.1633   0.1848
    ## Balanced Accuracy      0.9994   0.9979   0.9971   0.9948   0.9990

### Plot comparing accuracy of boosting method vs. the boosting iterations. The greater iterations had more accuracy.

    plot(gbmFit1, ylim=c(0.9, 1))

![](machine_learning-md_files/figure-markdown_strict/unnamed-chunk-14-1.png)

### Since Accuracy of test samples is the highest with the Random Forest method, this will be used to predict the classe output of the test sample.

    B2 <- predict(modFitB1, testing, type = "class")
    B2

    ##  1  2 31  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
    ##  B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
    ## Levels: A B C D E

#### For loop is generated to print the classe output intop txt files.

    pml_write_files = function(x){
        n = length(x)
        for(i in 1:n){
            filename = paste0("problem_id_",i,".txt")
            write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
        }
    }

    pml_write_files(B2)

Conclusion
----------

#### As shown by the analysis, the confusion matrix has shown that the random forest provides the best prediction model to assess the classe data from the data provided.
