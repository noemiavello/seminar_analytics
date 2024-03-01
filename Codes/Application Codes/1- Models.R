
#Install and load packages

#install.packages("foreign")
#install.packages("readxl")
#install.packages("klaR")
#install.packages("caret")
#install.packages("stringr")
#install.packages("corrplot")
#install.packages("e1071")
#install.packages("caTools")
#install.packages("reliabilitydiag")
# install.packages("devtools")
#devtools::install_github("aijordan/reliabilitydiag")
#install.packages("triptych")
#install.packages("magrittr") 
#install.packages("dplyr") 
#requireNamespace("purrr")
#install.packages("here")
#install.packageslibrary("gridExtra")
#install.packages("doParallel")
#install.packages("broom")
#install.packages("kableExtra")
#install.packages("jtools") 
#install.packages("modelsummary")

library("readxl")
library(dplyr)
library(magrittr)
library(data.table)
library(caret)
library(klaR)
library(reliabilitydiag)
library(tidyr)
library(lubridate)
library(corrplot)
library("e1071")
library(rpart)
library(reliabilitydiag)
library(triptych)
library(MASS)
library(ggplot2)
library(tidyr)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(randomForest)
library(here)
library(gridExtra)
library(broom)
library(kableExtra)
library(jtools) 
library(modelsummary)

#Vehicle loan prediction data set 
vehic <- read.csv("mydata.csv")

#columns MOBILENO_AVL_FLAG and LTV_new has the same values for all observations. 
#So, we need to remove those columns

vehic2 = subset(vehic, select = -c(MOBILENO_AVL_FLAG,LTV_new) )

#Remove Redundant variables 
#Remove highly correlated variables (greater than 0.7). 
#The absolute values of pair-wise correlations are considered. 
#If two variables have a high correlation, look at the mean absolute correlation of each variable and removes 
#the variable with the largest mean absolute correlation. 

#Create the matrix
#full_numerical_cor2 = cor(vehic2, use = "complete.obs")

#findCorrelation(full_numerical_cor2, cutoff = .6, verbose = TRUE, names = TRUE)

# find attributes that are highly corrected
#highlyCorrelated <- findCorrelation(full_numerical_cor2, cutoff=0.7)

# print indexes of highly correlated attributes
#print(highlyCorrelated)

# Indentifying Variable Names of Highly Correlated Variables
#highlyCorCol <- colnames(vehic2)[highlyCorrelated]

# Print highly correlated attributes
#highlyCorCol

# Remove highly correlated variables and MOBILENO_AVL_FLAG, and create a new dataset
#vehic7 <- vehic6[, -which(colnames(vehic6) %in% highlyCorCol)]
#vehic7 = subset(vehic7, select = -c(MOBILENO_AVL_FLAG))
#dim(vehic7)


#################################################################################################################################    

#MODELS 
#set seed 
set.seed(123)   

# define an 50%/50% train/test split of the dataset
    train_index <- createDataPartition(vehic2$LOAN_DEFAULT, p=0.50, list=FALSE)
    data_train <- vehic2[ train_index,]
    data_test <- vehic2[-train_index,]
    
#What proportion of the training and test dataset is 1?    
    mean(data_train$LOAN_DEFAULT == 1)
    mean(data_test$LOAN_DEFAULT == 1)
    
    #data distribution test
    test_count <- data_test%>%
      dplyr::group_by(LOAN_DEFAULT)%>%
      dplyr::count(LOAN_DEFAULT) 
    
    #barplot data test data distribution
    test_plot <- ggplot(test_count, aes(x = LOAN_DEFAULT, y = n)) +
      geom_col(fill = "gray70") +
       coord_flip()  +
      theme_minimal()
    
    test_plot + ggtitle("Test data") +
      xlab("loan default") + ylab("frequency") +
      geom_text(aes(label = n), vjust = 1, hjust=1.3, colour = "white", size = 3, position = position_dodge(1.5))
    
    #data distribution train
    train_count <- data_train%>%
      dplyr::group_by(LOAN_DEFAULT)%>%
      dplyr::count(LOAN_DEFAULT) 
    
    #barplot data train data distribution
    train_plot <- ggplot(train_count, aes(x = LOAN_DEFAULT, y = n)) +
      geom_col(fill = "gray70") +
      coord_flip()  +
      theme_minimal()
    
    train_plot + ggtitle("Train data") +
      xlab("loan default") + ylab("frequency") +
      geom_text(aes(label = n), vjust = 1, hjust=1.3, colour = "white", size = 3, position = position_dodge(1.5))
    
## 3- Prediction Models
    ## (1) Logistic Regression
    glm_fit = glm(LOAN_DEFAULT ~ .,
                  family = binomial,
                  data = data_train)
    summary(glm_fit)
    summ(glm_fit, robust = "HC1")
    
    modelsummary(mod, shape = term ~ model + statistic)
    
  
    #Model print
    tidy_logistic <- tidy(glm_fit)
    
    table <- kable(tidy_logistic, "html", booktabs = TRUE) %>%
      kable_styling(font_size = 12) %>% 
      kable_styling(bootstrap_options = c("striped", "hover"))
    
    print(table)
  
    # predict the test dataset
    pred <- predict(glm_fit, data_test, type="response") 
    pred2 <- predict(glm_fit, data_test)
    
    logitmodel.pred <- rep("No", nrow(data_test))
    logitmodel.pred[pred > 0.5] = "Yes"
    
    data_test2 <- data_test
    data_test2$LOAN_DEFAULT <- ifelse(data_test2$LOAN_DEFAULT==1, "Yes", "No") 
    
    matrix.logit <- as.data.frame.matrix(table(logitmodel.pred, data_test2$LOAN_DEFAULT))
    
    #Print confusion matrix
    table2 <- kable(matrix.logit, "html", booktabs = TRUE) %>%
      kable_styling(font_size = 12) %>% 
      kable_styling(bootstrap_options = c("striped", "hover"))
    
    print(table2)
  
    
    #accuracy
    mean.logit <- round(mean(logitmodel.pred == data_test2$LOAN_DEFAULT), digits = 4)
    
    no <- matrix.logit[1, 1]
    yes <- matrix.logit[2, 2]
    #Using the logistic model, we can correctly classify 91113 not default and 96 approving defaults
    #This yields a share of 78% correctly classified observation.
  
    #probabilities prediction in a data frame
    glm_probs = data.frame(probs = predict(glm_fit, newdata=data_test, type="response"))
    head(glm_probs)
    
    #Metrics
    library(MLmetrics)
    library(dplyr)
 
    #threshold 0.5
    Precision(data_test$LOAN_DEFAULT, ifelse(glm_probs >= .5, 1, 0), positive = 1)
    
    # create confusion matrix 
    confusionMatrix(factor(ifelse(glm_probs >= .5, 1, 0)), factor(data_test$LOAN_DEFAULT), mode = "everything",
                    positive="1")
    
    
    #defining an optimal threshold
    #f1_scores <- sapply(seq(0.01, 0.99, .01), function(thresh) F1_Score(data_train$LOAN_DEFAULT, ifelse(glm_probs >= thresh, 1, 0), positive = 1))
    #which.max(f1_scores) # 2
    
    #Precision(data_test$LOAN_DEFAULT, ifelse(glm_probs >= .2, 1, 0), positive = 1)
    #Recall(data_test$LOAN_DEFAULT, ifelse(glm_probs >= .2, 1, 0), positive = 1)
    #Accuracy(ifelse(glm_probs >= .4, 1, 0), data_test$LOAN_DEFAULT)
    
    #F1 = 2 * (Precision(data_test$LOAN_DEFAULT, ifelse(glm_probs >= .4, 1, 0), positive = 1) * Recall(data_test$LOAN_DEFAULT, ifelse(glm_probs >= .4, 1, 0), positive = 1)) / (Precision(data_test$LOAN_DEFAULT, ifelse(glm_probs >= .4, 1, 0), positive = 1) + Recall(data_test$LOAN_DEFAULT, ifelse(glm_probs >= .4, 1, 0), positive = 1))
    #F1
    
    ## (2) Naive Bayes 
    naive_fit <- naiveBayes(LOAN_DEFAULT ~ ., data = data_train) 
    naive_fit
    
    #Model print
    summary_table <- as.data.frame(naive_fit$tables)
    
    table3 <- kable(summary_table, "html") %>%
      kable_styling(font_size = 12) %>% 
      kable_styling(bootstrap_options = c("striped", "hover"))
    
    print(table3)
    
    #probabilities prediction
    naive_probs <- predict(naive_fit, newdata = data_test)
    naive_probs2 <- predict(naive_fit, newdata = data_test,type =  "raw")
    
    #confusion matrix
    matrix.naives <- table(naive_probs, data_test$LOAN_DEFAULT)
    matrix.naives2 <- as.data.frame.matrix(table(naive_probs, data_test$LOAN_DEFAULT))
    
    #change names
    colnames(matrix.naives2) = c("No", "Yes")
    rownames(matrix.naives2) = c("No", "Yes")
    
    #Print confusion matrix
    table4 <- kable(matrix.naives2, "html", booktabs = TRUE) %>%
      kable_styling(font_size = 12) %>% 
      kable_styling(bootstrap_options = c("striped", "hover"))
    
    print(table4)
    
    #filtering data
    nonb <- matrix.naives2[1, 1]
    yesnb <- matrix.naives2[2, 2]
    
    mean.naives <- round(mean(naive_probs == data_test$LOAN_DEFAULT), digits = 4)
   
    #Following the same procedure, it can be estimated the confusion matrix. 
    #Using the naive bayes model, it can correctly classify 18877 not defaulting and 11640 approving individuals. 
    
    #Naive Bayes performs with accurate predictions of 43.63%%.
    
    #Metrics
    confusionMatrix(factor(ifelse(naive_probs2[,2] >= .5, 1, 0)), factor(data_test$LOAN_DEFAULT), mode = "everything",
                    positive="1")
    
    #Precision(data_test$LOAN_DEFAULT, ifelse(naive_probs2[,2] >= .5, 1, 0), positive = 1)
    
    #f1_scores <- sapply(seq(0.01, 0.99, .01), function(thresh) F1_Score(data_train$LOAN_DEFAULT, ifelse(naive_probs2[,2] >= thresh, 1, 0), positive = 1))
    #which.max(f1_scores) # 1
    
    #Precision(data_test$LOAN_DEFAULT, ifelse(naive_probs2[,2] >= .1, 1, 0), positive = 1)
    #Recall(data_test$LOAN_DEFAULT, ifelse(naive_probs2[,2] >= .1, 1, 0), positive = 1)
    #Accuracy(ifelse(naive_probs2[,2] >= .1, 1, 0), data_test$LOAN_DEFAULT)
    
    #F1 = 2 * (Precision(data_test$LOAN_DEFAULT, ifelse(naive_probs2[,2] >= .1, 1, 0), positive = 1) * Recall(data_test$LOAN_DEFAULT, ifelse(naive_probs2[,2] >= .1, 1, 0), positive = 1)) / (Precision(data_test$LOAN_DEFAULT, ifelse(naive_probs2[,2] >= .1, 1, 0), positive = 1) + Recall(data_test$LOAN_DEFAULT, ifelse(naive_probs2[,2] >= .1, 1, 0), positive = 1))
    #F1
    
    # (3) Random Forest
    forest_fit = randomForest(as.factor(LOAN_DEFAULT) ~ ., 
                                             data=data_train,
                              ntree=100,
                              importance=TRUE)
    print(summary(forest_fit))
    
    # Assuming rf_model is your Random Forest model
    importance(forest_fit)

    # Assuming rf_model is your Random Forest model
    forest_fit$err.rate[nrow(forest_fit$err.rate), "OOB"]
    
    # Assuming rf_model is your Random Forest model
    print(forest_fit)

    
    #predictions
    forest_probs = predict(object = forest_fit,   
                            newdata = data_test,  
                            type = "class") 
    
    # Calculate the confusion matrix for the test set
    matrix.random <- table(data_test$LOAN_DEFAULT, forest_probs)
    matrix.random2 <- as.data.frame.matrix(table(data_test$LOAN_DEFAULT, forest_probs))
    
    #change names
    colnames(matrix.random2) = c("No", "Yes")
    rownames(matrix.random2) = c("No", "Yes")
    
    #Print confusion matrix
    table5 <- kable(matrix.random2, "html", booktabs = TRUE) %>%
      kable_styling(font_size = 12) %>% 
      kable_styling(bootstrap_options = c("striped", "hover"))
    
    print(table5)
    
    #accuracy
    mean.random = mean((data_test$LOAN_DEFAULT == forest_probs)) 
   
    #predicted probabilities 
    forest_probs2 = predict(object = forest_fit,
                                  newdata = data_test,
                                  type = "prob")
    #Metrics
    confusionMatrix(factor(forest_probs), factor(data_test$LOAN_DEFAULT), mode = "everything",
                    positive="1")
    
    #Precision(data_test$LOAN_DEFAULT, forest_probs, positive = 1)
    #Recall(data_test$LOAN_DEFAULT, forest_probs, positive = 1)
    #Accuracy(forest_probs, data_test$LOAN_DEFAULT)
    
    #F1 = 2 * (Precision(data_test$LOAN_DEFAULT, forest_probs, positive = 1) * Recall(data_test$LOAN_DEFAULT, forest_probs, positive = 1)) / (Precision(data_test$LOAN_DEFAULT, forest_probs, positive = 1) + Recall(data_test$LOAN_DEFAULT, forest_probs, positive = 1))
    #F1
   
####################################################################################################################################
    
# 4- Calibration 
    #generate a data frame with the probabilities predictions for each model 
    vehic_models <- data.frame(cbind(data_test$LOAN_DEFAULT, glm_probs$probs, naive_probs2[,2], forest_probs2[,2]))
    colnames(vehic_models)[colnames(vehic_models) == 'X1'] <- 'obs'
    colnames(vehic_models)[colnames(vehic_models) == 'X2'] <- 'Logistic'
    colnames(vehic_models)[colnames(vehic_models) == 'X3'] <- 'NB'
    colnames(vehic_models)[colnames(vehic_models) == 'X4'] <- 'RF'
    
    #exporting to use in plots
    saveRDS(vehic_models, "vehic_models.rds")
    

    

    
    
    
    
    
    
    
    
    
    