library(tidyverse)
library(rpart)
library(e1071)
library(kknn)
library(nnet)
library(mlr)


# READ IN DATA --------------------------------------------------------------------------------
income <- read.table("~/DScourseS20/ProblemSets/PS10/adult.data")
names(income) <- c("age","workclass","fnlwgt","education","education.num","marital.status","occupation","relationship","race","sex","capital.gain","capital.loss","hours","native.country","high.earner")

# Clean up the data ---------------------------------------------------------------------------
  ## Drop unnecessary columns
    income$native.country <- NULL
    income$fnlwgt         <- NULL
  
  ## Make sure continuous variables are coded as such
    income$age            <- as.numeric(income$age)
    income$hours          <- as.numeric(income$hours)
    income$education.num  <- as.numeric(income$education.num)
    income$capital.gain   <- as.numeric(income$capital.gain)
    income$capital.loss   <- as.numeric(income$capital.loss)
  
  ## Combine levels of categorical variables that currently have too many levels
    levels(income$education) <- list(Advanced = c("Masters,","Doctorate,","Prof-school,"), Bachelors = c("Bachelors,"), "Some-college" = c("Some-college,","Assoc-acdm,","Assoc-voc,"), "HS-grad" = c("HS-grad,","12th,"), "HS-drop" = c("11th,","9th,","7th-8th,","1st-4th,","10th,","5th-6th,","Preschool,"))
    levels(income$marital.status) <- list(Married = c("Married-civ-spouse,","Married-spouse-absent,","Married-AF-spouse,"), Divorced = c("Divorced,","Separated,"), Widowed = c("Widowed,"), "Never-married" = c("Never-married,"))
    levels(income$race) <- list(White = c("White,"), Black = c("Black,"), Asian = c("Asian-Pac-Islander,"), Other = c("Other,","Amer-Indian-Eskimo,"))
    levels(income$workclass) <- list(Private = c("Private,"), "Self-emp" = c("Self-emp-not-inc,","Self-emp-inc,"), Gov = c("Federal-gov,","Local-gov,","State-gov,"), Other = c("Without-pay,","Never-worked,","?,"))
    levels(income$occupation) <- list("Blue-collar" = c("?,","Craft-repair,","Farming-fishing,","Handlers-cleaners,","Machine-op-inspct,","Transport-moving,"), "White-collar" = c("Adm-clerical,","Exec-managerial,","Prof-specialty,","Sales,","Tech-support,"), Services = c("Armed-Forces,","Other-service,","Priv-house-serv,","Protective-serv,"))
  
  ## Break up the data:
    n <- nrow(income)
    train <- sample(n, size = .8*n)
    test  <- setdiff(1:n, train)
    income.train <- income[train,]
    income.test  <- income[test, ]
    
# The classification task ----------------------------------------------------------------------
    
# The 3-fold cross-validation strategy ---------------------------------------------------------
    
# The tuning strategy (e.g. random with 10 guesses) --------------------------------------------
    
# Each of the six “learners” (algorithms): -----------------------------------------------------
  ## Trees: classif.rpart
    
  ## Logistic regression: classif.glmnet
    
  ## Neural network: classif.nnet
    
  ## Naive Bayes: classif.naiveBayes
    
  ## kNN: classif.kknn
    
  ## SVM: classif.svm
    
    
