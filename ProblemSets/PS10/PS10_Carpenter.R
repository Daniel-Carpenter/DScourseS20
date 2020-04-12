library(tidyverse)
library(rpart)
library(e1071)
library(kknn)
library(nnet)
library(mlr)
library(caret)
library(stargazer)


# READ IN DATA --------------------------------------------------------------------------------
income <- read.table("~/DScourseS20/ProblemSets/PS10/adult.data")
names(income) <- c("age","workclass","fnlwgt","education","education.num",
                   "marital.status","occupation","relationship","race",
                   "sex","capital.gain","capital.loss","hours",
                   "native.country","high.earner")

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
    theTask        <- makeClassifTask(id = "taskname", data = income.train, target = "high.earner")
  
# The 3-fold cross-validation strategy ---------------------------------------------------------
    resampleStrat  <- makeResampleDesc(method = "CV", iters = 3)
    
# The tuning strategy (e.g. random with 10 guesses) --------------------------------------------
    tuneMethod     <- makeTuneControlRandom(maxit = 10L)
    
# Create the six “learners” (algorithms): ------------------------------------------------------ 
    trees          <- makeLearner("classif.rpart", predict.type = "response")
    logit          <- makeLearner("classif.glmnet", predict.type = "response")
    neuralNet      <- makeLearner("classif.nnet", predict.type = "response")
    naiveBayes     <- makeLearner("classif.naiveBayes", predict.type = "response")
    kNN            <- makeLearner("classif.kknn", predict.type = "response")
    SVM            <- makeLearner("classif.svm", predict.type = "response")
    
# Create Parameters for the "Learner" Alorithms ------------------------------------------------
    ## Trees Parameters
      treesParams  <- makeParamSet(makeIntegerParam("minsplit", lower = 10, upper = 50),
                                  makeIntegerParam("minbucket",lower = 5, upper = 50), 
                                  makeNumericParam("cp", lower=.001, upper=.2))
    
    ## Logit Parameters 
      logitParams <- makeParamSet(makeNumericParam("lambda",lower = 0, upper = 3),
                                  makeNumericParam("alpha",lower = 0, upper = 1))
    
    ## Neural Network Parameters
      neuralNetParams  <- makeParamSet(makeIntegerParam("size",lower = 1, upper = 10),
                                  makeNumericParam("decay",lower=.1, upper=.5), 
                                  makeIntegerParam("maxit", lower = 1000, upper = 1000))
    
    ## kNN Parameters  
      kNNParams   <- makeParamSet(makeIntegerParam("k",lower = 1, upper = 30))
    
    ## SVM Parameters
      paramSet    <- list(2^-2, 2^-1, 1, 2, 4, 2^10)
      SVMParams   <- makeParamSet(makeDiscreteParam("kernel", values="radial" ),
                                  makeDiscreteParam("cost",values = paramSet), 
                                  makeDiscreteParam("gamma", values = paramSet))

# Tune the Models ------------------------------------------------------------------------------
    
    tuningMeasures <- list(f1, gmean) ### RMSE performance measure, this can be changed to one or many
      
    ## Trees Tuning
      tunedTrees   <- tuneParams(learner = trees,
                                task = theTask,
                                resampling = resampleStrat,
                                measures = tuningMeasures,
                                par.set = treesParams,
                                control = tuneMethod,
                                show.info = TRUE)
    
    ## Logit Tuning
      tunedLogit  <- tuneParams(learner = logit,
                                task = theTask,
                                resampling = resampleStrat,
                                measures = tuningMeasures,      
                                par.set = logitParams,
                                control = tuneMethod,
                                show.info = TRUE)
      
    ## Neural Network Tuning
      tunedNNet   <- tuneParams(learner = neuralNet,
                                task = theTask,
                                resampling = resampleStrat,
                                measures = tuningMeasures,      
                                par.set = neuralNetParams,
                                control = tuneMethod,
                                show.info = TRUE)
      
    ## kNN Tuning
      tunedkNN    <- tuneParams(learner = kNN,
                                task = theTask,
                                resampling = resampleStrat, 
                                measures = tuningMeasures,      
                                par.set = kNNParams,
                                control = tuneMethod,
                                show.info = TRUE)
      
    ## SVM Tuning
      tunedSVM    <- tuneParams(learner = SVM,
                                task = theTask,
                                resampling = resampleStrat,
                                measures = tuningMeasures,      
                                par.set = SVMParams,
                                control = tuneMethod,
                                show.info = TRUE)

# Print Tables to StarGazer Ouput -----------------------------------------------------------------
    stargazer(tunedTrees,
              tunedLogit,
              tunedNNet,
              tunedkNN,
              tunedSVM)

# Apply the Optimal Tuning Parameters to Each of the Algorithms ---------------------------------
    ## Trees
      trees        <- setHyperPars(learner = trees, 
                                   par.vals = tunedTrees$x)
      
    ## Logit
      logit        <- setHyperPars(learner = logit,
                                   par.vals = tunedLogit$x)
      
    ## Neural Network
      neuralNet    <- setHyperPars(learner = neuralNet, 
                                   par.vals = tunedNNet$x)
      
    ## kNN
      kNN          <- setHyperPars(learner = kNN, 
                                   par.vals = tunedkNN$x)
      
    ## SVM
      SVM          <- setHyperPars(learner = SVM, 
                                   par.vals = tunedSVM$x)

# Verify Performance using Cross Validated Sample Sets -------------------------------------------
    ## Trees
      resample(trees, 
               theTask, 
               resampleStrat, 
               measures = tuningMeasures)
      
    ## Logit
      resample(logit, 
               theTask, 
               resampleStrat, 
               measures = tuningMeasures)
      
    ## Neural Network
      resample(neuralNet, 
               theTask, 
               resampleStrat, 
               measures = tuningMeasures)
      
    ## kNN
      resample(kNN, 
               theTask, 
               resampleStrat, 
               measures = tuningMeasures)
      
    ## SVM
      resample(SVM, 
               theTask, 
               resampleStrat, 
               measures = tuningMeasures)
      
# Train the Final Model ----------------------------------------------------------------------------
    ## Trees
      finalTrees  <- train(learner = trees, 
                           task = theTask)
      
    ## Logit
      finalLogit  <- train(learner = logit, 
                           task = theTask)
      
    ## Neural Network
      finalNNet   <- train(learner = neuralNet, 
                           task = theTask)
      
    ## kNN
      finalkNN    <- train(learner = kNN, 
                           task = theTask)
      
    ## SVM      
      finalSVM    <- train(learner = SVM, 
                           task = theTask)
      
# Predict in Test Set ----------------------------------------------------------------------------
    ## Trees
      predictTrees  <- as.data.frame(predict(finalTrees, 
                                             newdata = income.test))
      
    ## Logit
      predictLogit  <- as.data.frame(predict(finalLogit, 
                                             newdata = income.test))
      
    ## Neural Network
      predictNNet   <- as.data.frame(predict(finalNNet, 
                                             newdata = income.test))
      
    ## kNN
      predictkNN    <- as.data.frame(predict(finalkNN, 
                                             newdata = income.test))
      
    ## SVM      
      predictSVM    <- as.data.frame(predict(finalSVM, 
                                             newdata = income.test))
      
# Assess Performance -----------------------------------------------------------------------------
    ## Trees
      confusionMatrix(predictTrees$response, 
                      predictTrees$truth)
      
    ## Logit
      confusionMatrix(predictLogit$response, 
                      predictLogit$truth)
      
    ## Neural Network
      confusionMatrix(predictNNet$response, 
                      predictNNet$truth)
      
    ## kNN
      confusionMatrix(predictkNN$response, 
                      predictkNN$truth)
      
    ## SVM      
      confusionMatrix(predictSVM$response, 
                      predictSVM$truth)
      