library(mlr)
library(tidyverse)
library(magrittr)
library(glmnet)
library(stargazer)

# READ IN HOUSING DATA ---------------------------------------------------------------------------------
  
housing <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data")
  names(housing) <- c("crim","zn","indus","chas","nox","rm","age","dis","rad","tax","ptratio","b","lstat","medv")

  
# ADD FEATURES TO DATASET  -----------------------------------------------------------------------------
  
  housing$lmedv <- log (housing$medv)
  housing$medv  <- NULL # drop median value
  formula <- as.formula (lmedv ~ .^3 +
                             poly (crim, 6) +
                             poly (zn, 6) +
                             poly (indus, 6) +
                             poly (nox, 6) +
                             poly (rm, 6) +
                             poly (age, 6) +
                             poly (dis, 6) +
                             poly (rad, 6) +
                             poly (tax, 6) +
                             poly (ptratio, 6) +
                             poly (b, 6) +
                             poly (lstat, 6))
  mod_matrix <- data.frame(model.matrix(formula,housing))

  
# Replace Intercept Column by Response -----------------------------------------------------------------
    
  mod_matrix[ , 1] = housing$lmedv
    colnames(mod_matrix)[1] = "lmedv"
    head(mod_matrix)
  
    # Break up the Data
      n     <- nrow(mod_matrix)
      train <- sample(n,size=.8*n)
      test  <- setdiff(1:n,train)
      housing.train <- mod_matrix[train, ]
      housing.test  <- mod_matrix[test, ]
      
      # Dimensions of housing.train
        print(cat(NROW(housing.train), " x ", NCOL(housing.train)))
    

# LASSO -----------------------------------------------------------------------------------------------
  
  # Define the task:
    theTask <- makeRegrTask(id = "taskname", data = housing.train, target = "lmedv")
    print(theTask)
        
  # Tell it a new prediction algorithm
    predAlg <- makeLearner("regr.glmnet")
  
  # Search over penalty parameter lambda and force elastic net parameter to be 1 (LASSO)
    modelParams <- makeParamSet(makeNumericParam("lambda",lower=0,upper=1),makeNumericParam("alpha",lower=1,upper=1))
  
  # Take 50 random guesses at lambda within the interval I specified above
    tuneMethod <- makeTuneControlRandom(maxit = 50L)
  
  # Do the tuning
    tunedModel <- tuneParams(learner = predAlg,
                           task = theTask,
                           resampling = resampleStrat,
                           measures = rmse,       # RMSE performance measure, this can be changed to one or many
                           par.set = modelParams,
                           control = tuneMethod,
                           show.info = TRUE)
  
  # CROSS VALIDATION -----------------------------------------------------------------------------------
   
     # Apply the optimal algorithm parameters to the model
      predAlg <- setHyperPars(learner=predAlg, par.vals = tunedModel$x)
    
    # Verify performance on cross validated sample sets
      resample(predAlg,theTask,resampleStrat,measures=list(rmse))
    
    # Train the final model
      finalModel <- train(learner = predAlg, task = theTask)
    
    # Predict in test set!
      prediction <- predict(finalModel, newdata = housing.test)
    
      print(head(prediction$data))
      print(get.rmse(prediction$data$truth,prediction$data$response))
    
    # Trained parameter estimates
      # getLearnerModel(finalModel)$beta

      
# RIDGE REGRESSION -------------------------------------------------------------------------------------
      
      # Search over penalty parameter lambda and force elastic net parameter to be 0 (ridge)
        modelParams <- makeParamSet(makeNumericParam("lambda",lower=0,upper=1),makeNumericParam("alpha",lower=0,upper=0))
      
      # Change to 6-Fold CV
        resampleStrat <- makeResampleDesc(method = "CV", iters = 6)
    
        tunedModel <- tuneParams(learner = predAlg,
                                 task = theTask,
                                 resampling = resampleStrat,
                                 measures = rmse,       # RMSE performance measure, this can be changed to one or many
                                 par.set = modelParams,
                                 control = tuneMethod,
                                 show.info = TRUE)
      
      # Apply the optimal algorithm parameters to the model
        predAlg <- setHyperPars(learner=predAlg, par.vals = tunedModel$x)
      
      # Verify performance on cross validated sample sets
        resample(predAlg,theTask,resampleStrat,measures=list(rmse))
      
      # Train the final model
        finalModel <- train(learner = predAlg, task = theTask)
      
      # Predict in test set!
        prediction <- predict(finalModel, newdata = housing.test)
      
      # Print RMSE
        print(get.rmse(prediction$data$truth,prediction$data$response))
      
      # Trained parameter estimates
        # getLearnerModel(finalModel)$beta

# NET ELASTICS MODEL ---------------------------------------------------------------------------------------------
        # Change alpha/upper to 1 for Net Elastic 
        modelParams <- makeParamSet(makeNumericParam("lambda",lower=0,upper=1),makeNumericParam("alpha",lower=0,upper=1)) 
        
        # Change to 6-Fold CV
        resampleStrat <- makeResampleDesc(method = "CV", iters = 6)
        
        tunedModel <- tuneParams(learner = predAlg,
                                 task = theTask,
                                 resampling = resampleStrat,
                                 measures = rmse,       # RMSE performance measure, this can be changed to one or many
                                 par.set = modelParams,
                                 control = tuneMethod,
                                 show.info = TRUE)
        
        # Apply the optimal algorithm parameters to the model
          predAlg <- setHyperPars(learner=predAlg, par.vals = tunedModel$x)
        
        # Verify performance on cross validated sample sets
          resample(predAlg,theTask,resampleStrat,measures=list(rmse))
        
        # Train the final model
          finalModel <- train(learner = predAlg, task = theTask)
        
        # Predict in test set!
          prediction <- predict(finalModel, newdata = housing.test)
        
        # Print RMSE
          print(get.rmse(prediction$data$truth,prediction$data$response))
        
        # Trained parameter estimates
          # getLearnerModel(finalModel)$beta
          
# SUMMARY TABLE -------------------------------------------------------------------------------------------------
          
          summaryTable <- matrix(c(0.00605,   0.095,     0.0139,
                                   0.1548824, 0.1499291, 0.1654741,
                                   0.1545515, 0.1520700, 0.1705171), ncol = 3, byrow = TRUE)
          colnames(summaryTable) <- c("LASSO", "Ridge Reg.", "Net Elastic")
          rownames(summaryTable) <- c("Lambda", "In-Sample RMSE", "Out-of-Sample RMSE")
        
          stargazer(summaryTable)
          
          