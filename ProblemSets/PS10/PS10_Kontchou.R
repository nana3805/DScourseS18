#install.packages("rpart")
#install.packages("e1071")
#install.packages("kknn")
#install.packages("nnet")
#install.packages("mlr")
library(rpart)
library(e1071)
library(kknn)
library(nnet)
library(mlr)

set.seed(100)

income <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data")
names(income) <- c("age","workclass","fnlwgt","education","education.num","marital.status","occupation","relationship","race","sex","capital.gain","capital.loss","hours","native.country","high.earner")

# From UC Irvine's website (http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.names)
#   age: continuous.
#   workclass: Private, Self-emp-not-inc, Self-emp-inc, Federal-gov, Local-gov, State-gov, Without-pay, Never-worked.
#   fnlwgt: continuous.
#   education: Bachelors, Some-college, 11th, HS-grad, Prof-school, Assoc-acdm, Assoc-voc, 9th, 7th-8th, 12th, Masters, 1st-4th, 10th, Doctorate, 5th-6th, Preschool.
#   education-num: continuous.
#   marital-status: Married-civ-spouse, Divorced, Never-married, Separated, Widowed, Married-spouse-absent, Married-AF-spouse.
#   occupation: Tech-support, Craft-repair, Other-service, Sales, Exec-managerial, Prof-specialty, Handlers-cleaners, Machine-op-inspct, Adm-clerical, Farming-fishing, Transport-moving, Priv-house-serv, Protective-serv, Armed-Forces.
#   relationship: Wife, Own-child, Husband, Not-in-family, Other-relative, Unmarried.
#   race: White, Asian-Pac-Islander, Amer-Indian-Eskimo, Other, Black.
#   sex: Female, Male.
#   capital-gain: continuous.
#   capital-loss: continuous.
#   hours-per-week: continuous.
#   native-country: United-States, Cambodia, England, Puerto-Rico, Canada, Germany, Outlying-US(Guam-USVI-etc), India, Japan, Greece, South, China, Cuba, Iran, Honduras, Philippines, Italy, Poland, Jamaica, Vietnam, Mexico, Portugal, Ireland, France, Dominican-Republic, Laos, Ecuador, Taiwan, Haiti, Columbia, Hungary, Guatemala, Nicaragua, Scotland, Thailand, Yugoslavia, El-Salvador, Trinadad&Tobago, Peru, Hong, Holand-Netherlands.

######################
# Clean up the data
######################
# Drop unnecessary columns
income$native.country <- NULL
income$fnlwgt         <- NULL
# Make sure continuous variables are coded as such
income$age            <- as.numeric(income$age)
income$hours          <- as.numeric(income$hours)
income$education.num  <- as.numeric(income$education.num)
income$capital.gain   <- as.numeric(income$capital.gain)
income$capital.loss   <- as.numeric(income$capital.loss)
# Combine levels of categorical variables that currently have too many levels
levels(income$education) <- list(Advanced = c("Masters,","Doctorate,","Prof-school,"), Bachelors = c("Bachelors,"), "Some-college" = c("Some-college,","Assoc-acdm,","Assoc-voc,"), "HS-grad" = c("HS-grad,","12th,"), "HS-drop" = c("11th,","9th,","7th-8th,","1st-4th,","10th,","5th-6th,","Preschool,"))
levels(income$marital.status) <- list(Married = c("Married-civ-spouse,","Married-spouse-absent,","Married-AF-spouse,"), Divorced = c("Divorced,","Separated,"), Widowed = c("Widowed,"), "Never-married" = c("Never-married,"))
levels(income$race) <- list(White = c("White,"), Black = c("Black,"), Asian = c("Asian-Pac-Islander,"), Other = c("Other,","Amer-Indian-Eskimo,"))
levels(income$workclass) <- list(Private = c("Private,"), "Self-emp" = c("Self-emp-not-inc,","Self-emp-inc,"), Gov = c("Federal-gov,","Local-gov,","State-gov,"), Other = c("Without-pay,","Never-worked,","?,"))
levels(income$occupation) <- list("Blue-collar" = c("?,","Craft-repair,","Farming-fishing,","Handlers-cleaners,","Machine-op-inspct,","Transport-moving,"), "White-collar" = c("Adm-clerical,","Exec-managerial,","Prof-specialty,","Sales,","Tech-support,"), Services = c("Armed-Forces,","Other-service,","Priv-house-serv,","Protective-serv,"))

# Break up the data:
n <- nrow(income)
train <- sample(n, size = .8*n)
test  <- setdiff(1:n, train)
income.train <- income[train,]
income.test  <- income[test, ]

#Define the task
# Define the task:
task.highearner <- makeClassifTask(data = income.train, target = "high.earner")
print(task.highearner)

#Set resampling strategy (here let's do 3-fold CV)
CrossValidation <- makeResampleDesc(method = "CV", iters = 3)

#Take 10 random guesses at lambda
tuneMethod <- makeTuneControlRandom(maxit = 10L) 

#Tell mlr what prediction algorithm we'll be using 
#For each algorithm, add predict.type = “response” as an additional argument to the makeLearner() function.
predict.trees <- makeLearner("classif.rpart", predict.type = "response")
predict.logit <- makeLearner("classif.glmnet", predict.type = "response")
predict.nn <- makeLearner("classif.nnet", predict.type = "response")
predict.nb <- makeLearner("classif.naiveBayes", predict.type = "response")
predict.knn <-makeLearner("classif.kknn", predict.type = "response")
predict.svm <- makeLearner("classif.svm", predict.type = "response")

#Set hyperparameters for Tree Model
getParamSet("classif.rpart")

TreesParams <- makeParamSet(
  makeIntegerParam("minsplit",lower = 10, upper = 50),
  makeIntegerParam("minbucket", lower = 5, upper = 50),
  makeNumericParam("cp", lower = 0.001, upper = 0.2)
)

#Set hyperparameters for Logistic Model
getParamSet("classif.glmnet")

LogitParams <- makeParamSet(
  makeNumericParam("lambda",lower=0,upper=3),
  makeNumericParam("alpha",lower=0,upper=1))
  
#Set hyperparameters for Neural Network Model
getParamSet("classif.nnet")

nnParams <- makeParamSet(
  makeIntegerParam("size", lower = 1, upper = 10),
  makeNumericParam("decay", lower = 0.1, upper = 0.5),
  makeIntegerParam("maxit", lower = 1000, upper = 1000))

#Do not have to set parameters for Naive Bayes

#Set hyperparameters for KnN
getParamSet("classif.knn")

knnParams <- makeParamSet(
  makeIntegerParam("k", lower = 1, upper = 30))

#Set hyperparameters for SVM
getParamSet("classif.svm")

svmParams  <- makeParamSet(
  makeDiscreteParam("cost", values = 2^c(-2, -1, 0, 1, 2, 10)), 
  makeDiscreteParam("gamma", values = 2^c(-2, -1, 0, 1, 2, 10)))

#Tuning Models
#tuning trees
tuned.trees <- tuneParams(learner = predict.trees,
                               task = task.highearner,
                               resampling = resampleStrat,
                               measures = list(f1, gmean),
                               par.set = TreesParams,
                               control = tuneMethod,
                               show.info = TRUE)
#tuning logit
tuned.logit <- tuneParams(learner = predict.logit,
                               task = task.highearner,
                               resampling = resampleStrat,
                               measures = list(f1, gmean),
                               par.set = LogitParams,
                               control = tuneMethod,
                               show.info = TRUE)
#tuning neural network
tuned.nn <- tuneParams(learner = predict.nn,
                            task = task.highearner,
                            resampling = resampleStrat,
                            measures = list(f1, gmean),      
                            par.set = nnParams,
                            control = tuneMethod,
                            show.info = TRUE)

#tuning knn
tuned.knn <- tuneParams(learner = predict.knn,
                             task = task.highearner,
                             resampling = resampleStrat,
                             measures = list(f1, gmean),      
                             par.set = knnParams,
                             control = tuneMethod,
                             show.info = TRUE)
#tuning svm
tuned.svm <- tuneParams(learner = predict.svm,
                             task = task.highearner,
                             resampling = resampleStrat,
                             measures = list(f1, gmean),      
                             par.set = svmParams,
                             control = tuneMethod,
                             show.info = TRUE)

#Training the models
#optimal algorithm parameters
predict_trees <- setHyperPars(learner=predict.trees, par.vals = tuned.trees$x)
predict_logit <- setHyperPars(learner=predict.logit, par.vals = tuned.logit$x)
predict_nn    <- setHyperPars(learner=predict.nn, par.vals = tuned.nn$x)
predict_knn   <- setHyperPars(learner=predict.knn, par.vals = tuned.knn$x)
#predict_svm   <- setHyperPars(learner=predict.svm, par.vals = tuned.svm$x)

#cross validation of sample results
sampleResults.tree  <- resample(learner = predict_trees, task = task.highearner, resampling = resampleStrat, measures=list(gmean))
sampleResults.logit <- resample(learner = predict_logit, task = task.highearner, resampling = resampleStrat, measures=list(gmean))
sampleResults.nn    <- resample(learner = predict_nn, task = task.highearner, resampling = resampleStrat, measures=list(gmean))
sampleResults.knn   <- resample(learner = predict_knn, task = task.highearner, resampling = resampleStrat, measures=list(gmean))
#sampleResults.svm   <- resample(learner = predict_svm, task = task.highearner, resampling = resampleStrat, measures=list(gmean))

#run model on training data 
TreeModel  <- train(learner = predict_trees, task = task.highearner)
LogitModel <- train(learner = predict_logit, task = task.highearner)
NNModel   <- train(learner = predict_nn, task = task.highearner)
KNNModel   <- train(learner = predict_knn, task = task.highearner)
NBModel   <- train(learner = predict.nb, task = task.highearner)
#SVMModel  <- train(learner = predict_svm, task = task.highearner)

# Predict in test set for each algorithm
predict_tree_test  <- predict(TreeModel, newdata = income.test)
predict_logit_test <- predict(LogitModel, newdata = income.test)
predict_neuralnet_test <- predict(NNModel, newdata = income.test)
predict_knn_test  <- predict(KNNModel, newdata = income.test)
predict_nb_test   <- predict(NBModel, newdata = income.train)
#predict_svm_test  <- predict(SVMModel, newdata = income.test)

# Out of sample f1 and gmean for each algorithm
performance(predict_tree_test, measures = list(f1, gmean))
performance(predict_logit_test, measures = list(f1, gmean))
performance(predict_neuralnet_test, measures = list(f1, gmean))
performance(predict_knn_test, measures = list(f1, gmean))
performance(predict_nb_test, measures = list(f1, gmean))
#performance(predict_svm_test, measures = list(f1, gmean))



