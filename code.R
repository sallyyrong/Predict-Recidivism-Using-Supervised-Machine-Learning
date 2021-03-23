#title: "Challenge 1"
#author: "Manny Duran, Sally Rong, Jenny Wu. Xixi Zheng"
#date: "2/10/2021"

knitr::opts_chunk$set(echo = TRUE)
library(glmnet)
library(gbm)
library(dplyr)
library(ggplot2)

# Data Set-up ---------------------------------------------------------------------------------
dat = read.csv("recidivism_data_sample.csv")
pseudo.dat = read.csv("recidivism_data_pseudo_new.csv")
dat = dat[,-1] #Taking out the ID variable because it isn't pertinent in our data. 
dat$race = as.factor(dat$race)
set.seed(54321)
n.total <- nrow(dat)
prop.train <- 0.8
k <- sample(1:n.total, size = round(n.total*prop.train), replace = FALSE)
train.dat = dat[k,]
test.dat = dat[-k,]


y <- dat$recidivate
x <- model.matrix(recidivate ~ ., data = dat)[,-1]

thresholds <- seq(from = 0.3, to = 0.7, by = 0.05)
set <- c("accuracy", "precision", "recall", "MSE", "FPR", "FNR", "Diff")

# Logistic Regression ---------------------------------------------------------------------------------
#modeling with a logistic regression, using family = "binomial" because our outcomes are binary 
log1 = glm(recidivate ~., family = binomial(link = "logit"), data = train.dat) 
# use logistic regression to predict on test data
mod2.pred = predict(log1, newdata = test.dat, type = "response") 

# Metrics 
log.table <- data.frame(matrix(, nrow=9, ncol=7))
colnames(log.table) <- set
rownames(log.table) = thresholds
for(i in 1:length(thresholds)){
  mod2.class = as.numeric(mod2.pred >= thresholds[i]) # classifying predictions as recidivating or not
  TP<- sum(test.dat$recidivate == 1 & mod2.class == 1) # calculate true positives
  FP<- sum(test.dat$recidivate == 0 & mod2.class == 1) # calculate false positives
  TN<- sum(test.dat$recidivate == 0 & mod2.class == 0) # calculate true negatives
  FN<- sum(test.dat$recidivate == 1 & mod2.class == 0) # calculate false negatives
  accuracy <- (TP + TN)/ (TN + FN +TP +FP)
  precision <- (TP)/ (TP + FP)
  recall <- (TP)/ (TP + FN)
  MSE = mean((test.dat$recidivate - mod2.pred)^2)
  FPR <- FP / (FP +TN) # false positive rate
  FNR <- FN / (FN+TP) # false negative rate -- want a model with a low false negative rate
  diff = abs(FPR - FNR)
  log.table[i,] = cbind(accuracy, precision, recall, MSE, FPR, FNR, diff)
}

min.threshold = rownames(log.table)[which.min(log.table[,7])] #saving the threshold that gives us threshold that has the lowest absolute difference between FPR and FNR for comparison in the end
min.threshold
log.table


# Logistic Calibration Plot ----------------------------------------------------------------------------------
# Creating the logistic calibration plot

data = cbind(test.dat, mod2.pred, mod2.class)
data$TP = as.numeric(test.dat$recidivate == 1 & mod2.class == 1) # generate column for true positives 
risk_deciles = quantile(data$mod2.pred, probs = seq(from = 0, to = 1, by = 0.1)) # generate risk deciles, producing sample quantiles corresponding to probabilities from 0 to 1 in increments of 0.1

data$risk_decile_bin = as.numeric(cut(data$mod2.pred, breaks = risk_deciles, include.lowest = TRUE)) # create bins for risk deciles

some_results = data %>% group_by(risk_decile_bin) %>% summarise(pred.prob = mean(mod2.pred), pos = mean(recidivate)) #creating a new data table with the mean predicted probabilities and mean fraction of true positives separated into the 10 decile bins created in the previous line


ggplot(some_results, aes(x = pred.prob, y = pos)) + geom_point() + geom_line() +
  ggtitle("Logistic Calibration Plot") + ylim(0,1) + ylab("Fraction of True Positives") + 
  xlab("Mean Predicted Probabilities") + geom_abline() 

# Ridge Regression ----------------------------------------------------------------------------------
lambdas <- 10^seq(from = 6, to = -2, length = 100) # creating various lambda values
ridge.train <- glmnet(x = x[k,], y = y[k], alpha = 0, family = "binomial", lambda = lambdas) # creating ridge regression with family = "binomial" because our outcomes are binary 
test.mse.ridge <- rep(NA,length(lambdas)) # empty object to use with function

for (i in 1:length(lambdas)){
  ridge.pred.test <- predict(ridge.train, newx = x[-k,], s = lambdas[i], type= "response") # use ridge regression model to predict onto test data iteratively with 1:i(length((lamdas)))
  test.mse.ridge[i] <- mean((y[-k] - ridge.pred.test)^2) # calculating mse for each iteration and storing it
}

min = lambdas[which.min(test.mse.ridge)] # locating the minimum test mse's and its optimal lamda value

ridge = glmnet(x = x[k,], y = y[k], alpha = 0, family = "binomial", lambda = min)# ridge regression model with optimal lambda value
ridge.pred = predict(ridge, newx = x[-k,], type = "response", s = min) # use ridge regression to predict onto test data
ridge.pred = as.vector(ridge.pred)

# Metrics
ridge.table <- data.frame(matrix(, nrow=9, ncol=7))
colnames(ridge.table) <- set
rownames(ridge.table) = thresholds
for(i in 1:length(thresholds)){
  ridge.class = as.numeric(ridge.pred >= thresholds[i]) # classifying recidivate predictions iteratively for each threshold value
  TP = sum((y[-k]) == 1 & ridge.class == 1) # calculate true positives
  TN = sum((y[-k]) == 0 & ridge.class == 0) # calculate false positives
  FP = sum((y[-k]) == 0 & ridge.class == 1) # calculate true negatives
  FN = sum((y[-k]) == 1 & ridge.class == 0) # calculate false negatives
  r.accuracy <- (TP + TN)/ (TN + FN +TP +FP)
  r.precision <- (TP)/ (TP + FP)
  r.recall <- (TP)/(TP + FN)
  r.FPR <- FP/(FP+ TN) # false positive rate
  r.FNR <- FN/(FN + TP) # false negative rate -- want a model with a low FPR
  r.mse = mean((y[-k] - ridge.pred)^2)
  r.diff = abs(r.FPR - r.FNR) # absolute value of difference between FPR and FNR 
  ridge.table[i,] = cbind(r.accuracy, r.precision, r.recall, r.mse, r.FPR, r.FNR, r.diff)
}
r.min.threshold = rownames(ridge.table)[which.min(ridge.table[,7])] # choosing the lowest diff value's threshold as our classification threshold
r.min.threshold
ridge.table


# Ridge Regression Calibration Plot ----------------------------------------------------------------------------------
# Creating the ridge regression calibration plot

# storing predicted values along with test values
ridge.pred = predict(ridge, newx = x[-k,], type = "response", s = min)
ridge.pred = as.vector(ridge.pred)
ridge.graph = cbind(test.dat, ridge.pred)


risk_deciles_ridge = quantile(ridge.graph$ridge.pred, probs = seq(from = 0, to = 1, by = 0.1)) # generate risk deciles, producing sample quantiles corresponding to probabilities from 0 to 1 in increments of 0.1


ridge.graph$risk_decile_bin = as.numeric(cut(ridge.graph$ridge.pred, breaks = risk_deciles_ridge, include.lowest = TRUE)) # create bins for risk deciles

some_results_ridge = ridge.graph %>% group_by(risk_decile_bin) %>% summarise(pred.prob = mean(ridge.pred), pos = mean(recidivate)) #creating a new data table with the mean predicted probabilities and mean fraction of true positives separated into the 10 decile bins created in the previous line

ggplot(some_results_ridge, aes(x = pred.prob, y = pos)) + geom_point() + geom_line() +
  ggtitle("Ridge Regression Calibration Plot") + ylab("Fraction of True Positives") +
  xlab("Mean Predicted Probabilities")+ ylim(0,1) + geom_abline()

# Boosted Trees ----------------------------------------------------------------------------------
#create vector of different interaction depth to train model
depths <- c(1,2,3,4,5,6,7) 
#empty list that will hold the models trained with different depths
bt.models <- list() 
set.seed(2019207)
#Loop through each with cross validation for each interaction depth for a training classification model of boosted trees with shrinking parameter of 0.01, 1000 trees to fit and with 10 cross validation folds. 
for (i in 1:length(depths)){
  
  bt.models[[i]] <- gbm(recidivate ~  ., 
                        data = train.dat, shrinkage = 0.01,
                        distribution="bernoulli", n.trees = 1000, 
                        interaction.depth = depths[i], 
                        cv.folds = 10)
  print(i)
}


#Implementing CV over two parameters: depths and number of trees
n.depths <- length(bt.models) 
depths <- rep(NA, n.depths)
min.cv.error <- rep(NA, n.depths)
best.n.trees <- rep(NA, n.depths)

#Considering all of the models, all values of interaction depth and all number of trees,
#we are looking to find the best model.

for (i in 1:n.depths){
  
  bt.curr <- bt.models[[i]]
  depths[i] <- bt.curr$interaction.depth
  min.cv.error[i] <- min(bt.curr$cv.error)
  best.n.trees[i] <- which.min(bt.curr$cv.error)
  rm(bt.curr)
}

#This gives us the best number of variables to predict upon.
m <- which.min(min.cv.error)
m
#This gives us the best number of trees to create.
final.ntrees <- best.n.trees[m]
final.ntrees
#This gives us the best number of nodes for a tree to have.
final.depth <- depths[m]
final.depth

#finding the predicted probabilities using the best number of m and trees for the test data 
test.dat.preds <- predict(bt.models[[m]], newdata = test.dat, 
                          n.trees = final.ntrees, type = "response")



# Boosted Trees Plot ----------------------------------------------------------------------------------
bt.one <- bt.models[[3]]
bt.one$n.trees
bt.one$interaction.depth
bt.one$cv.error[1:20]
plot(x = 1:bt.one$n.trees, y = bt.one$cv.error) #Plotting to get the gist of the number of trees that minimizes CV error

# Metrics
boost.table <- data.frame(matrix(, nrow=9, ncol=7))
colnames(boost.table) <- set
rownames(boost.table) = thresholds
boosteddat = cbind(test.dat, test.dat.preds)
for(i in 1:length(thresholds)){
  boosteddat$yhat = as.numeric(boosteddat$test.dat.preds >= thresholds[i])
  TP = sum(boosteddat$recidivate == 1 & boosteddat$yhat == 1)
  TN = sum(boosteddat$recidivate == 0 & boosteddat$yhat == 0)
  FP = sum(boosteddat$recidivate == 0 & boosteddat$yhat == 1)
  FN = sum(boosteddat$recidivate == 1 & boosteddat$yhat == 0)
  b.accuracy = (TP+TN)/nrow(boosteddat)
  b.precision = TP/(TP + FP)
  b.recall = TP/(TP+FN)
  b.FPR = FP/(FP+TN)
  b.FNR = FN/(FN+TP)
  b.mse = mean((test.dat.preds - test.dat$recidivate)^2)
  b.diff = abs(b.FPR -b.FNR)
  boost.table[i,]<-cbind(b.accuracy, b.precision, b.recall, b.mse, b.FPR, b.FNR, b.diff)
}
b.min.threshold = rownames(boost.table)[which.min(boost.table[,7])]
b.min.threshold
boost.table



# Boosted Trees Calibration Plot ----------------------------------------------------------------------------------
boosted.graph = boosteddat

risk_deciles_boosted = quantile(boosted.graph$test.dat.preds, probs = seq(from = 0, to = 1, by = 0.1))# generate risk deciles, producing sample quantiles corresponding to probabilities from 0 to 1 in increments of 0.1

boosted.graph$risk_decile_bin = as.numeric(cut(boosted.graph$test.dat.preds, breaks = risk_deciles_boosted, include.lowest = TRUE)) # create bins for risk deciles

some_results_boosted = boosted.graph %>% group_by(risk_decile_bin) %>% summarise(pred.prob = mean(test.dat.preds), pos = mean(recidivate)) #creating a new data table with the mean predicted probabilities and mean fraction of true positives separated into the 10 decile bins created in the previous line


ggplot(some_results_boosted, aes(x = pred.prob, y = pos)) + geom_point() + geom_line() +
  ggtitle("Boosted Trees Calibration Plot") + ylab("Fraction of True Positives") +
  xlab("Mean Predicted Probabilities") + ylim(0,1) + geom_abline()


# Tables to Compare Models ----------------------------------------------------------------------------------
min.threshold
log.table
r.min.threshold
ridge.table
b.min.threshold
boost.table
mods = rbind(log.table[min.threshold,], ridge.table[r.min.threshold,], boost.table[b.min.threshold,])

# Build function for our model to use on pseudo code ----------------------------------------------------------------------------------
test.dat.preds.fin <- predict(bt.models[[m]], newdata = pseudo.dat, 
                              n.trees = final.ntrees, type = "response")

test.dat.preds.fin[1:100]

