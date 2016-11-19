#setwd("/home/abhijit331/Dropbox/math282B-Winter16")
setwd("C:/Users/abhijit331/Dropbox/math282B-Winter16")
library(lattice)
library(caret)
library(quantreg)
load("04cars.rda")
tmp = dat[,c("All-Wheel_Drive","Rear-Wheel_Drive","Horsepower","Highway_MPG")] # extract selected variables
tmp = tmp[complete.cases(tmp),] # extracts complete cases
tmp = as.data.frame(tmp)
names(tmp) = c("awd","rwd","hp","mpg") # abbreviate names
tmp = tmp[complete.cases(tmp),]

#Problem 1
tmp = tmp[sample(nrow(tmp)),]
k = c(5,10,413)
error = c()
for(j in k)
{
mean.error = crossval(j,tmp)
mean.error.ls = mean.error[1]
mean.error.lad = mean.error[2]
error = rbind(error,c(mean.error.ls,mean.error.lad))
}
colnames(error) = c("L2","L1")
rownames(error) = c("5","10","413")
crossval = function(k,tmp)
{
  error.ls = 0
  error.lad = 0
  fold = cut(seq(1,nrow(tmp)),breaks = k,labels = F)  # get labels for data
  for(i in 1:k)
  {
    test_index = which(fold == i,arr.ind = T) # get indices ofall datapoints with label i
    test.data = tmp[test_index,]              # test data contains all datapoints with label i
    train.data = tmp[-test_index,]            # training data contains all datapoints not in the test set
    fit.ls = lm(mpg ~ poly(hp,2,raw = TRUE), data = train.data)
    fit.lad = rq(mpg ~poly(hp,2,raw = T), data = train.data)
    predict.ls = predict(fit.ls,data.frame(hp = test.data$hp))
    predict.lad = predict(fit.lad,data.frame(hp = test.data$hp))
    error.ls = error.ls + sum((predict.ls-test.data$mpg)^2)/length(test.data$mpg)
    error.lad = error.lad + sum((predict.lad - test.data$mpg)^2)/length(test.data$mpg)
  }
  return(c(error.ls,error.lad)/k)
}
error
# Based on the error table ,we can see that the L2 model has lower mean errors for k = 5,10,413.
# Therefore, it is appropriate to choose the L2 model. 

#2nd Problem 
library(MASS)
backwardSelect = function(parameter)
{
  temp = get_all_vars(parameter)
  X = temp[,-1] 
  y = temp[,1]
  for(j in 1:13)
  {
    f.val = c()  
    for(i in 1:dim(X)[2])
    {
      temp = X[,-i]
      f.val[i] = anova(lm(y ~.,data = temp),lm(y ~ .,data=X))$F[2]
    }
    if(min(f.val) > 4) # if the minimum F value is greater than 4, return the remaining model  
    {
      return(head(X))
    }
    else  # find the lowest F statistic, and remove the variable associated with that F statistic
    {
      minimum = min(f.val)
      min.index = which(f.val == minimum)
      X = X[,-min.index]
    }
  }
}

X = scale(Boston,scale = F)
attach(as.data.frame(X))
backwardSelect(medv ~ crim + zn +indus + chas +nox+rm+age+dis+rad+tax+ptratio+black+lstat)

# The formula returns the variables remaining in the model after backward selection. In the Boston dataset, 11 variables
# remain in the final model, and two are removed(indus,age)



#Problem 3
# Worked along with Gokul Ramanathan and Prateek Kulkarni

# return a function that returns the prediction function
fit = function(xtrain, ytrain)
{  
pred = function(lm1, newdata)
{
return(predict(lm1, newdata))
}
return(pred)
}

subSampleSelect = function(X, y, fit, p, B){
  e = c()
  for(i in 1:B){
    selectRow = sample(1:nrow(X), ceiling(p*nrow(X))) 
    traindf = X[selectRow,]
    ytrain = y[selectRow]
    testdf = X[-selectRow,]
    ytest = y[-selectRow]
    pred1 = fit(traindf, ytrain)
    lm1 = lm(ytrain~., data.frame(ytrain = ytrain, traindf))
    e = c(e, sum((pred1(lm1, testdf) - ytest)^2)/length(ytest))
  }
  return(mean(e))
}
subSampleSelect(Boston[,1:13], y=Boston[,14], fit = fit, p=0.7, B=20)





