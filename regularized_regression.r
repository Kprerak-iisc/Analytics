library(MASS)
?Boston
summary(Boston)
model<-lm(medv~.,data=Boston)
summary(lm(medv~.,data=Boston))
model1<-lm(medv~crim+zn+chas+nox+rm+dis+rad+tax+ptratio+black+lstat,data=Boston)
summary(lm(medv~crim+zn+chas+nox+rm+dis+rad+tax+ptratio+black+lstat,data=Boston))
library(car)
vif(model)

#Lasso approach
library(lars)
lars(as.matrix(Boston[,-14]),y=Boston[,14])
plot(lars(as.matrix(Boston[,-14]),y=Boston[,14]))
summary(lars(as.matrix(Boston[,-14]),y=Boston[,14]))
library(glmnet)
x=as.matrix(Boston[,-14])
y=Boston[,14]
plot(glmnet(x,y,alpha=0)$beta,main="L2 regularization ")
