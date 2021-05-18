library(ISLR)
attach(Hitters)
names(Hitters)
fix(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))
Hitters=na.omit(Hitters)
sum(is.na(Hitters$Salary))
x=model.matrix(Salary~.,Hitters)[,-1]
y=Hitters$Salary
library(glmnet)
grid=10^seq(10,-2,length=100)
lasso.mod=glmnet(x,y,aplha=1,lambda = grid)
dim(coef(lasso.mod))

#predict coefficients using lambda value of 50
predict(lasso.mod,s=50,type="coefficients")[1:20,]

#training set created in order to test differenct in test error for the ridge regression 
set.seed(1)
train=sample(1:nrow(x),nrow(x)/2)
test=(-train)
y.test=y[test]

#cross-validation used to choose best paramater of lambda
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],aplha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam

#predict
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)
out=glmnet(x,y,aplha=1)
predict(out,type="coefficients", s=bestlam)[1:20,]
mean((lasso.pred-y.test)^2)
