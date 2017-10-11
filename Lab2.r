#SVM for binary classification
library(e1071)

set.seed(1)
x=matrix(rnorm(200*2),ncol=2)
x[1:100,]=x[1:100,]+2
x[101:150,]=x[101:150,]-2
y=c(rep(1,150),rep(2,50))
dat=data.frame(x=x,y=as.factor(y))

plot(x, col=y+1)

train=sample(200,100)
svmfit=svm(y~.,data=dat[train,],kernel="radial",gamma=1,cost=1)
plot(svmfit,dat[train,],svSymbol=17, dataSymbol=1)

summary(svmfit)

svmfit=svm(y~.,data=dat[train,],kernel="radial",gamma=1,cost=1e5)
plot(svmfit,dat[train,])

set.seed(1)
tune.out=tune(svm, y~., data=dat[train,], kernel="radial",
              ranges=list(cost=c(0.1,1,10,100,1000),
                          gamma=c(0.5,1,2,3,4)))

summary(tune.out)

bestmod=tune.out$best.model
summary(bestmod)

newpred=predict(tune.out$best.model,data=dat[-train,])
table(prediction=newpred,actual=dat[-train,"y"])

#SVM for Multiple Classes
set.seed(1)
x=rbind(x,matrix(rnorm(50*2),ncol=2))
y=c(y,rep(0,50))
x[y==0,2]=x[y==0,2]+2
dat.new=data.frame(x=x,y=as.factor(y))
plot(x,col=(y+1))

svmfit3=svm(y~., data=dat.new, kernel="radial", cost=10, gamma=1, probability=TRUE)
plot(svmfit3,dat.new)

data(iris)
svm3.iris = svm(iris$Species ~., iris, kernel="radial", cost=100, gamma=1, probability=TRUE)
svm3.iris

class.iris = predict(svm3.iris, iris, probability = TRUE)
table(predict=class.iris, truth=iris$Species)

com.iris = data.frame(attr(class.iris, "probabilities"), class.iris, iris$Species)
com.iris