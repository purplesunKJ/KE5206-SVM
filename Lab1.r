library(e1071)

data("iris")
iris
table.iris = table(iris$Species)
pie(table.iris)
hist(iris$Sepal.Length)
boxplot(Petal.Width~Species, data = iris, col = c('red','green3', 'blue')[iris$Species])
plot(x=iris$Petal.Length,y=iris$Petal.Width,col=c('red','green3','blue')[iris$Species])
plot(x=iris$Petal.Length,y=iris$Petal.Width, pch=21,bg=c('red','green3','blue')[iris$Species])
plot(x=iris$Sepal.Length,y=iris$Sepal.Width,pch=21,bg=c('red','green3','blue')[iris$Species])
pairs(iris[1:4], main="Edgar Anderson's Iris Data", pch=21,bg=c('red','green3','blue')[iris$Species])

#SVM
iris.subset = subset(iris, select = c('Sepal.Length', 'Sepal.Width','Species'),Species%in%c('setosa','virginica'))
plot(x=iris.subset$Sepal.Length,y=iris.subset$Sepal.Width,col=iris.subset$Species,pch=19)
svm.iris.model=svm(Species~.,data=iris.subset,kernel='linear',cost=1,scale=FALSE)
svm.iris.model$index

points(iris.subset[svm.iris.model$index, c(1,2)],col='black',cex=2)
svm.iris.model$SV
svm.iris.model$coefs

w = t(svm.iris.model$coefs)%*%svm.iris.model$SV
svm.iris.model$rho

w0 = -svm.iris.model$rho
w1 = w[1,1]
w2 = w[1,2]
abline(a=-w0/w2, b=-w1/w2, col='red', lty=5)

svm.iris.model2=svm(Species~.,data=iris.subset,kernel='linear',cost=10000,scale=FALSE)
svm.iris.model2$index

w = t(svm.iris.model2$coefs)%*%svm.iris.model2$SV

model.iris<-svm(Species~.,iris,kernel='linear')
model.iris$SV
model.iris$coefs

class.iris<-predict(model.iris,iris)
table(predict=class.iris,truth=iris$Species)