library(ISLR)
library(e1071)
library(ROSE)

# import Default dataset and save it in a dataframe 
df = Default

#Understanding the data
names(df)
dim(df)
summary(df)
str(df)
table(df$default)
table.df = table(df$default)
prop.table(table(df$default))

# Visualise the data
pie(table.df)
hist(df$balance)
hist(df$income)
boxplot(df$balance~default, data = df, col = c('green3','red')[df$default])
plot(x=df$income,y=df$balance, pch=21,bg=c('green3','red')[df$default])
pairs(df[2:4], main="Default Analysis", pch=21,bg=c('green','red')[df$default])


########################
####Train Test Split####
########################
# Resampling the data to have a balanced "yes" and "no" default value
df_balanced <- ovun.sample(default ~ ., data = df, method = "both", p=0.5, N=10000, seed = 1)$data

# Visualise the resample data
table(df_balanced$default)
prop.table(table(df_balanced$default))

## set the seed to make your partition reproductible
set.seed(123)

## 80% of the sample size as training set
smp_size <- floor(0.80 * nrow(df_balanced))
train_ind <- sample(seq_len(nrow(df_balanced)), size = smp_size)

train <- df_balanced[train_ind, ]
test <- df_balanced[-train_ind, ]

###########################
####SVM-linear Analysis####
###########################
# SVM model tuning for linear function
tune.out_linear = tune(svm, default~., data=train, kernel="linear",ranges=list(cost=c(0.1,1,10,100),gamma=c(0.5,1,2,3)))
summary(tune.out_linear)
bestmod_linear = tune.out_linear$best.model
summary(bestmod_linear)

# inspect the confusion matix table
newpred_linear=predict(tune.out_linear$best.model,test)
table(prediction=newpred_linear, actual=test$default) 

###########################
####SVM-radial Analysis####
###########################
# SVM model tuning for radial function
tune.out_radial = tune(svm, default~., data=train, kernel="radial",ranges=list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4)))
summary(tune.out_radial)
bestmod_radial = tune.out_radial$best.model
summary(bestmod_radial)

# inspect the confusion matix table
newpred_radial=predict(tune.out_radial$best.model,test)
table(prediction=newpred_radial, actual=test$default) 

#############################################
####SVM-radial Analysis(balance & income)####
#############################################
# SVM model tuning for radial model (balance & income)
tune.out_2.1 = tune(svm, default~balance + income, data=train, kernel="radial",ranges=list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4)))
summary(tune.out_2.1)
bestmod_2.1=tune.out_2.1$best.model
summary(bestmod_2.1)

# inspect the confusion matix table
newpred_2.1=predict(tune.out_2.1$best.model,test)
table(prediction=newpred_2.1, actual=test$default) 

#############################################
####SVM-radial Analysis(income & student)####
#############################################
# SVM model tuning for radial model (income & student)
tune.out_2.2 = tune(svm, default~income + student, data=train, kernel="radial",ranges=list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4)))
summary(tune.out_2.2)
bestmod_2.2=tune.out_2.2$best.model
summary(bestmod_2.2)

# inspect the confusion matix table
newpred_2.2=predict(tune.out_2.2$best.model,test)
table(prediction=newpred_2.2, actual=test$default) 

##############################################
####SVM-radial Analysis(balance & student)####
##############################################
# SVM model tuning for radial model (balance & student)
tune.out_2.3 = tune(svm,default~balance + student,data=train, kernel="radial",ranges=list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4)))
summary(tune.out_2.3)
bestmod_2.3=tune.out_2.3$best.model
summary(bestmod_2.3)

# inspect the confusion matix table
newpred_2.3=predict(tune.out_2.3$best.model,test)
table(prediction=newpred_2.3, actual=test$default) 

# svmfit=svm(default~.,data=train, kernel="linear",gamma=1,cost=1e5)
