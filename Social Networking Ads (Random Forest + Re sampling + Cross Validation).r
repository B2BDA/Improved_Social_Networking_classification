
setwd("C:/Users/bishw/Dropbox/IVY DATA SCIENCE/R/Class 11")
df=read.csv('Social_Network_Ads.csv', na.strings = "")
head(df)
df$User.ID<-NULL

head(df)
summary(df)
str(df)
df$Purchased<-as.factor(df$Purchased)

library(ggplot2)
g<-ggplot(data = df)
g+geom_bar(aes(x=Gender))

g+geom_histogram(aes(x=Age), binwidth = 5, color='pink')+scale_x_continuous(limits = c(18,60), breaks = seq(18,60,5))

g+geom_histogram(aes(x=EstimatedSalary), binwidth = 10000, color='pink')+scale_x_continuous(limits = c(15000,150000), breaks = seq(15000,150000,10000))

g+geom_bar(aes(x=Purchased))

library(caTools)

set.seed(1429)
split<-sample.split(Y = df$Purchased, SplitRatio = 0.7)
train<-subset(x = df, subset = split==T)
test<-subset(x=df, subset = split==F)

head(train)
head(test)

library(randomForest)

rf<-randomForest(formula = Purchased ~ . , data = train, ntree=500, mtry = sqrt(ncol(df)-1), importance = T, proximity = T)

print(rf)

library(caret)
prd1<-predict(rf, test)
confusionMatrix(prd1, test$Purchased)

plot(rf)

t<-tuneRF(train[,-1], train[,1], ntreeTry = 500, stepFactor = 0.5, improve = 0.5, trace = T, plot = T)

rfi<-randomForest(formula = Purchased ~ . , data = train, ntree=500, mtry = 1, importance = T, proximity = T)

print(rf)

pred2<-predict(rf, test)
confusionMatrix(pred2, test$Purchased)

# Random Search
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
set.seed(1429)
mtry <- sqrt(ncol(train))
rf_random <- train(Purchased~., data=train, method="rf", metric="Accuracy", tuneLength=15, trControl=control)
print(rf_random)
plot(rf_random)

rfi<-randomForest(formula = Purchased ~ . , data = train, ntree=500, mtry = 2, importance = T, proximity = T)
print(rf)
pred2<-predict(rf, test)
confusionMatrix(pred2, test$Purchased, positive = '1')

hist(treesize(rf),
     main = "No. of Nodes for the Trees",
     col = "green")

varImpPlot(rf,
           sort = T)
           #n.var = 10,
           #main = "Top 10 - Variable Importance")
importance(rf)
varUsed(rf)

getTree(rf, 1, labelVar = TRUE)


library(party)

tr<-ctree(formula = Purchased ~ . , data = train)
plot(tr, type='simple')

library(ROSE)

summary(train$Purchased)

train.up<-ovun.sample(formula = Purchased ~ . , method = 'over', N = 180*2, seed = 1429, data = train)$data
train.down<-ovun.sample(formula = Purchased ~ . , method = 'under', N = 100*2, seed = 1429, data = train)$data
train.both<-ovun.sample(formula = Purchased ~ . , method = 'both', N = 280, seed = 1429, data = train)$data
train.smote<-ROSE(formula = Purchased ~ . , N = 300, seed = 1429, data = train)$data

rfu<-randomForest(formula = Purchased ~ . , data = train.up, ntree=500, mtry = 2, importance = T, proximity = T)
print(rfu)
predu<-predict(rfu, test)
confusionMatrix(predu, test$Purchased, positive = '1')


rfd<-randomForest(formula = Purchased ~ . , data = train.down, ntree=500, mtry = 2, importance = T, proximity = T)
print(rfd)
predd<-predict(rfd, test)
confusionMatrix(predd, test$Purchased, positive = '1')

rfb<-randomForest(formula = Purchased ~ . , data = train.both, ntree=500, mtry = 2, importance = T, proximity = T)
print(rfb)
predb<-predict(rfb, test)
confusionMatrix(predb, test$Purchased, positive = '1')

rfr<-randomForest(formula = Purchased ~ . , data = train.smote, ntree=500, mtry = 2, importance = T, proximity = T)
print(rfr)
predr<-predict(rfr, test)
confusionMatrix(predr, test$Purchased, positive = '1')




















