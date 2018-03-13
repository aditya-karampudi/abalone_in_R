


rm(list = ls())
library(glmnet)
library(DMwR)
library(FNN)
library(e1071)
library(corrplot)
library(vegan)
library(Metrics)
library(MASS)
library(rpart)
library(caret)
library(pls)
library(Metrics)
library(car)
avaal<- read.csv("C:\\Users\\mohad\\Desktop\\INSOFE\\R work\\Abalone.csv")

avaal <- read.csv("Abalone.csv")
names(avaal)
str(avaal)



#lets add all weight columns and make it into one
#xx <- avaal$Shuckedweight+avaal$Visceraweight+avaal$Shellweight
#head(xx)

#avaal <- subset(avaal, select = -c(Shuckedweight, Visceraweight, Shellweight))
#avaal <- cbind(avaal, weight=xx)


#avaal <- subset(avaal, select = -Wholeweight)
avaal <- subset(avaal, select = -Wholeweight)



#lets dummify
library(dummies)

dumm1 <- dummy(avaal$Sex)
avaal <- subset(avaal, select = -Sex)
avaal <- cbind(avaal, dumm1 )



summary(avaal)

stan <- subset(avaal, select = -c(SexF, SexI, SexM))


pcastan <- prcomp(train_x, scale. = T)
summary(pcastan)

pcastan$x

newdata <- data.frame(age=train_y$age, pcastan$x)

newdata <- newdata[,1:5]
library(e1071)
svmmodel <- svm(age~., data = newdata, kernel="radial", gamma=.01, cost=3)
summary(svmmodel)

newtest <- predict(pcastan, newdata = test_x,scale.=T)
newtest <- as.data.frame(newtest)

newtest <- newtest[,1:5]

plotvar <- pcastan$sdev/sum(pcastan$sdev)


plot(plotvar)

testsvm <- predict(svmmodel, newdata = newtest)

regr.eval(testsvm,test_y)



xxx <- subset(avaal,select = c(age, SexF, SexI, SexM))


#standaradize
stan <- decostand(stan, method = "standardize")

#lets add the columns
avaall <- cbind(stan,xxx)

#lets remove the first column as it is not necessary
avaall <- subset(avaall, select=-X)

#check the height
plot(avaall$Height, avaall$age)
avaall[avaall$Height==0,]


##
avaall$weight.diff = abalone$weight.w - 
(abalone$weight.v + abalone$weight.s + abalone$weight.sh)

avaall$weight_diff <- avaall$
  avaal

#for original data
n=nrow(stan)
set.seed(1000)
split1 <- sample(1:n, 0.7*n, replace = F)
train1 <- stan[split1,]
test1 <- stan[-split1,]

train_y <- subset(train1, select=age)
train_x <- subset(train1, select=-age)



test_y <- subset(test1, select=age)

test_x<- subset(test1, select=-age)
#n=nrow(avaall)
#set.seed(100)
#split <- sample(1:n, 0.7*n, replace = F)
#train <- avaall[split,]
#test <- avaall[-split,]


lmorig <- lm(age~., data =train1 )
summary(lmorig)
plot(lmorig)

predlmo <- predict(lmorig, newdata = test1)
rmselmo <- rmse(predlmo, test1$age)
vif(lmorig)
mapelmo <- mean(abs(test1$age-predlmo)/test1$age)*100
errorlmo <- regr.eval(train1$age, predict(lmorig, newdata = test1))


llmmodel <- lm(age~ Shuckedweight + Shellweight + SexI, data = train1)
summary(llmmodel)

predlm <- predict(llmmodel, newdata = test1)
rmselm <- rmse(predlm, test1$age)
vif(llmmodel)
mapelm <- mean(abs(test$age-predlm)/test$age)*100
errorlm <- regr.eval(train1$age, predict(llmmodel, newdata = test1))

nosexi <- avaal[which(avaal$SexI)]

aicmodel <- stepAIC(llmmodel)

lmaic <- lm(aicmodel)
summary(lmaic)
plot(lmaic)

predaic <- predict(lmaic, newdata = test)
rmseaic <- rmse(predaic, test$age)
mapeaic <- mean(abs(test$age-predaic)/test$age)*100

vif(lmaic)

#to know whether there is pattern or not for this probability should be greater than 5%
lmtest::bptest(llmmodel)


#to do lasso regression we have to have a matrix
avallas <- as.matrix(avaall)
n=nrow(avallas)
set.seed(1000)
split1 <- sample(1:n, 0.7*n, replace = F)
train2 <- avallas[split1,]
test2 <- avallas[-split1,]


y=subset(train2, select=age)
train2 <- subset(train2, select=-age)

ytest = subset(test2, select=age)
test2 <- subset(test2, select=-age)


#doing lasso only
reg1 <-  glmnet(train2, y, alpha=0)

#doing ridge only
reg2 <-  glmnet(train2, y, alpha=1)

plot(reg1,xvar="lambda",label=TRUE) 


plot(reg2,xvar="lambda",label=TRUE) 


cv1 <- cv.glmnet(train2,y, alpha=1)

cv2 <- cv.glmnet(train2,y, alpha=0) 

cv3 <- cv.glmnet(train2, y, alpha=.5)

cv4 <- cv.glmnet(train2, y)

plot(cv1)

plot(cv2)

plot(cv3)
plot(cv4)



fit1=glmnet(train2,y,lambda=cv1$lambda.min,alpha=0.5) 



regr.eval(y,predict(fit1,train2) )
regr.eval(ytest,predict(fit1,test2) )




trainlog <- subset(train1, select=-age)
testlog <- subset(test1, select=-age)

trainlog$agelog <- log(train1$age)
testlog$agelog <- log(test1$age)


#training for pca
n=nrow(datapcaa)
set.seed(1234)
splitt <- sample(1:n, 0.7*n, replace=F )
trainpca <- datapcaa[splitt,]
testpca <- datapcaa[-splitt,]


traintar <- subset(trainpca, select = age)
testtar <- subset(testpca, select = age)



trainpca <- subset(trainpca, select = -age)
testpca <- subset(testpca, select = -age)

pca_data <-prcomp(trainpca )
summary(pca_data)
plot(pca_data)


train.pca <- data.frame(age=traintar$age, pca_data$x)

train.pca <- train.pca[,1:6]

#for test
pca_datatest <-prcomp(testpca, scale. = T)
summary(pca_datatest)
plot(pca_datatest)

pca_datatest <- prcomp(testpca)
test.pca <- data.frame(age=testtar$age, pca_datatest$x)
test.pca <- test.pca[,1:6]


svmtrain <- svm(age~.,train.pca, kernel = 'radial', cost = 6)

train.pre <-predict(svmtrain, newdata = train.pca, type="class")
tabtrdapr <- table(train.pca$age, train.pre)
acc1 <- sum(diag(tabtrdapr))/sum(tabtrdapr)
rmse <- rmse(train.pre, train.pca$age)


#lets do svm
model1 <- svm(train1$age~., data = train1, cost=10 , kernel= "radial", gamma=.1)



train.pre <-predict(model1, newdata = train1, type="class")
rmsetrain <- rmse(train.pre, train1$age)

#lets do mape for svm model
mapesvm<- mean(abs(train.pre-train1$age)/train1$agelog)*100
evaltrain <- regr.eval(train1$age, train.pre)
evaltrain

test.pre <- predict(model1, newdata = test1, type="class")

rmsetest <- rmse(test.pre, test1$agelog)
mapesvmtest <- mean(abs(test.pre-test1$age)/test1$age)*100

evaltest <- regr.eval(test1$age, test.pre)
evaltest






#lets do apriori
library(arules)
rules <- apriori(trans, parameter = list(sup=0.4, conf=.4, target="rules"))


#lets do decicion treed
#apply c5.0
library(C50)
dtc50 <- C5.0(response~., data = train, rules= T)

#this gives the important connections
C5imp(dtc50, pct=TRUE)  

summary(dtc50)


#doing rpart
library(rpart)
dtcart <- rpart(response~., cp=.000000012, data = train1, method = 'class')
plotcp(dtcart)
plot(dtcart,main="Regression Tree for Species",margin=0.90,uniform=TRUE) 


summary(train)



#lets do knn regression

trainnoresponse <- subset(train, select = -age)
testnoreponse <- subset(test, select = -age)
trainres <- subset(train, select = age)
testres <- subset(test, select = age)



#knn 
knnres <- knn.reg(train = trainnoresponse, test = testnoreponse, y=trainres, k=10)
pred <- data.frame(knnres$pred)
res <- rmse(testres, pred)  
res
mean(abs(testres-pred)/testres)*100


#Steps to follow to execute the problem: 
 # 1.	Install the packages FNN, Metrics 
#install.packages("FNN")  #"Fast Nearest Neighbours" for knn regression 
#install.packages("Metrics") #to calculate error metrics for regression

# Run the model 
pred <- knn.reg(train = trainData, test = testData, y = train.tgt, k = 1 ) 
actual <- test.tgt 
pred <- data.frame(pred$pred) 
result2 <- rmse(actual = actual, predicted = pred) 




#lets do lasso regression
trainnoresponse <- subset(train, select = -age)
testnoreponse <- subset(test, select = -age)
trainresponse <- subset(train, select = age)
testresponse <- subset(test, select = age)
lasso <- glmnet(trainnoresponse, trainresponse, family="binomial", alpha=1)




#lets do decision trees
dec <- rpart(age~., data = train, method = 'anova')
predec <- predict(dec, train, type = "vector")
rmsetr <- rmse(predec, train$age)

predectest <- predict(dec, test, type = "vector")
rmsedec <- rmse(predectest, test$age)



#lets do random forest
# Create RF model
library(randomForest)
rf <- randomForest(income ~., data = training, ntree = 500)
# "mtry" is the number of variables tried at each split 
table(predict(rf,testing), testing$income)
rf
plot(rf)
head(mean(rf$err.rate[,1]))
rf$importance


#lets apply adaboost
grid <- expand.grid(iter = seq(50,150,50), maxdepth = c(1:3), nu = seq(0.01, 0.1, 0.01))
trControl <- trainControl(method = "cv", number = 5,  
                          search = "grid", verboseIter = T, allowParallel = T)
best_param <- train(data_train[,-16], data_train$Class, method = "ada", trControl = trControl,
                    tuneGrid = grid)
set.seed(2345)
model <- ada(x = data_train[,-16], y = data_train[,16], test.x = data_test[,-16], test.y = data_test[,16],
             loss = "exponential", iter = 150, nu = 0.005, verbose = T, ... = rpart.control(maxdepth = 1))






#clusterung

fit<- kmeans(mydata,centers=5,iter.max=10) 


#hierachial clustering
d <- dist(mydata,            method = "euclidean") # distance matrix d 
fit <- hclust(d, method="ward")

#spectral clustering
require(kernlab)
specclu = specc(x, kernel = "laplacedot",centers=3) plot(x, col=specclu) 

sa<-1

summary(avaal)

points(avaal$Length)

hist(avaal$Length)

hist(avaal$Diameter)
hist(avaal$Height)
hist(avaal$Wholeweight)
hist(avaal$Shuckedweight)
hist(avaal$Rings)
summary(avaal)

plot(avaal$Rings, avaal$Length)

plot(avaal$Wholeweight, avaal$age)
plot(avaal$Diameter, avaal$age)
plot(avaal$Height, avaal$age)
plot(avaal$Wholeweight, avaal$age)
plot(avaal$Shuckedweight, avaal$age)
plot(avaal$Visceraweight, avaal$age)
plot(avaal$Shellweight, avaal$age)


#the height is more for two points
avaal <- avaal[which(avaal$Height<0.4),]

plot(avaal$Height, avaal$age)

avaal$ringfac <- as.factor(avaal$Rings)
levels(avaal$ringfac)

#according to data age =rings+1.5
age <-as.data.frame(avaal$Rings+1.5)
names(age) <- "age"

#ADDING age column to data
avaal <- cbind(avaal, age)

#removing ring column
avaal <- subset(avaal, select = -Rings)

#convert the factors into dummies
#library(dummies)
#dumm <- dummy(avaalage$Sex)

#avalaged <- cbind(avaalage, dumm)

#avalaged <- subset(avalaged, select = -Sex)

#summary(avaal)

#plot(avaal)

#avaal <- avalaged


#lets do transformation of weight
avalaged$logwholeweight <- log(avalaged$Wholeweight)
avalaged$logShuckedweight <- log(avalaged$Shuckedweight)
avalaged$logVisceraweight <- log(avalaged$Visceraweight)
avalaged$logrings <- log(avalaged$age)

#splitting 
n=nrow(avaal)
set.seed(100)
split <- sample(1:n, 0.7*n, replace = F)
train <- avaal[split,]
test <- avaal[-split,]

summary(avaalage)

corrplot(cor(avaal[-1]))

lm0 <- lm(log(age)~., data = train1)
summary(lm0)
plot(lm0)

lm0aic <- stepAIC(lm0)

lm1 <- lm(age~Sex + Length + Diameter + Height  + Shuckedweight + 
            Visceraweight + Shellweight, data = train)


vif(lm1)
plot(lm1)

summary(lm1)
pm1 <- predict(lm1, newdata = test)
rmse1 <- rmse(pm1, test$age)

rmse1







#lets apply pca
library(dummies)
dumm <- dummy(avaal$Sex)

datapca <- cbind(avaal, dumm)

datapca <- subset(avaal, select = -c(age))

xx <- subset(avaal, select = age)


##library(infotheo)                  
#datapcas <- decostand(datapca, method = 'standardize')                  
#summary(datapcas)                  

#add age for splitting as target variable is required for accuracy
datapcaa <- cbind(datapca, xx)






#lets apply the svm of train data on test data

test.pre <- predict(svmtrain, newdata = test.pca, type="class")
rmsetest <- rmse(test.pre, test.pca$age)




#lets apply linear model to it
lmpca <- pcr(age~., data = train.pca)
summary(lmpca)
plot(lmpca)
validationplot(lmpca,  val.type = "R2")
predplot(lmpca)
coefplot(lmpca)



pcapre <- predict(lmpca, test.pca)
rmse(test.pca$age, pcapre)



lm3 <- lm(log(age)~ Diameter + Height + Shuckedweight + Shellweight, data = train1)
pm3 <- predict(lm3, newdata = test1)
plot(lm3)
rmse3 <- rmse(exp(pm3), test1$age)
rmse3
#lets do transformation of weight



#removing points which lie in cooks radar

plot(lm1aic)


lm0 <- stepAIC(lm1)

lm2 <- lm(age ~ Diameter + Height +  Shuckedweight + Visceraweight + 
            Shellweight + SexF + SexI, data = train)
summary(lm2)
plot(lm2)


vif(lm1)
pm2 <- predict(lm2, newdata = test)
rmse2 <- rmse(pm2, test$age)

rmse2



#no infant
noinfant <- avaalage[which(avaalage$Sex!='I'),]

