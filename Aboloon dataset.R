

rm(list = ls())
library(glmnet)
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
avaal<- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data", header = F)


names(avaal) <- c("Sex", "Length", "Diameter", "Height", "Wholeweight", "Shuckedweight", "Visceraweight", "Shellweight", "Rings")

names(avaal)
str(avaal)

plot(avaal$Height, avaal$age)

#the height is more for two points
avaal <- avaal[which(avaal$Height<0.4),]

corrplot(cor(avaal[-1]))


#lets add all weight columns and make it into one
#xx <- avaal$Shuckedweight+avaal$Visceraweight+avaal$Shellweight
#head(xx)

#avaal <- subset(avaal, select = -c(Shuckedweight, Visceraweight, Shellweight))
#avaal <- cbind(avaal, weight=xx)


#avaal <- subset(avaal, select = -Wholeweight)

#according to data age =rings+1.5
age <-as.data.frame(avaal$Rings+1.5)
names(age) <- "age"

#ADDING age column to data
avaal <- cbind(avaal, age)

#removing ring column
avaal <- subset(avaal, select = -Rings)
avaal <- subset(avaal, select = -Wholeweight)



#lets dummify
library(dummies)

dumm1 <- dummy(avaal$Sex)
avaal <- subset(avaal, select = -Sex)
avaal <- cbind(avaal, dumm1 )



summary(avaal)

stan <- subset(avaal, select = -c(age, SexF, SexI, SexM))
xxx <- subset(avaal,select = c(age, SexF, SexI, SexM))


#standaradize
stan <- decostand(stan, method = "standardize")

#lets add the columns
avaall <- cbind(stan,xxx)


n=nrow(avaall)
set.seed(100)
split <- sample(1:n, 0.7*n, replace = F)
train <- avaall[split,]
test <- avaall[-split,]



llmmodel <- lm(age~Diameter + Height + Shuckedweight + Shellweight + SexI, data = train)
summary(llmmodel)

predlm <- predict(llmmodel, newdata = test)
rmselm <- rmse(predlm, test$age)
vif(llmmodel)


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


#lets do svm
model1 <- svm(train$age~., data = train, cost=10 , kernel= "radial", gamma=.1)


train.pre <-predict(model1, newdata = train, type="class")
rmsetrain <- rmse(train.pre, train$age)

#lets do mape for svm model
mapesvm<- mean(abs(train.pre-train$age)/train$age)*100


test.pre <- predict(model1, newdata = test, type="class")

rmsetest <- rmse(test.pre, test$age)
mapesvmtest <- mean(abs(test.pre-test$age)/test$age)*100

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

lm0 <- lm(age~., data = train)
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

datapca <- subset(datapca, select = -c(age))

xx <- subset(avaal, select = age)


##library(infotheo)                  
#datapcas <- decostand(datapca, method = 'standardize')                  
#summary(datapcas)                  

#add age for splitting as target variable is required for accuracy
datapcaa <- cbind(datapcas, xx)






#training for pca
n=nrow(datapca)
set.seed(1234)
splitt <- sample(1:n, 0.7*n, replace=F )
trainpca <- datapcas[splitt,]
testpca <- datapcas[-splitt,]


traintar <- subset(trainpca, select = age)
testtar <- subset(testpca, select = age)



trainpca <- subset(trainpca, select = -age)
testpca <- subset(testpca, select = -age)

pca_data <-prcomp(trainpca, scale. = T)
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



lm3 <- lm(age~Sex + Diameter + Height + Shuckedweight + Shellweight, data = train)
pm3 <- predict(lm3, newdata = test)
plot(lm3)
rmse3 <- rmse(pm3, test$age)
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

