library(MASS)
library(readr)
?birthwt 
names(birthwt)
dim(birthwt)
birthwt$low <- factor(birthwt$low, levels = c(0,1),labels = c("No", "Yes"))
birthwt$race <- factor(birthwt$race, levels = c(1:3), labels=c("white","black","other"))
birthwt$smoke <- factor(birthwt$smoke, levels = c(0,1), labels = c("No", "Yes"))
birthwt$ht <- factor(birthwt$ht, levels = c(0,1), labels = c("No", "Yes"))
birthwt$ui <- factor(birthwt$ui, levels = c(0,1),labels = c("No", "Yes"))
birthwt$ptl <- factor(birthwt$ptl)
birthwt$ftv <- factor(birthwt$ftv)
birthwt$X1<- NULL
birthwt
birthwt$ht
summary(birthwt)
barplot(table(birthwt$race), xlab= "Race", ylab="Frequency")
barplot(table(birthwt$smoke), xlab= "Smoking Status", ylab="Frequency")
library(ggplot2)
ggplot(data=birthwt, aes(x=lwt, y=bwt)) +
  geom_point()
ggplot(data=birthwt, aes(x=ptl, y=bwt)) +
  geom_boxplot()
ggplot(data=birthwt, aes(x=smoke, y=bwt)) +
  geom_boxplot()
ggplot(data=birthwt, aes(x=ui, y=bwt)) +
  geom_boxplot()
ggplot(data=birthwt, aes(x=race, y=bwt)) +
  geom_boxplot()
pairs(birthwt)
cor(x=birthwt$age,y=birthwt$bwt)
cor(x=birthwt$lwt,y=birthwt$bwt)
plot(birthwt$low,birthwt$lwt,xlab="Low birth weight or not",ylab="mother's weight at last menstrual period(in pounds)")
plot(birthwt$low,birthwt$age,xlab="Low birth weight or not",ylab="Age of mother")
plot(birthwt$low,birthwt$race)
glm(birthwt$low~birthwt$age+birthwt$lwt,family="binomial")
glm(birthwt$low~birthwt$age+birthwt$lwt+birthwt$smoke,family="binomial")
summary(glm(birthwt$low~birthwt$age+birthwt$lwt+birthwt$smoke,family="binomial"))
glm(birthwt$low~birthwt$lwt+birthwt$smoke+birthwt$ht+birthwt$race,family="binomial")
summary(glm(birthwt$low~birthwt$lwt+birthwt$smoke+birthwt$ht+birthwt$race,family="binomial"))
glm(birthwt$low~birthwt$lwt+birthwt$smoke+birthwt$ht+birthwt$race+birthwt$ui,family="binomial")
summary(glm(birthwt$low~birthwt$lwt+birthwt$smoke+birthwt$ht+birthwt$race+birthwt$ui,family="binomial")
)
glm(birthwt$low~birthwt$lwt+birthwt$smoke+birthwt$ht+birthwt$race+birthwt$ui,family="binomial")
summary(glm(birthwt$low~birthwt$lwt+birthwt$smoke+birthwt$ht+birthwt$race+birthwt$ui,family="binomial"))
model.low=glm(birthwt$low~birthwt$lwt+birthwt$smoke+birthwt$ht+birthwt$race+birthwt$ui,family="binomial")
model.low$fit
plot(birthwt$low,model.low$fit)
summary(birthwt$low)
birthwt$low.pred=ifelse(model.low$fit>0.315,1,0)
table(birthwt$low,birthwt$low.pred)
sensitivity<-43/59
sensitivity
82/130
42/59
87/130
41/59
41/59
library(pROC)
roc(birthwt$low,model.low$fit)
plot(roc(birthwt$low,model.low$fit))
