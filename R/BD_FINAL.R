A1 <- read.csv("doc/jong.csv")

A2 <- A1
A2 <- A1[,c(5,6,10,11,12,14,16)]

Z1 <- data.frame(table(A3$진료과목코드))

A3 <- transform(A2,
                main = ifelse(A2$주상병코드 == "B01"|
                                A2$주상병코드 == "B018"|
                                A2$주상병코드 == "B019",1,NA),
                sub = ifelse(A2$부상병코드 == "J209",1,
                             ifelse(A2$부상병코드 == "L299",2,
                             ifelse(A2$부상병코드 == "J304",3,
                             ifelse(A2$부상병코드 == "L509",4,
                             ifelse(A2$부상병코드 == "L239",5,
                             ifelse(A2$부상병코드 == "J00",6,
                             ifelse(A2$부상병코드 == "J069",7,0)))))
                             )
                ))

prop.table(table(A3$성별코드))


A3$sub <- factor(A3$sub)
A3$main <- factor(A3$main)
A3$성별코드 <- factor(A3$성별코드)
A3$심결가산율 <- factor(A3$심결가산율)
A3$진료과목코드 <- factor(A3$진료과목코드)
A3$연령대코드 <- (5*(A3$연령대코드-1)+5*(A3$연령대코드)-1)/2

A3 <- A3[A3$진료과목코드==1|A3$진료과목코드==4|A3$진료과목코드==11|
           A3$진료과목코드==14|A3$진료과목코드==23,]

train <- A3[1:13186,] #70%
test <- A3[13187:nrow(A3),] #30%

library(dplyr)
library(plyr)
library(ggplot2)
A3 <- A3 %>%
  na.omit()

train1 <- train[train$진료과목코드==14|
                  train$진료과목코드==23,]
test1 <- test[test$진료과목코드==14|
                test$진료과목코드==23,]

okfine <- lm(심결본인부담금~연령대코드+진료과목코드+
                  심결가산율+입내원일수,data = train)



ok2 <- lm(심결본인부담금~연령대코드,data=train)

summary(okfine)
attach(train)
plot(심결본인부담금~연령대코드)
abline(okfine$model$연령대코드)

relweights(okfine)

pred <- predict(okfine,newdata=test1)
cor(pred,test1$심결본인부담금,use="pairwise.complete.obs")



ggplot()+
  geom_point(aes(x=연령대코드,y=심결본인부담금),data = train)+
  coord_cartesian(ylim = c(0,100000)) +
  geom_smooth(aes(x=연령대코드,y=심결본인부담금))

geom_smooth(aes(x=심결가산율,y=심결본인부담금))

ggplot()+
  geom_jitter(aes(x=심결가산율,y=심결본인부담금),data = train) +
  coord_cartesian(ylim = c(0,75000),
                  xlim = c(10,35)) +
  geom_abline(intercept = 3000, slope =  499.56, 
              col = "blue",lty = 2 ,size=1)

test2 <- read.csv("doc/2011_v.csv")
test2_1 <- test2[,c(5,6,10,11,12,14,16)]

test2_2 <- transform(test2,
                main = ifelse(test2$주상병코드 == "B01"|
                                test2$주상병코드 == "B018"|
                                test2$주상병코드 == "B019",1,NA),
                sub = ifelse(test2$부상병코드 == "J209",1,
                             ifelse(test2$부상병코드 == "J304",2,
                                    ifelse(test2$부상병코드 == "J039",3,
                                           ifelse(test2$부상병코드 == "J069",4,
                                                  ifelse(test2$부상병코드 == "L299",5,NA))))
                )
)

test2_2$sub <- factor(test2_2$sub)
test2_2$성별코드 <- factor(test2_2$성별코드)
test2_2$진료과목코드 <- factor(test2_2$진료과목코드)
test2_2$연령대코드 <- (5*(test2_2$연령대코드-1)+5*(test2_2$연령대코드)-1)/2

test2_2 <- test2_2[test2_2$진료과목코드==1|test2_2$진료과목코드==4|test2_2$진료과목코드==11|
                     test2_2$진료과목코드==14|test2_2$진료과목코드==23,]

test2_2 <- test2_2 %>%
  na.omit()

test_f <- test2_2[1:6653,]

pred <- predict(okfine,newdata=test2_2)
cor(pred,test2_2$입내원일수,use="pairwise.complete.obs").

#2009 1/2 설계
#2009 2/2 측정 [1] 0.8985317
#2011 1/1 측정 [1] 0.8547914

ggEffect=function(fit,x=1,probs=c(0.25,0.5,0.75),point=TRUE,xvalue=NULL){
  df=fit$model
  coef=fit$coef
  name=colnames(df)
  

str(A3)
