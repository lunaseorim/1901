A1 <- read.csv("doc/2009_v.csv")

A2 <- A1[,c(5,6,10,11,12,14,16)]

Z1 <- data.frame(table(A2$부상병코드))

A3 <- transform(A2,
                main = ifelse(A2$주상병코드 == "B01"|
                                A2$주상병코드 == "B018"|
                                A2$주상병코드 == "B019",1,NA),
                sub = ifelse(A2$부상병코드 == "J209",1,
                             ifelse(A2$부상병코드 == "J304",2,
                             ifelse(A2$부상병코드 == "J039",3,
                             ifelse(A2$부상병코드 == "J069",4,
                             ifelse(A2$부상병코드 == "L299",5,NA))))
                             )
                )

A3$sub <- factor(A3$sub)
A3$성별코드 <- factor(A3$성별코드)
A3$진료과목코드 <- factor(A3$진료과목코드)
A3$연령대코드 <- (5*(A3$연령대코드-1)+5*(A3$연령대코드)-1)/2

A3 <- A3[A3$진료과목코드==1|A3$진료과목코드==4|A3$진료과목코드==11|
           A3$진료과목코드==14|A3$진료과목코드==23,]

train <- A3[1:nrow(A3)/2,]
test <- A3[nrow(A3)/2+1:nrow(A3),]

library(dplyr)
library(plyr)
library(ggplot2)
A3 <- A3 %>%
  na.omit()

okfine <- lm(입내원일수~성별코드+연령대코드+진료과목코드+
                    심결요양급여비용총액+sub,data = train)

summary(okfine)

pred <- predict(okfine,newdata=test)
cor1 <- cor(pred,test$입내원일수,use="pairwise.complete.obs")

ploting <- data.frame(pred =pred,
                      value = test$입내원일수)

coef<-okfine$coef
labels=as.character(A)
intercept=coef[1]+coef[2]
slope=coef[3]+coef[4]
intercep

ggplot(aes(x=value,y=pred),data = ploting)+
  geom_point() +
  xlim(0,5) +
  ylim(0,5) +
  geom_smooth(method = "lm")

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
