library(nnet)

getDataSet<-function(item,from,to,size){
  dataframe<-NULL
  to<-to-size+1
  for(i in from:to) {
    start<-i
    end<-start+size-1
    temp<-item[c(start:end)]
    dataframe<-rbind(dataframe,t(temp))
  }
  return(dataframe)
}

INPUT_NODES<-4
HIDDEN_NODES<-INPUT_NODES*2
OUTPUT_NODES<-2
ITERATION<-500

in_learning<-getDataSet(A3$전남,1,10,INPUT_NODES)
in_learning

out_learning<-getDataSet(A3$전남,5,12,OUTPUT_NODES)
out_learning

model<-nnet(in_learning,out_learning,size=HIDDEN_NODES,linout = TRUE,rang = 0.1,skip=TRUE,maxit = ITERATION)

in_forecasting<-getDataSet(A3$전남,9,12,INPUT_NODES)
in_forecasting

predicted_values<-predict(model,in_forecasting,type="raw")
predicted_values

real<-getDataSet(A3$전남,13,14,OUTPUT_NODES)
real

ERR<-abs(real-predicted_values)
MAPE<-mean(ERR/real)*100
MAPE

plot(c(1:2),real, type="o", col="blue")
lines(c(1:2),predicted_values, type="o", col="red")

#예측

in_learning<-getDataSet(A3$전남,1,18,INPUT_NODES)
in_learning


out_learning<-getDataSet(A3$전남,5,20,OUTPUT_NODES)
out_learning

model<-nnet(in_learning,out_learning,size=HIDDEN_NODES,linout = TRUE,rang = 0.1,skip=TRUE,maxit = ITERATION)


in_forecasting<-getDataSet(A3$전남,17,20,INPUT_NODES)
in_forecasting

predicted_values<-predict(model,in_forecasting,type="raw")
predicted_values

real<-A3$전남[19:20]
real

plot(1:2,real,xlab="기간",ylab="종가",ylim = c(2000,3000),type="o")
lines(2:3,predicted_values,type="o",col="red")
grid()
plot(11:20,A3[11:20,13],ylim = c(0,3000))
lines(19:20,predicted_values,type="o",col="red")
