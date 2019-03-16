library(ggplot2)
library(grid)
library(tseries)
library(portes)
library(forecast)
library(TSA)
library(reshape2)

data = read.csv("C:/Users/Administrator/Desktop/宏观组/GDP预测/GDP.csv")
head(data)

# 换算成1978年的不变价GDP
data$不变价GDP = data$GDP现价[1]*data$GDP指数/100

#取对数
tsdata = ts(data$不变价GDP,start=1978)
plot(tsdata)
tsdata = log(tsdata)
plot(tsdata)

p1 = ggplot(data,aes(年份,不变价GDP,colour = I("steelblue")))+
  geom_line(size = 1)+geom_point(size=3)+xlab("时间")+ylab("GDP")
p2 = ggplot(data,aes(年份,log(不变价GDP),colour = I("steelblue")))+
  geom_line(size = 1)+geom_point(size=3)+xlab("时间")+ylab("GDP")
grid.newpage()
pushViewport(viewport(layout = grid.layout(1,2)))
vplayout <- function(x, y) {
  viewport(layout.pos.row = x, layout.pos.col = y)
}
print(p1, vp = vplayout(1,1))
print(p2, vp = vplayout(1,2))

# 指数平滑法——————————————————————————————————————————————————————————————————
m1 = window(tsdata,end=2013);m1
m2 = window(tsdata,start=2014);m2

# 1.简单指数平滑 适用条件：相加模型，并且处于恒定水平和没有季节性变动的时间序列

X1<-HoltWinters(m1, beta=FALSE, gamma=FALSE)
mf1 = forecast(X1,h=4)
accuracy(mf1,m2)
# 2.霍尔特指数平滑法  适用条件：一个增长或降低趋势的、没有季节性的相加模型
X2<-HoltWinters(m1, gamma=FALSE) 
mf2 = forecast(X2,h=4)
accuracy(mf2,m2)

win.graph(width = 4.875,height = 3,pointsize = 8)
plot(X2$x,type = "b",ylab = "采购价格")
legend("bottomright",legend = c("真实值","拟合值"),col = c("black","red"),lty=c(1,1))
points(X2$fitted[,1],type = "b",col = "red")

# 混成检验
par(mfrow=c(2,2)) 
plot(MahdiMcLeod(resid(X2))[,1],MahdiMcLeod(resid(X2))[,4],main="Generalized variance tests", 
     ylab="p-value", xlab="lag",pch=16,ylim=c(0,1))
abline(h=0.05,lty=2)
plot(LjungBox(resid(X2))[,1],LjungBox(resid(X2))[,4],main="Ljung-Box tests", ylab="p-value", 
     xlab="lag",pch=16,ylim=c(0,1));
abline(h=0.05,lty=2) 
Acf(resid(X2),main="ACF of residuals",lag.max=20) 
plot(resid(X2),type="o",ylab="Residual",pch=16); title("Residual series");
abline(h=0,lty=2)

##ARIMA——————————————————————————————————————————————————————————————————

## 平稳行检验 adf.test
adf.test(tsdata) 
adf.test(diff(tsdata,differences=1))##差分平稳

tsdata_diff = diff(tsdata,differences=1)
plot(tsdata_diff)
#模型识别
par(mfrow = c(2,2))
acf(m1);pacf(m1);acf(diff(m1,1));pacf(diff(m1,1))
par(mfrow=c(1,1))
res = armasubsets(m1,nar=15,nma=15,y.name="test",ar.method = "ols")
plot(res)


arima1 = Arima(m1,order=c(2,1,1),method="ML");arima1
mf3 = forecast(arima1,h=4)
accuracy(mf3,m2)


win.graph(width = 4.875,height = 3,pointsize = 8)
plot(tsdata,type = "b")
legend("topright",legend = c("真实值","拟合值"),col = c("black","red"),lty=c(1,1))
points(fitted(arima1),type = "b",col = "red")


arima0=auto.arima(m1,d=1,max.Q=6,max.P=6,start.P=3,start.Q=3,
                  max.q=6,max.p=6,start.q=3,start.p=3,
                  seasonal = FALSE,trace = T) ##44143.01
accuracy(arima0)
tsdiag(arima0)

mf4 = forecast(arima0,h=4)
accuracy(mf4,m2)

par(mfrow=c(2,2)) 
plot(seq(5,30,5),MahdiMcLeod(resid(arima1))[,4],main="Generalized variance tests", 
     ylab="p-value", xlab="lag",pch=16,ylim=c(0,1))
abline(h=0.05,lty=2)
plot(seq(5,30,5),LjungBox(resid(arima1))[,4],main="Ljung-Box tests", ylab="p-value", 
     xlab="lag",pch=16,ylim=c(0,1));
abline(h=0.05,lty=2) 
Acf(resid(arima1),main="ACF of residuals",lag.max=20) 
plot(resid(arima1),type="o",ylab="Residual",pch=16); title("Residual series");
abline(h=0,lty=2)

##模型比较

# 拟合+预测
pre1 = forecast(arima1,h=4)
pre1 = c(pre1$fitted,pre1$mean)
pre1 = exp(pre1)


pre2 = forecast(X2,h=4)
pre2 = c(pre2$fitted,pre2$mean)
pre2 = exp(pre2)

# 仅预测
pre1 = forecast(arima1,h=4)
pre1 = pre1$mean
pre1 = exp(pre1)


pre2 = forecast(X2,h=4)
pre2 = pre2$mean
pre2 = exp(pre2)

y=data[(dim(data)[1]-length(pre1)+1):dim(data)[1],c("年份","不变价GDP")]
y$arima模型预测值 = pre1
y$双参数指数平滑预测值 = pre2
y

y1=melt(y,id='年份')
ggplot(y1, aes(x=年份, y=value, group=variable,color=variable)) + 
  geom_line(size = 1) +geom_point(size=2)+ labs(x='年份',y='GDP值') +
  theme(legend.title=element_blank())


#预测至2030————————————————————————————————————————————————————————————————————
par(mfrow=c(1,2))
arima_all = Arima(tsdata,order=c(2,1,1),method="ML")
plot(forecast(arima_all,h=13),main="")
pre = forecast(arima_all,h=13)
pre_value = ts(c(pre$x,pre$mean),start = 1978)
pre_value = exp(pre_value)
pre_lower = exp(pre$lower[,2])
pre_upper = exp(pre$upper[,2])

plot(pre_value,lwd=2,xlab="")
points(exp(pre$mean),col="blue",type = "l",lwd=3)
points(pre_lower,col = "grey",type = "l",lwd=2,lty="dashed")
points(pre_upper,col = "grey",type = "l",lwd=2,lty="dashed")
abline(v=2017,col="red",lty="dashed")

par(mfrow=c(1,1))



#预测5年跟迭代预测5年是一样的结果
#由arima直接输出结果
tsdata = ts(data$不变价GDP,start=1978)
tsdata = log(tsdata)
arima_all = Arima(tsdata,order=c(2,1,1),method="ML")
pre = forecast(arima_all,h=13)
pre$mean

#迭代预测
# 预测值与模型参数、前面的序列值都有关系
# 以前面的预测值+真实值重新建立模型，参数也被改变
tsdata = ts(data$不变价GDP,start=1978)
tsdata = log(tsdata)
res = c()
while(TRUE)
  {
  arima_all = Arima(tsdata,order=c(2,1,1),method="ML")
  pre_value1 = forecast(arima_all,h=1)$mean
  res = c(res,pre_value1)
  tsdata = ts(c(tsdata,pre_value1),start=1978)
  if(as.numeric(time(pre_value1)==2030)){break}
}
res

# 模型仍然是用真实值建立的，参数不变
tsdata = ts(data$不变价GDP,start=1978)
tsdata = log(tsdata)
arima_all = Arima(tsdata,order=c(2,1,1),method="ML")
res = c()
while(TRUE)
{
  
  pre_value1 = forecast(tsdata,model = arima_all,h=1)$mean
  res = c(res,pre_value1)
  tsdata = ts(c(tsdata,pre_value1),start=1978)
  if(as.numeric(time(pre_value1)==2030)){break}
}
res
