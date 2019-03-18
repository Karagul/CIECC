#
# 改进：进出口选用中国对美国的
# 改进：汇率使用中国对其他国家的汇率
# 改进：其他国家的GDP也纳入考虑范围
# 改进：加入利率
# 改进：对GDP进行季节调整
library(corrplot)
library(tseries)
library(urca)
library(vars)
library(sqldf)
library(TSA)
library(forecast)
# 合并数据、数据预处理
setwd("C:/Users/Administrator/Desktop/宏观组/")
# setwd("E:/graduate/实习求职/CICCE实习/")
seasonGDP = read.csv("数据挖掘/宏观经济预判/宏观/季度GDP.csv")
head(seasonGDP)
dim(seasonGDP)

monthdata = read.csv("数据挖掘/宏观经济预判/宏观/month.csv")
head(monthdata)


data = sqldf('select year,quarter,avg(REAL_RATE) as REAL_RATE_Q,sum(REAL_EX) as REAL_EX_Q,sum(REAL_IM) as REAL_IM_Q from monthdata group by year,quarter')
dim(data)
data = merge(data,seasonGDP,by=c("year","quarter"),all=FALSE)
head(data)
plot(ts(data["REAL_RATE_Q"],start=c(1995,1),frequency = 4),ylab="实际汇率")#实际汇率

exrate = ts(as.double(data[,"REAL_RATE_Q"]),start=c(1995,1),frequency = 4)
plot(stl(exrate,"per"),main="stl分解")

data1 = data[,c("REAL_RATE_Q","REAL_EX_Q","REAL_IM_Q","REAL_GDP")]
plot(ts(data1,start=c(1995,1),frequency = 4))

#取对数
data2 = sqldf("select log(REAL_RATE_Q) as LNEXRATE, 
                      log(REAL_EX_Q) as LNEXPORT,
                      log(REAL_IM_Q) as LNIMPORT,
                      log(REAL_GDP) as LNGDP from data1" )
plot(ts(data2,start=c(1995,1),frequency = 4))
plot(ts(data2["LNEXRATE"],start=c(1995,1),frequency = 4))#对数实际汇率

#相关系数
r = cor(na.omit(data2))
corrplot.mixed(r,lower="number",upper="color",addrect = 3,tl.col = "black",tl.cex = 0.7,rect.col = "black")

#VAR------------------------------------------------------------------------
#单位根检验
# 原序列kpss检验在95%水平显著，非平稳
# 一阶差分序列在95%水平不显著，平稳
# 一阶单整
for(i in 1:4)
{
  print(kpss.test(data2[,i],null="Level"))
  print(kpss.test(data2[,i],null="Trend"))
  print(kpss.test(diff(data2[,i]),null="Level"))
  print(kpss.test(diff(data2[,i]),null="Trend"))
  
}


#协整检验
# EG两步法
#所有的回归结果都显著
z = list()->z1
for(i in 1:4)z[[i]] = lm(data2[,i]~.,data2[,-i])
for(i in 1:4)print(summary(z[[i]]))
#对回归残差的DF检验都显著，说明残差都为I(0)，可能存在协整关系
for(i in 1:4)
{
  z1[[i]] = ur.df(z[[i]]$residuals)
  if(i==1)print(z1[[i]]@cval);print(z1[[i]]@teststat)
}
#pillips-Ouliaris
#都不显著
zz=ca.po(data2,demean="constant",type="Pu") #方差率检验
zz@cval;zz@teststat

zz = ca.po(data2,demean="constant",type="Pz") #多元迹检验
zz@cval;zz@teststat

#josenhan
# 拒绝r<=2的原假设，但不能拒绝r<=3的原假设
at = ca.jo(data2,type="trace",ecdet="const",season = 4)
at@cval;at@teststat
# 拒绝r<=1的原假设，但不能拒绝r<=2的原假设，至少存在一组长期协整关系。
ae = ca.jo(data2,type="eigen",ecdet="const",season = 4)
ae@cval;ae@teststat

# 是否使用1阶或4阶差分的数据建立VAR
data2 = data.frame(apply(data2,2,diff,1)) #1阶的效果已经很好了

#建立VAR、确定最大滞后阶数
VARselect(data2, lag.max = 10, type = "both",season = 4) 
#模型平稳性检验
p1ct <- VAR(data2, p = 4, type =  "both",season=4)    
p1ct 
plot(p1ct)
plot(p1ct, names = "LNEXRATE")    
# computes the multivariate Portmanteau- and Breusch-Godfrey test for serially correlated errors              
ser11 <- serial.test(p1ct, lags.pt = 16, type = "PT.asymptotic")
ser11$serial     
# The null hypothesis in portmanteau test is that the residuals are serially uncorrelated.

# The null hypothesis in normality test is that the residuals are subject to normal-distributed.
norm1 <- normality.test(p1ct)              
norm1$jb.mul
plot(norm1)

#格兰杰因果检验
##help.search("granger")  ## if I FORGET THE CODE FOR granger
var.2c <- VAR(data2, p = 4, type = "both",season=4)
for(name in names(data2))
  {print(causality(var.2c, cause = name))}

#use a robust HC variance-covariance matrix for the Granger test:
causality(var.2c, cause =  "LNEXRATE", vcov.=vcovHC(var.2c))

#脉冲响应分析
var.2c <- VAR(data2, p = 4, type = "both",season=4)
irf(var.2c, impulse = "LNEXRATE", response =names(data2), boot =FALSE)

plot(irf(var.2c, impulse =  "LNEXRATE", response = names(data2),n.ahead=30,ortho=F, ci = 0.95))
plot(irf(var.2c, impulse =  "LNEXRATE", response = names(data2),n.ahead=30,ortho=F, ci = 0.95,cumulative=T))

irf(var.2c, impulse = "LNEXRATE", response = names(data2)[-1], boot =FALSE)
plot(irf(var.2c, impulse = "LNEXRATE", response =names(data2)[-1], boot =FALSE))
#方差分解
var.2c <- VAR(data2, p = 4, type = "const",season=4)
vd<-fevd(var.2c, n.ahead = 30)

for(name in names(data2))
{
  plot(vd, names = name)
}

#预测
zp=predict(var.2c,n.ahead =20,ci = 0.95)
plot(zp)
fanchart(zp)

# 压力测试
# 把未来一期的压力因素的预测值改成压力值，其他变量用预测值
var_model = var.2c
name = "LNEXRATE"
value = 6.0748

press_test(name,value,data2,press = T)

#差分还原
inv_diff = function(diff_series)
{
  origin = rep(0,length(diff_series))
  origin[1] = diff_series[1]
  for(i in 2:length(diff_series))
  {
    origin[i] = diff_series[i]+origin[i-1]
  }
  return(origin)
}


press_test = function(name,value,data2,press=T){

  
  var_model = VAR(data2, p = 4, type =  "both",season=4)
  if(press==T){
    pre1 = predict(var_model,n.ahead =1,ci = 0.95)
    pre_value = c() #新一期预测值
    for(i in 1:length(pre1$fcst))
    {
      pre_value = c(pre_value,pre1$fcst[[i]][1])
    }
  
    #原数据取对数后的最后一期
    last = data.frame(matrix(c(1.861610,8.245735,8.085942,11.148151),1,4))
    names(last) = names(data2)
    # value需取对数并差分
    value_trans = log(value)-last[name]
    
    hstry = data2
    new1 = data.frame(matrix(pre_value,1,4))
    names(new1) =names(data2)
    new1[name] = value_trans
    hstry = data2
    hstry = rbind(hstry,new1)
    
    #再预测:感觉不能重新估计模型，而要用回原来的模型
    var.press = VAR(hstry , p = 4, type = "both",season=4)
    pre.press=predict(var.press,n.ahead =20,ci = 0.95)
    plot(pre.press)
    print(pre.press$fcst)
    #把预测值还原,逆差分，取指数
    pre.press_value = data.frame(cbind(pre.press$fcst[[1]][,1],pre.press$fcst[[2]][,1],
                            pre.press$fcst[[3]][,1],pre.press$fcst[[4]][,1]))
    names(pre.press_value) = names(data2)
    

    wait_trans = rbind(last,pre.press_value)
    trans = data.frame(exp(apply(wait_trans,2,inv_diff)))

  }
  if(press==F){
    pre2 = predict(var_model,n.ahead =2,ci = 0.95)
    pre_value = c() #新二期预测值
    for(i in 1:length(pre2$fcst))
    {
      pre_value = c(pre_value,pre2$fcst[[i]][2,1])
    }
    print(pre_value)}
}

#原数据取对数后的最后一期
# LNEXRATE LNEXPORT LNIMPORT LNGDP
# 1.861610 8.245735 8.085942 11.148151
# ARIMA------------------------------------------------------------------------------------------

library(tseries)
library(sqldf)
library(TSA)
library(forecast)
library(portes)
# 合并数据、数据预处理
setwd("C:/Users/Administrator/Desktop/宏观组/")
# setwd("E:/graduate/实习求职/CICCE实习/")
seasonGDP = read.csv("数据挖掘/宏观经济预判/宏观/季度GDP.csv")
head(seasonGDP)
dim(seasonGDP)

monthdata = read.csv("数据挖掘/宏观经济预判/宏观/month.csv")
head(monthdata)


data = sqldf('select year,quarter,avg(REAL_RATE) as REAL_RATE_Q,sum(REAL_EX) as REAL_EX_Q,sum(REAL_IM) as REAL_IM_Q from monthdata group by year,quarter')
dim(data)
data = merge(data,seasonGDP,by=c("year","quarter"),all=FALSE)
head(data)

data1 = data[,c("REAL_RATE_Q","REAL_EX_Q","REAL_IM_Q","REAL_GDP")]
plot(ts(data1,start=c(1995,1),frequency = 4))

data2 = sqldf("select log(REAL_RATE_Q) as LNEXRATE, 
                      log(REAL_EX_Q) as LNEXPORT,
                      log(REAL_IM_Q) as LNIMPORT,
                      log(REAL_GDP) as LNGDP from data1" )
plot(ts(data2,start=c(1995,1),frequency = 4))


## 平稳行检验 adf.test或pp.test,两者的原假设不同
i=1
tsdata = ts(data2[,i],start=c(1995,1),frequency = 4)
adf.test(tsdata) 
adf.test(diff(tsdata,differences=1))##差分平稳
# tsdata = diff(tsdata)
#模型识别
win.graph(width = 4.875,height = 3,pointsize = 8)
acf(tsdata,xaxp = c(0,20,10)) #2阶截尾
win.graph(width = 4.875,height = 3,pointsize = 8)
pacf(tsdata,xaxp = c(0,20,10))#2阶截尾
eacf(tsdata)#建议建立ARMA(2,2)模型

arima1 = arima(tsdata,order=c(4,0,0),method="ML");arima1

# arima1 = arima(tsdata,order=c(6,0,6),seasonal=list(order=c(6,0,6),period=12),method="ML");arima1
win.graph(width = 4.875,height = 3,pointsize = 8)
plot(tsdata,type = "b")
legend("topright",legend = c("真实值","拟合值"),col = c("black","red"),lty=c(1,1))
points(fitted(arima1),type = "b",col = "red")

mean(abs(arima1$residuals))


arima0=auto.arima(tsdata,seasonal = TRUE,trace = T) ##44143.01
accuracy(arima0)
tsdiag(arima0) 
#汇率 ARIMA(2,0,1)(0,1,1)[4]   

par(mfrow=c(2,2)) 
plot(seq(5,30,5),MahdiMcLeod(resid(arima0))[,4],main="Generalized variance tests", 
     ylab="p-value", xlab="lag",pch=16,ylim=c(0,1))
abline(h=0.05,lty=2)
plot(seq(5,30,5),LjungBox(resid(arima0))[,4],main="Ljung-Box tests", ylab="p-value", 
     xlab="lag",pch=16,ylim=c(0,1));
abline(h=0.05,lty=2) 
Acf(resid(arima0),main="ACF of residuals",lag.max=20) 
plot(resid(arima0),type="o",ylab="Residual",pch=16); title("Residual series");
abline(h=0,lty=2)

win.graph(width = 4.875,height = 3,pointsize = 8)
plot(forecast(arima0,h=8))
pre  = forecast(arima0,h=6)
pre_value = ts(c(pre$x,pre$mean),start = 1995,frequency =4)
pre_value = exp(pre_value)
pre_lower = exp(pre$lower[,2])
pre_upper = exp(pre$upper[,2])

par(mfrow=c(1,1))
plot(pre_value,lwd=2,xlab="")
points(exp(pre$mean),col="blue",type = "l",lwd=3)
points(pre_lower,col = "grey",type = "l",lwd=2,lty="dashed")
points(pre_upper,col = "grey",type = "l",lwd=2,lty="dashed")
abline(v=c(2018,2),col="red",lty="dashed")

mean(window(pre_value,start=2018,end=c(2018,4)))
m = mean(window(pre_value,start=2019))

sigma = sqrt(var(pre$x))
m-2*sigma;m-sigma;m;m+sigma;m+2*sigma



