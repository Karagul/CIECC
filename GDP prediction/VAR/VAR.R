
library(corrplot)
library(tseries)
library(zoo)
library(urca)
library(vars)
library(sqldf)
library(TSA)
library(forecast)

setwd("E:/graduate/实习求职/实习/CICCE实习/")
#月度数据的处理---------------------------------------------
monthdata = read.csv("./宏观组/GDP预测/VAR/月度数据.csv")
monthdata$ym = as.character(monthdata$ym)
head(monthdata)


#社零额1/2月没数据的用线性插值法补上
monthdata[,5] = na.approx(monthdata[,5])

# 固定资产投资为累计值，缺失1月数据，1月改为2月值的1/2，再差分得到月度值
a=4
for(i in length(monthdata[,a]):2)
{
  if(is.na(monthdata[i,a]) & !is.na(monthdata[i-1,a]))
  {
    monthdata[i,a]=monthdata[i-1,a]*0.5
  }
}
    
for(i in 1:length(monthdata[,a]))
{
  if(!is.na(monthdata[i,a]) & !length(grep("年1月", monthdata[i,1])))
  {
    monthdata[i,a] = monthdata[i,a]-monthdata[i+1,a]
  }
}


#固定资产投资有明显的季节波动和异方差
plot(ts(monthdata[,a][length(monthdata[,a]):1],end=c(2018,11),start=c(2000,01),frequency = 12))

#进出口金额换算成人民币亿元
monthdata$export = monthdata$export*monthdata$rate*0.00001
monthdata$import = monthdata$import*monthdata$rate*0.00001
plot(ts(monthdata[,6][length(monthdata[,6]):1],end=c(2018,11),start=c(2000,01),frequency = 12))
plot(ts(monthdata[,7][length(monthdata[,7]):1],end=c(2018,11),start=c(2000,01),frequency = 12))

#提取年月
monthdata$year = as.numeric(substr(monthdata[,1],1,4))
monthdata$month = substr(monthdata[,1],6,7)
monthdata$month = as.numeric(sub("月","",monthdata$month))
#提取年季
monthdata$yq = paste(monthdata$year,monthdata$quarter,sep="-")


#剔除价格因素
monthdata[,c(4,5,6,7,8)] = monthdata[,c(4,5,6,7,8)]/monthdata$CPI2
head(monthdata)
# 提取季度数据
quaterdata = sqldf('select year,quarter,yq,
                    sum(invest) as inv1,
                    sum(retail) as ret1,
                    sum(export) as exp1,
                    sum(import) as imp1,
                    avg(CPI2) as CPI1
                    from monthdata group by yq')
quaterdata = quaterdata[-76,]#缺12月数据，第四季度不完整，去掉第四季度
M2 = sqldf("select M2 from monthdata where month in (3,6,9,12) order by yq")
quaterdata = cbind(quaterdata,M2)
head(quaterdata)


#季度数据的处理
seasonGDP = read.csv("./宏观组/GDP预测/VAR/季度数据.csv")
head(seasonGDP)
dim(seasonGDP)

#合并表格
data = merge(quaterdata,seasonGDP[,c(2,3,8,9)],by=c("year","quarter"),all.x=TRUE)
head(data)


#删除开头结尾的缺失值
# library(mice)
# md.pattern(data)
# library("VIM")
# aggr(data,prop=FALSE,numbers=TRUE)
# data1<-na.omit(data) #2001-01 2018-03
# head(data1)





#全数据
data1 = data[,-c(1,2,3)]
plot(ts(data1,start=c(2001,1),frequency = 4),main = "time series")

#训练集测试集
dataTR = data1[1:72,]
dataTE = data1[73:75,]

#取对数差分
data2 = dataTR[1:(dim(dataTR)[1]-1),]
for(i in 1:8)
{
  data2[,i] = diff(log(dataTR[,i]))
} 
head(data2)

plot(ts(data2,start=c(2001,1),frequency = 4),main = "time series")


#相关系数
r = cor(na.omit(data2))
corrplot.mixed(r,lower="number",upper="color",addrect = 3,tl.col = "black",tl.cex = 0.7,rect.col = "black")

#VAR------------------------------------------------------------------------
#单位根检验
# 原序列kpss检验在95%水平显著，非平稳
# 一阶差分序列在95%水平不显著，平稳
# 一阶单整
for(i in 1:6)
{
  print(kpss.test(data1[,i],null="Level"))
  print(kpss.test(data1[,i],null="Trend"))
  print(kpss.test(diff(data1[,i]),null="Level"))
  print(kpss.test(diff(data1[,i]),null="Trend"))
  
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
at = ca.jo(data2,type="trace",ecdet="const",season = 12)
at@cval;at@teststat
# 拒绝r<=1的原假设，但不能拒绝r<=2的原假设，至少存在一组长期协整关系。
ae = ca.jo(data2,type="eigen",ecdet="const",season = 12)
ae@cval;ae@teststat

# 是否使用1阶或4阶差分的数据建立VAR
# data2 = data.frame(apply(data2,2,diff,1)) #1阶的效果已经很好了

#建立VAR、确定最大滞后阶数
VARselect(data2, lag.max =4, type = "both",season = 4) 
#模型平稳性检验
p1ct <- VAR(data2, p = 2, type =  "both",season=4)    
p1ct 
plot(p1ct)
plot(p1ct, names = "LNEXRATE")    

plot(stability(p1ct, type = c("OLS-CUSUM"), h = 0.15, dynamic = FALSE, rescale = TRUE))


# computes the multivariate Portmanteau- and Breusch-Godfrey test for serially correlated errors              
ser11 <- serial.test(p1ct, lags.pt = 16, type = "PT.asymptotic")
ser11$serial     
# The null hypothesis in portmanteau test is that the residuals are serially uncorrelated.

# The null hypothesis in normality test is that the residuals are subject to normal-distributed.
norm1 <- normality.test(p1ct)              
norm1$jb.mul
plot(norm1)

var.predict<-predict(p1ct,n.ahead=3,ci=0.95)
plot(var.predict,names="GDP1")
fanchart(var.predict,color = c("red"),names="GDP1")


var.predict$fcst$GDP1

#预测结果还原
fcst_restore=function(pre,before_diff)#预测向量值，差分前的值（如要预测2018，则给2017-q4的对数实际GDP）
{
  pre[1,] = before_diff+pre[1,]#还原差分
  for(i in 2:dim(pre)[1])
  {
    pre[i,] = pre[i-1,]+pre[i,]
  }
  pre = exp(pre)#还原对数
  return(pre)#得到实际GDP
}

#预测2018年实际GDP
pre = var.predict$fcst$GDP1
before_diff = log(dataTR[72,"GDP1"])
fcst_restore(pre,before_diff)



#实际GDP
dataTE$GDP1

#生成预测结果对比表

pre_res = function(data2,p,n.ahead,before_diff)
{
  p1ct <- VAR(data2, p = p, type =  "both",season=4)
  var.predict<-predict(p1ct,n.ahead=n.ahead,ci=0.95)
  pre = var.predict$fcst$GDP1
  fcst_restore(pre,before_diff)
}

p=1
n.ahead=3
before_diff = log(dataTR[72,"GDP1"])
pre_res(data1,p,n.ahead,before_diff)


#计算RMSE
PRED = fcst_restore(pre,before_diff)[,1]
RMSE = sqrt(sum((PRED-dataTE$GDP1)^2)/3);RMSE
MAE = sum(abs(PRED-dataTE$GDP1))/3;MAE

#p=4 782.2018
#p=1 578.6242
#p=2 545.8547 
#P=3 811.0139

#使用所有数据预测2019

data11 = data1[1:(dim(data1)[1]-1),]
for(i in 1:8)
{
  data11[,i] = diff(log(data1[,i]))
} 
head(data11)
p=1
n.ahead=9
before_diff = log(data1[75,"GDP1"])
pre_res(data11,p,n.ahead,before_diff)

pre_df = data.frame(pre_res(data11,p,n.ahead,before_diff)[,1:3],
                    quater = c("2018Q4","2019Q1","2019Q2","2019Q3","2019Q4"))

pre_df
real_df = data.frame(data1[69:75,"GDP1"],
                     quater = c("2017Q1","2017Q2","2017Q3","2017Q4","2018Q1","2018Q2","2018Q3"))

res = merge(real_df,pre_df,by="quater",all=T)




#脉冲响应分析
var.2c <- VAR(data2, p = 2, type = "both",season=4)
irf(var.2c, impulse =names(data2), response ="GDP1", boot =FALSE)
#瞬时
plot(irf(var.2c, impulse = names(data2), response ="GDP1", boot =FALSE,n.ahead=30,ortho=F, ci = 0.95))
#累积

plot(irf(var.2c, impulse = names(data2), response ="GDP1",n.ahead=30,ortho=F, ci = 0.95,cumulative=T),ylim=c(-1,1.5))


irf(var.2c, impulse = "LNEXRATE", response = names(data2)[-1], boot =FALSE)
plot(irf(var.2c, impulse = "LNEXRATE", response =names(data2)[-1], boot =FALSE))
#方差分解
var.2c <- VAR(data2, p = 4, type = "const",season=4)
vd<-fevd(var.2c, n.ahead = 30)

for(name in names(data2))
{
  plot(vd, names = name)
}


#季度GDP平减指数预测
seasonGDP = read.csv("./宏观组/GDP预测/VAR/季度数据.csv")
head(seasonGDP)
seasonGDP = seasonGDP[c(75:1),]
plot(ts(seasonGDP$deflator2000,start=c(2000,1),frequency = 4))
tsdata = ts(seasonGDP$deflator2000,start=c(2000,1),frequency = 4)
## 平稳行检验 adf.test或pp.test,两者的原假设不同
adf.test(tsdata) 
adf.test(diff(tsdata,differences=2))##差分平稳
# tsdata = diff(tsdata)

plot(stl(diff(tsdata,1,2),"per"))
#模型识别
win.graph(width = 4.875,height = 3,pointsize = 8)
acf(diff(tsdata,1,2),xaxp = c(0,20,10)) #2阶截尾
win.graph(width = 4.875,height = 3,pointsize = 8)
pacf(diff(tsdata,1,2),xaxp = c(0,20,10))#2阶截尾
eacf(diff(tsdata,1,2))#建议建立ARMA(2,2)模型

arima1 = arima(tsdata,order=c(4,1,1),method="ML");arima1
# arima1 = arima(tsdata,order=c(6,0,6),seasonal=list(order=c(6,0,6),period=12),method="ML");arima1
win.graph(width = 4.875,height = 3,pointsize = 8)
plot(tsdata,type = "b")
legend("topright",legend = c("真实值","拟合值"),col = c("black","red"),lty=c(1,1))
points(arima0$fitted,type = "b",col = "red")

mean(abs(arima1$residuals))


arima0=auto.arima(tsdata,trace = T) ##44143.01
accuracy(arima0)
tsdiag(arima0)

plot(forecast(arima0,h=9))



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

