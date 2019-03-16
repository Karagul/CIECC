#房价对消费的影响

# 待改进：1. 第三部分，可考虑采用季度值
#         2. 原始数据改成可从网上直接下载的年度、月度数据（可用万得直接在excel中更新），其他数据均在R中处理
#         3. 可尝试VAR、省级数据面板回归



library(corrplot)

setwd("C:/Users/Administrator/Desktop/宏观组")
# setwd("F:/")
data = read.csv("./a宏观组-房价消费模型/司徒雪颖 房地产+相关行业/data.csv")
head(data)
tail(data)

names(data) = c("year","HousePrice","GoodsRetSale",
                "CPI","CityCPI","CountryCPI","Population","CityPopulationProp",
                "Deposit","CityDisposableIncome","CityExpenditure","CountryDisposableIncome","CountryExpenditure",
                "Appliance","Furniture","Decoration",
                "Deposit2")


## 一、住宅价格波动对消费的总体影响
data1 = NULL
#价格水平转换处理：2000=100
#以cpi 1979=100 处理房价
baseyear = 2000
data1$HousePrice = data$HousePrice/(data$CPI/data$CPI[data$year==baseyear])
# 以cpi 1979=100 处理城镇居民可支配收入
data1$CityDisposableIncome = data$CityDisposableIncome/(data$CityCPI/data$CityCPI[data$year==baseyear])
# 以cpi 1979=100 处理城镇居民消费支出
data1$CityExpenditure = data$CityExpenditure/(data$CityCPI/data$CityCPI[data$year==baseyear])
# 计算人均储蓄余额，并用cpi1979=100 处理
data1$Deposit = data$Deposit2/data$Population/(data$CPI/data$CPI[data$year==baseyear]) 
data1 = as.data.frame(data1)


lm1 = lm(log(CityExpenditure)~log(CityDisposableIncome)+log(HousePrice)+log(Deposit),data=data1)
summary(lm1)
acf(lm1$residuals)
## 二、住宅价格波动对社会消费品零售总额的影响
data2 = NULL
#价格水平转换处理：2000=100
#以cpi 1979=100 处理房价
baseyear = 2000
data2$HousePrice = data$HousePrice/(data$CityCPI/data$CityCPI[data$year==baseyear])
#计算全国居民收入水平，并用cpi 1979=100 处理
data2$Income = data$CityDisposableIncome/(data$CityCPI/data$CityCPI[data$year==baseyear])*data$CityPopulationProp+
  data$CountryDisposableIncome/(data$CountryCPI/data$CountryCPI[data$year==baseyear])*(1-data$CityPopulationProp)

data2$CityDisposableIncome = data$CityDisposableIncome/(data$CityCPI/data$CityCPI[data$year==baseyear])

# 以cpi 1979=100 处理社消
data2$GoodsRetSale = data$GoodsRetSale/(data$CPI/data$CPI[data$year==baseyear])

data2 = as.data.frame(data2)
# 多重共线性
par(mfrow=c(1,1))
corrplot.mixed(cor(log(na.omit(data2))),lower = "number", upper = "color",tl.col="black")

lm21 = lm(log(GoodsRetSale)~log(Income)+log(HousePrice)+log(dplyr::lag(GoodsRetSale,1)),data=data2)
summary(lm21)
lm22 = lm(log(GoodsRetSale)~log(Income)+log(HousePrice)+log(dplyr::lag(GoodsRetSale,1))+log(dplyr::lag(GoodsRetSale,2)),data=data2)
summary(lm22)
par(mfrow=c(2,2))
plot(lm2)

## 三、住宅价格波动对房地产相关行业销售的影响
data3 = NULL
baseyear = 2001
#价格水平转换处理：2001=100
#以cpi 1979=100 处理房价
data3$HousePrice = data$HousePrice/(data$CPI/data$CPI[data$year==2001])
#计算全国居民收入水平，并用cpi1979=100 处理
data3$Income = data$CityDisposableIncome/(data$CityCPI/data$CityCPI[data$year==2001])*data$CityPopulationProp+
  data$CountryDisposableIncome/(data$CountryCPI/data$CountryCPI[data$year==2001])*(1-data$CityPopulationProp)
  
#以cpi 1979=100 处理商品零售额
data3$Appliance = data$Appliance/(data$CPI/data$CPI[data$year==2001])
data3$Furniture = data$Furniture/(data$CPI/data$CPI[data$year==2001])
data3$Decoration = data$Decoration/(data$CPI/data$CPI[data$year==2001])
data3 = as.data.frame(data3)
### （一）对家电商品消费的影响
lm31 = lm(log(Appliance)~log(Income)+log(HousePrice)+log(dplyr::lag(Appliance,1)),data=data3)
summary(lm31)
par(mfrow=c(2,2))
plot(lm31)



### （二）对家具商品消费的影响
lm32 = lm(log(Furniture)~log(Income)+log(HousePrice)+log(dplyr::lag(Furniture,1)),data=data3)
summary(lm32)
par(mfrow=c(2,2))
plot(lm32)


### （三）对装潢材料消费的影响
lm33 = lm(log(Decoration)~log(Income)+log(HousePrice)+log(dplyr::lag(Decoration,1)),data=data3)
summary(lm33)
par(mfrow=c(2,2))
plot(lm33)

acf(lm31$residuals)
acf(lm32$residuals)
acf(lm33$residuals)

par(mfrow=c(1,1))