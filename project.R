setwd("c:/Rdata")
getwd()

sales_store=read.csv('sales_store.csv',fileEncoding = "euc-kr")
head(sales_store)



#데이터추출하기
library(dplyr)
qty.sports = sales_store %>% 
  select(YM, CATEGORY, ITEM_CNT, QTY, PRICE, MAXTEMP, SALEDAY, STORE_CNT, RAIN_DAY, HOLIDAY, Year, MONTH, Quart) %>% 
  filter(CATEGORY == "스포츠,이온음료") %>% 
  mutate(income = QTY*PRICE)
View(qty.sports)
qty.sports[,14]
frame()

#년도별 매출액
year_income = data.frame(qty.sports %>% 
  group_by(Year) %>% 
  summarise(year_sum=sum(income)))
par(mfrow = c(3, 2))
year_income
barplot(year_sum~Year, data = year_income, main = "income ~ year")
#매출량은 연도가 지남에 따라 증가하고 있다.

#년도마다 월별에 따른 매출액액
month_2009 = data.frame(qty.sports %>% 
  filter (Year == 2009) %>% 
  group_by(MONTH) %>% 
  summarise(month_sum = sum(income)))
month_2009  
barplot(month_sum~MONTH, data = month_2009 ,main = "income ~ month / 2009")

month_2010 = data.frame(qty.sports %>% 
                          filter (Year == 2010) %>% 
                          group_by(MONTH) %>% 
                          summarise(month_sum = sum(income)))
month_2010  
barplot(month_sum~MONTH, data = month_2010 ,main = "income ~ month / 2010")

month_2011 = data.frame(qty.sports %>% 
                          filter (Year == 2011) %>% 
                          group_by(MONTH) %>% 
                          summarise(month_sum = sum(income)))
month_2011  
barplot(month_sum~MONTH, data = month_2011 ,main = "income ~ month / 2011")

month_2012 = data.frame(qty.sports %>% 
                          filter (Year == 2012) %>% 
                          group_by(MONTH) %>% 
                          summarise(month_sum = sum(income)))
month_2012 
barplot(month_sum~MONTH, data = month_2012 ,main = "income ~ month / 2012")

month_2013 = data.frame(qty.sports %>% 
                          filter (Year == 2013) %>% 
                          group_by(MONTH) %>% 
                          summarise(month_sum = sum(income)))
month_2013 
barplot(month_sum~MONTH, data = month_2013 ,main = "income ~ month / 2013")


##온도에 따른 매출량 분석
par(mfrow = c(1, 1))
Temp = data.frame(qty.sports %>% 
  group_by(Year,MONTH, MAXTEMP) %>% 
  summarise(income))
Temp
plot(income~MAXTEMP, data=Temp)
install.packages("ggplot2")
library(ggplot2)
install.packages("plotly")
library(plotly)
ggplot(Temp, aes(x = MONTH, y = income, color = factor(Year))) +
  geom_point(aes(size = MAXTEMP)) +
  scale_color_manual(values = c("red", "orange", "yellow", "green", "blue")) +
  labs(title = "년도별 월별 매출과 최고온도", x = "월", y = "매출") +
  scale_x_continuous(breaks = 1:12) +
  facet_wrap(~Year, ncol = 5) +
  theme_minimal()

ggplot(Temp, aes(x = MAXTEMP, y = income, color = factor(Year), shape = factor(Year), size = 2 * income)) +
  geom_point() +
  labs(title = "MAXTEMP에 따른 income", x = "MAXTEMP", y = "income") +
  theme_minimal()

## 비오는 날과 매출
Rain = data.frame(qty.sports %>% 
                    group_by(Year,MONTH, RAIN_DAY) %>% 
                    summarise(income))
