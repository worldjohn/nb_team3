setwd("c:/Rdata")
getwd()

sales_store=read.csv('sales_store.csv',fileEncoding = "euc-kr")
head(sales_store)



#데이터추출하기
library(dplyr)
library(ggplot2)
library(plotly)
library(leaps)
library(car)
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
plot(income~RAIN_DAY, data = qty.sports)
#비가 많이 오면 매출량이 증가하는건가?

## 매장수 매출의 관계
plot(income~STORE_CNT, data=qty.sports)
#별로 관계없어보이는데?


#ITEM_CNT
qty.sports %>%
  group_by(ITEM_CNT) %>%
  summarize(avg_income = mean(income)) %>%
  ggplot(aes(x = ITEM_CNT, y = avg_income)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "ITEM_CNT에 따른 매출액 평균", x = "ITEM_CNT", y = "매출액 평균") +
  theme_minimal()






###################################데이터분석##################################

#종속변수의 정규성 검사
shapiro.test(qty.sports[,14])
powerTransform(qty.sports[,14])
shapiro.test(qty.sports[,14]*(-0.03691399)) #ㅈㄴ이상함함
log_qty=log(qty.sports[,14])
shapiro.test(log_qty) #0.1672  이걸로 결정
sqrt_shap=sqrt(qty.sports[,14])
shapiro.test(sqrt_shap)  ##0.04235  


##qty랑 PRICE를 빼고 진행행야 되나?
model1 = lm(log(income) ~ ITEM_CNT + QTY + PRICE + MAXTEMP+
              SALEDAY + STORE_CNT + RAIN_DAY + HOLIDAY + Year + MONTH, data = qty.sports)
summary(model1)

cor_result= cor(qty.sports[,c("ITEM_CNT", "QTY", "PRICE", "MAXTEMP", "SALEDAY", "STORE_CNT", "RAIN_DAY", "HOLIDAY", "Year", "MONTH")])
cor_result_bool = cor_result > 0.7 | cor_result < -0.7
cor_result_bool
pairs(qty.sports[,c("ITEM_CNT", "QTY", "PRICE", "MAXTEMP", "SALEDAY", "STORE_CNT", "RAIN_DAY", "HOLIDAY", "Year", "MONTH")])


#saleday랑 store cnt가 독립적이지 않은데 괜찮은가??
both = step(model1, direction = "both", trace = FALSE)
summary(both)
###backward 방법으로 선택함 #holiday 빼고 전부 포함했네
forward = step(model1, direction = "forward", trace = FALSE)
summary(forward)

backward = step(model1, direction = "backward", trace = FALSE)
summary(backward)


leaps = regsubsets(log(income) ~ ITEM_CNT + QTY + PRICE + MAXTEMP+
                     SALEDAY + STORE_CNT + RAIN_DAY + HOLIDAY + Year +
                     MONTH, data = qty.sports, nbest=10)
summary(leaps)
plot(leaps)
plot(leaps, scale = "adjr2")
plot(leaps, scale = "Cp")

summary.out=summary(leaps)
which.max(summary.out$adjr2)
summary.out$which[71,]
##price랑 holiday 빼고 전부 포함 , nbest 값은 클수록 전부 비교할 수 있어서 좋음
##nbest를 5,10으로 진행해봤는데 결과가 똑같음음
model_leap = lm(log(income) ~ ITEM_CNT + QTY + MAXTEMP+
              SALEDAY + STORE_CNT + RAIN_DAY + Year + MONTH, data = qty.sports)
summary(model_leap)

anova(model1,both, model_leap)










