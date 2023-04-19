setwd('C:/Rdata')
library(dplyr)

#########################################
### STEP O. 데이터 로딩
#########################################
sales_store=read.csv('sales_storecnt_dust_baseball_추가.csv',fileEncoding = "euc-kr")
head(sales_store)

# YM과 마지막 두 컬럼은 삭제: 여러가지 방법이 있음
sales_store = subset(sales_store, select=-c(YM, YM.1, 판매년월))
sales_store = select(sales_store, -c(YM, YM.1, 판매년월))
sales_store = select(sales_store, -c('YM', 'YM.1', '판매년월'))
sales_store = sales_store[, !colnames(sales_store) %in% c('YM', 'YM.1', '판매년월')]
#sales_store = sales_store[, -c('YM', 'YM.1', '판매년월')] ---> 에러남

head(sales_store)

#########################################
### STEP 1. 데이터 전처리
#########################################
# 스포츠,이온음료만 추출
sales.sport = subset(sales_store, CATEGORY='스포츠,이온음료')
head(sales.sport)
dim(sales.sport)

# 이상치 확인
sum(is.na(sales_store))
boxplot(sales.sport['QTY'], horizontal=T)
boxplot(sales.sport['RAIN_DAY'], horizontal=T)
boxplot(sales.sport[c(3, 8)])

# 매출데이터(SALES) 컬럼 생성: 2가지 방법
sales.sport = sales.sport %>% mutate(SALES=QTY*PRICE)
sales.sport$SALES = sales.sport$QTY * sales.sport$PRICE
head(sales.sport)

#########################################
### STEP 2. 독립변수 분석
#########################################
# 년도별 매출 확인
year_sales = data.frame(sales.sport %>% group_by(Year) %>% summarise(year_sum=sum(SALES)))
barplot(year_sum~Year, data=year_sales)
plot(year_sum~Year, data=year_sales, 'l', lwd=2, col=2)
plot(x=year_sales$Year, y=year_sales$year_sum)
# 매출은 해마다 증가함을 보임

# 월별 매출 확인
month_sales = data.frame(sales.sport %>% group_by(MONTH) %>% summarise(month_sum=sum(SALES)))
barplot(month_sum~MONTH, data=month_sales)
plot(month_sum~MONTH, data=month_sales, 'l', lwd=2, col=2)
# 봄에 피크 -> 장마철에 주춤하다가 한여름에 다시 피크 -> 이후 겨울까지 하락이어감

# 기온과 매출과의 관계
plot(SALES~MAXTEMP, data=sales.sport, col=2, pch=16)
cor(sales.sport$SALES, sales.sport$MAXTEMP)
# 상관계수=0.769: 높은 양의 상관관계 보임

# 강우일수와 매출과의 관계
plot(SALES~RAIN_DAY, data=sales.sport, col=2, pch=16)
cor(sales.sport$SALES, sales.sport$RAIN_DAY)
# 상관계수=0.627: 높은 양의 상관관계 보임

# 매장수와 매출과의 관계
plot(SALES~STORE_CNT, data=sales.sport, col=2, pch=16)
cor(sales.sport$SALES, sales.sport$STORE_CNT)
# 상관계수=0.492: 약한 양의 상관관계 보임

# 판매일수와 매출과의 관계
plot(SALES~SALEDAY, data=sales.sport, col=2, pch=16)
cor(sales.sport$SALES, sales.sport$SALEDAY)
# 상관계수=0.518: 약한 양의 상관관계 보임

# 매장수와 판매일수와의 관계
plot(SALEDAY~STORE_CNT, data=sales.sport, col=2, pch=16)
cor(sales.sport$SALEDAY, sales.sport$STORE_CNT)
# 상관계수=0.992597: 아주 강한 양의 상관관계 보임
# 따라서 매장수변수를 제거함

# 상품수와 매출과의 관계
plot(SALES~ITEM_CNT, data=sales.sport, col=2, pch=16)
cor(sales.sport$SALES, sales.sport$ITEM_CNT)
# 상관계수=0.617: 강한 양의 상관관계 보임

# 휴일수와 매출과의 관계
plot(SALES~HOLIDAY, data=sales.sport, col=2, pch=16)
cor(sales.sport$SALES, sales.sport$HOLIDAY)
# 상관계수=0.176: 상관관계 거의 없음

# 황사일수와 매출과의 관계
plot(SALES~DUSTDAY, data=sales.sport, col=2, pch=16)
cor(sales.sport$SALES, sales.sport$DUSTDAY)
# 상관계수=-0.236: 아주 약한 음의 상관관계 보임

# 가격과 매출과의 관계
plot(SALES~PRICE, data=sales.sport, col=2, pch=16)
cor(sales.sport$SALES, sales.sport$PRICE)
# 상관계수=0.297: 아주 약한 양의 상관관계 보임

# 프로야구시즌과 매출과의 관계
baseball_sales = data.frame(sales.sport %>% group_by(BASEBALL) %>% 
                                summarise(mean_sales=mean(SALES)))
barplot(mean_sales~BASEBALL, data=baseball_sales)
cor(sales.sport$SALES, sales.sport$BASEBALL)
# 시즌중의 평균이 비시즌의 두배 이상이다
# 상관계수=0.701: 강한 양의 상관관계 보임

# 프로야구시즌과 기온과의 관계
baseball_temp = data.frame(sales.sport %>% group_by(BASEBALL) %>% 
                               summarise(mean_temp=mean(MAXTEMP)))
barplot(mean_temp ~ BASEBALL, data=baseball_temp)
cor(sales.sport$MAXTEMP, sales.sport$BASEBALL)
# 상관계수=0.891: 아주 강한 양의 상관관계 보임
# 프로야구 시즌과 MAXTEMP는 상과관계가 높으므로 BASEBALL 변수를 제외함

# Month과 매출과의 관계
plot(SALES~MONTH, data=sales.sport, col=2, pch=16)
cor(sales.sport$SALES, sales.sport$MONTH)
# 상관계수=0.153: 상관관계 거의 없음

# Quart와 매출과의 관계
plot(SALES~Quart, data=sales.sport, col=2, pch=16)
cor(sales.sport$SALES, sales.sport$Quart)
# 상관계수=0.153: 상관관계 거의 없음

## 독립변수 집합에서 제외할 변수들
# 매장수(STORE_CNT)와 프로야구시즌(BASEBALL)을 제외함
# QTY, Year, CATEGORY도 제외
sales.sport = subset(sales.sport, select=-c(STORE_CNT, BASEBALL, QTY, Year, CATEGORY))
head(sales.sport)

# 상관계수 크기순:
# 기온(0.769)>강우일수(0.627)>상품수(0.617)>판매일수(0.518)>
#       가격(0.297)>황사일수(-0.236)>휴일수(0.176)>MONTH(0.153)>Quart(0.153)
#########################################
### STEP 3. 종속변수의 정규성 검정
#########################################
hist(sales.sport$SALES, probability=T, breaks=10)
lines(density(sales.sport$SALES),col=2, type='h')
shapiro.test(sales.sport$SALES)  # p-value=0.0009225

# 매출(SALES)변수가 정규성이 없으므로 powerTransform()을 적용해본다
powerTransform(sales.sport$SALES)  # lambda = -0.0369
shapiro.test(sales.sport$SALES^(-0.0369))  # p-value=0.1651

# log() 변환 시도
shapiro.test(log(sales.sport$SALES))  # p-value =0.1672

# 두 p-value에 별 차이가 없으므로 log()변환한 매출을 새로운 종속변수로 선택함
sales.sport$LOG_SALES = log(sales.sport$SALES)
head(sales.sport)
hist(sales.sport$LOG_SALES, probability=T, breaks=10)
lines(density(sales.sport$LOG_SALES),col=2, type='h')

#########################################
### STEP 4. 회귀식 추정
#########################################
# 최초에 모든 독립변수를 다 포함시켜 모델을 만든다
model1 = lm(LOG_SALES ~ ITEM_CNT+PRICE+MAXTEMP+SALEDAY+RAIN_DAY+HOLIDAY+
                DUSTDAY+MONTH+Quart, data=sales.sport)
summary(model1)


#########################################
### STEP 5. 회귀모형 선택
#########################################
## 1) STEP 방법:
min.model = lm(LOG_SALES ~ 1, data=sales.sport)
max.model = lm(LOG_SALES ~ ITEM_CNT+PRICE+MAXTEMP+SALEDAY+RAIN_DAY+HOLIDAY+
                   DUSTDAY+MONTH+Quart, data=sales.sport)

# Forward 방식
fwd.model = step(min.model, direction='forward',
                 scope=LOG_SALES ~ ITEM_CNT+PRICE+MAXTEMP+SALEDAY+RAIN_DAY+HOLIDAY+
                     DUSTDAY+MONTH+Quart, trace=T)
fwd.model  # AIC=-256.99: MAXTEMP + SALEDAY + Quart + PRICE + HOLIDAY + DUSTDAY


# Backward 방식
back.model = step(max.model, direction='backward', trace=T)
back.model  # AIC=-258.22: PRICE + MAXTEMP + SALEDAY + RAIN_DAY + HOLIDAY + DUSTDAY + MONTH

# Both 방식
both.model = step(max.model, direction='both', trace=T)
both.model  # AIC=-258.22: PRICE + MAXTEMP + SALEDAY + RAIN_DAY + HOLIDAY + DUSTDAY + MONTH
summary(both.model)  # Backward 방식과 동일함

# Forward방식과 Backward방식의 비교
anova(fwd.model, back.model)  # p-value=0.096
AIC(fwd.model, back.model)  # Backward가 살짝 작다


## 2) All Subsets Regression 방법:
install.packages('leaps')
library(leaps)
leaps = regsubsets(LOG_SALES ~ ITEM_CNT+PRICE+MAXTEMP+SALEDAY+RAIN_DAY+HOLIDAY+
                       DUSTDAY+MONTH+Quart, data=sales.sport, nbest=9)
summary(leaps)
plot(leaps)  # default scale='bic'
bic.model=lm(LOG_SALES ~ PRICE + MAXTEMP + SALEDAY)
    
plot(leaps, scale='Cp')   # MAllow's Cp

plot(leaps, scale='adjr2')   # 수정결정계수




#########################################
### STEP 6. 잔차분석
#########################################
par(mfrow=c(2,2))
plot(both.model)
# 선형성, 등분산성은 통과, 정규성은 없어보인다
shapiro.test(both.model$residuals) # 겨우 통과(p-value=0.0519)
par(mfrow=c(1,1))


#########################################
### STEP 7. MSE 계산
#########################################
# 1) Log(MSE)
y_log = cbind(sales.sport$LOG_SALES, predict(both.model))

log.MSE = mean(both.model$residuals^2)
log.MSE  # 0.01070674


# 2) Log(MSE)
y = cbind(exp(sales.sport$LOG_SALES), exp(predict(both.model)), resid=exp(sales.sport$LOG_SALES)-exp(both.model$fitted.values))

MSE = mean(y[,3]^2)
MSE  # 1.24972e+11


#########################################
### STEP 8. 설명변수의 상대적 중요도 분석
#########################################
relweights = function(fit,...){
    R = cor(fit$model)
    nvar = ncol(R)
    rxx = R[2:nvar, 2:nvar]
    rxy = R[2:nvar, 1]
    svd = eigen(rxx)
    evec = svd$vectors
    ev = svd$values
    delta = diag(sqrt(ev))
    lambda = evec %*% delta %*% t(evec)
    lambdasq = lambda ^ 2
    beta = solve(lambda) %*% rxy
    rsquare = colSums(beta ^ 2)
    rawwgt = lambdasq %*% beta ^ 2
    import = (rawwgt/rsquare) * 100
    import = as.data.frame(import)
    row.names(import) = names(fit$model[2:nvar])
    names(import) = "Weights"
    import = import[order(import), 1, drop=FALSE]
    dotchart(import$Weights, labels=row.names(import),
             xlab="% of R-Square", pch=19,
             main="Relative Importance of Predictor Variables",
             sub=paste("Total R-Sqaure=", round(rsquare, digits=3)),
             ...)
    return(import)
}

result = relweights(both.model, col='darkblue')
result


# ggplot2을 사용하여 변수의 상대적 중요도를 시각화
library(ggplot2)
plotRelWeights = function(fit){
    data = relweights(fit)
    data$Predictors = rownames(data)
    p = ggplot(data=data, aes(x=reorder(Predictors, Weights), y=Weights,
                              fill=Predictors)) +
        geom_bar(stat='identity', width=0.5) +
        ggtitle('Relative Importance of Predictor Variables') +
        ylab(paste0('% of R-Square \n(Total R-Squar=', attr(data, 'R-Square'),')')) +
        geom_text(aes(y=Weights-0.1, label=paste(round(Weights,1), '%')), hjust=1) +
        guides(fill=FALSE) +
        coord_flip()
    p
}

plotRelWeights(both.model)

###################
### RAIN_DAY를 포함시켜봤더니 23.5% 나옴
model_rain=lm(LOG_SALES ~ PRICE+MAXTEMP+HOLIDAY+
                  DUSTDAY+MONTH+RAIN_DAY, data=sales.sport)
summary(model_rain)
plotRelWeights(model_rain)

# RAIN_DAY를 제외하면 안된다

### ITEM_CNT를 포함시켜봤더니 22.9% 나옴
model_item=lm(LOG_SALES ~ PRICE+MAXTEMP+HOLIDAY+
                  DUSTDAY+MONTH+ITEM_CNT, data=sales.sport)
summary(model_item)
plotRelWeights(model_item)
# ITEM_CNT를 제외하면 안된다, 결정계수는 89.7%

### Quart를 포함시켜봤더니 1.7% 나옴
model_quart=lm(LOG_SALES ~ PRICE+MAXTEMP+HOLIDAY+
                   DUSTDAY+MONTH+Quart, data=sales.sport)
summary(model_quart)
plotRelWeights(model_quart)
# Quart는 제외하는 것이 맞다
###################



