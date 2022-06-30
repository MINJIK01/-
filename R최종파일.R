###########
### EDA 
###########

##### 특정품목 조달 내역 EDA #####

# 0. 사전 작업

## 0.1 라이브러리 및 파일 불러오기 
library(tidyverse)    # 데이터 전처리
library(data.table)   # 데이터 전처리
library(lubridate)    # 시간 데이터 전처리
'%notin%' = Negate('%in%') # 함수 설정

## 0.2 워킹 디렉토리 설정
CURRENT_WORKING_DIR <- dirname(rstudioapi::getActiveDocumentContext()$path)
CURRENT_WORKING_DIR
setwd(CURRENT_WORKING_DIR)


# 1. 데이터 불러오기
data0 = fread('data/특정품목 조달 내역_2010.csv',encoding='UTF-8')
data1 = fread('data/특정품목 조달 내역_2011.csv',encoding='UTF-8')
data2 = fread('data/특정품목 조달 내역_2012.csv',encoding='UTF-8')
data3 = fread('data/특정품목 조달 내역_2013.csv',encoding='UTF-8')
data4 = fread('data/특정품목 조달 내역_2014.csv',encoding='UTF-8')
data5 = fread('data/특정품목 조달 내역_2015.csv',encoding='UTF-8')
data6 = fread('data/특정품목 조달 내역_2016.csv',encoding='UTF-8')
data7 = fread('data/특정품목 조달 내역_2017.csv',encoding='UTF-8')
data8 = fread('data/특정품목 조달 내역_2018.csv',encoding='UTF-8')
data9 = fread('data/특정품목 조달 내역_2019.csv',encoding='UTF-8')
data10 = fread('data/특정품목 조달 내역_2020.csv',encoding='UTF-8')
data11 = fread('data/특정품목 조달 내역_2021.csv',encoding='UTF-8')

# 1.1 데이터 전처리
## 계약번호_납품요구의 데이터 형식에 따라서 따로 저장
data06 = bind_rows(data0,data1,data2,data3,data4,data5,data6) 
data711 = bind_rows(data7,data8,data9,data10,data11) 

## 데이터 처리 편의성을 위해 변수명 변경
colnames(data06)[4] = '계약일자_납품요구'
colnames(data711)[4] = '계약일자_납품요구'
colnames(data06)[5] = '계약번호_납품요구'
colnames(data711)[5] = '계약번호_납품요구'
colnames(data06)[31] = '최초계약일자_납품요구'
colnames(data711)[31] = '최초계약일자_납품요구'

## 계약번호_납품요구 행의 데이터 형식을 character로 통일 
data06$계약번호_납품요구 = as.character(data06$계약번호_납품요구)
data711$계약번호_납품요구 = as.character(data711$계약번호_납품요구)

## 하나의 데이터셋 생성
data = bind_rows(data06,data711)

## 품명 중 마스크또는보조용품 / 기념품 / 장식품 제외
data <- data %>% filter(품명 != "마스크또는보조용품") %>% filter(품명 != "기념품") %>% filter(품명 != "장식품")

## 품명/세부품명 중 '활성탄(\u6d3b\u6027\u70ad)'을 '활성탄'으로 변경
data$품명<-ifelse(data$품명=="활성탄(\u6d3b\u6027\u70ad)","활성탄",data$품명)
data$세부품명<-ifelse(data$세부품명=="활성탄(\u6d3b\u6027\u70ad)","활성탄",data$세부품명)

## 수요기관 지역의 시도를 나타내는 파생변수 생성
Split <- strsplit(as.character(data$수요기관지역명), " ", fixed = TRUE) 
data$수요기관지역명_sido = sapply(Split, "[", 1) 

## 최종 데이터셋 생성
data <- data %>% select(조달구분, 계약구분, 계약납품구분, 계약일자_납품요구, 수요기관명, 수요기관구분, 수요기관지역명, 품명, 금액, 업체명, 업체기업구분명, 납품기한일자, 계약일자_납품요구_year, 계약일자_납품요구_yearmonth,납품기한일자, 납품기한일자_year,납품기한일자_yearmonth,계약방법)


# 2. 데이터 시각화

# 2.0 데이터 조회
## 데이터 형식 및 크기 
data %>% str() # 1443행 * 38열 

## 데이터 NA값 조회
data %>% is.na() %>% colSums()

## 데이터 칼럼별 고유값 조회
data %>% group_by(계약납품구분) %>% summarize(count=n())

## 1) 수요기관 소속지역별 계약건수
p71 <- data %>% group_by(수요기관지역명_sido) %>% summarise(count = n()) 
p71  %>%
  mutate(수요기관지역명_sido = reorder(수요기관지역명_sido, count)) %>% 
  ggplot(aes(x=수요기관지역명_sido,y=count,fill=수요기관지역명_sido,order=count)) +
  geom_bar(stat = "identity",alpha=0.6) +
  theme_set(theme_light(base_family="NanumGothic"))+
  theme(legend.title = element_blank()) +
  xlab("수요기관 소속지역")+
  ylab("계약건수")+
  ggtitle("수요기관 소속지역별 계약건수") +
  coord_flip()+
  theme(legend.position="none")






##### 물품 조달요청 상세내역 EDA #####

# 0. 사전 작업

## 0.1 라이브러리 및 파일 불러오기 
library(dplyr)

## 0.2 워킹 디렉토리 설정
setwd("~/Desktop/조달청 공모전/데이터")


# 1. 데이터 불러오기
data1=fread('물품_조달요청_상세내역2019.csv')
data2=fread('물품_조달요청_상세내역2020.csv')
data3=fread('물품_조달요청_상세내역2021.csv')

sample_data=rbind.data.frame(data1,data2,data3)
sample_data
sample_data %>% str()

# 1.1 데이터 전처리

sample_data<-sample_data %>% 
  select(접수일자,수요기관구분, 품명, 금액, 접수지청명, 수량, 단위) %>% 
  mutate(접수일자=substr(접수일자,1,4))

## 비철금속만 추출
data<-sample_data %>% filter(품명=="활성탄(活性炭)"|품명=="활성탄장비"|품명=="알루미늄프로파일"|품명=="알루미늄"|품명=="주석광석"|품명=="니켈카드뮴전자")
data %>% str()


# 2. 데이터 시각화

## NA값 확인
data %>% is.na() %>% colSums()

## 시각화
## 1) 연도별 총 계약건수
c1<- data %>%  group_by(접수일자) %>% summarize(count=n())
c1
c1%>%
  ggplot(aes(x=접수일자,y=count,fill=접수일자)) +
  geom_bar(stat = "identity",alpha=0.6) +
  xlab("접수일자")+
  ylab("계약건수")+
  ggtitle("연도별 계약건수") +
  theme_set(theme_light(base_family="NanumGothic"))+
  theme(legend.title = element_blank()) 

xlab("수요기관")+
  ylab("계약건수")+
  ggtitle("수요기관별 계약건수") +
  theme_set(theme_light(base_family="NanumGothic"))+
  theme(legend.title = element_blank()) 

#=> 해마다 계약건수가 많아지고 있음


## 2) 수요기관별 계약건수
c2<-data %>%  group_by(수요기관구분) %>% summarize(count=n());c2
c2%>%
  ggplot(aes(x=reorder(수요기관구분,-count),y=count,fill=수요기관구분)) +
  geom_bar(stat = "identity",alpha=0.6) +
  xlab("수요기관")+
  ylab("계약건수")+
  ggtitle("수요기관별 계약건수") +
  theme_set(theme_light(base_family="NanumGothic"))+
  theme(legend.title = element_blank()) 

#=> 수요기관별 계약건수는 
#지자체> 공기업> 기타기관> 국가기관> 준정부기관> 지방공기업 순으로 많았음


## 3) 접수지청별 계약건수
c3<- data %>%  group_by(접수지청명) %>% summarize(count=n()) %>%  mutate(접수지청명=substr(접수지청명,1,2))
c3$접수지청명[10]<-"본청"
c3
c3 %>% 
  ggplot(aes(x=reorder(접수지청명,-count),y=count,fill=접수지청명)) +
  geom_bar(stat = "identity",alpha=0.6) +
  xlab("접수지청명")+
  ylab("계약건수")+
  ggtitle("접수지청별 계약건수") +
  theme_set(theme_light(base_family="NanumGothic"))+
  theme(legend.title = element_blank()) 

#=> 접수지청별 계약건수는
#경남>인천>부산>서울>대구>본청>대전>전북,제주,광주 순으로 많았음


## 4) 2019 접수지청별 계약건수
c3<- data %>%  group_by(접수지청명) %>% summarize(count=n()) %>%  mutate(접수지청명=substr(접수지청명,1,2))
c3
c2019<-data %>%  filter(접수일자=='2019') %>% 
  group_by(접수지청명) %>% summarize(count=n()) %>%  mutate(접수지청명=substr(접수지청명,1,2)) 
c2019$접수지청명[7]<-"본청"

#시각화
c2019 %>% 
  ggplot(aes(x=reorder(접수지청명,-count),y=count,fill=접수지청명)) +
  geom_bar(stat = "identity",alpha=0.6) +
  xlab("접수지청명")+
  ylab("계약건수")+
  ggtitle("2019 접수지청별 계약건수") +
  theme_set(theme_light(base_family="NanumGothic"))+
  theme(legend.title = element_blank()) 

#2019년 접수지청별 계약건수는 경남>서울>부산>인천>대구>대전>본청 순으로 많았음. 광주, 전북, 제주, 충북은 없었음
data<-mutate(접수지청명=substr(접수지청명,1,2))


## 5) 2020 접수지청별 계약건수
c2020<-data %>%  filter(접수일자=='2020') %>% 
  group_by(접수지청명) %>% summarize(count=n()) %>% 
  mutate(접수지청명=substr(접수지청명,1,2)) 
c2020$접수지청명[8]<-"본청"
c2020 %>% 
  ggplot(aes(x=reorder(접수지청명,-count),y=count,fill=접수지청명)) +
  geom_bar(stat = "identity",alpha=0.6) +
  xlab("접수지청명")+
  ylab("계약건수")+
  ggtitle("2020 접수지청별 계약건수") +
  theme_set(theme_light(base_family="NanumGothic"))+
  theme(legend.title = element_blank()) 

#2020년 접수지청별 계약건수는 경남>인천>대구>대전>부산>인천>전북>본청 순으로 많았음. 광주, 제주, 충북은 없었음


## 6) 2021 접수지청별 계약건수
c2021<-data %>%  filter(접수일자=='2021') %>% 
  group_by(접수지청명) %>% summarize(count=n()) %>%  
  mutate(접수지청명=substr(접수지청명,1,2)) 
c2021$접수지청명[8]<-"본청"
c2021 %>% 
  ggplot(aes(x=reorder(접수지청명,-count),y=count,fill=접수지청명)) +
  geom_bar(stat = "identity",alpha=0.6) +
  xlab("접수지청명")+
  ylab("계약건수")+
  ggtitle("2021 접수지청별 계약건수") +
  theme_set(theme_light(base_family="NanumGothic"))+
  theme(legend.title = element_blank()) 

#=> 2021년 접수지청별 계약건수는 경남>부산>대구>서울>광주>인천>제주>조달청>충북 순으로 많았음. 대전, 전북은 없었음


## 7) 품목 데이터 

#품목별 계약건수
c4<-data %>%  group_by(품명) %>% 
  summarize(계약건수=n(),
                거래가격=sum(금액) ,
                거래량=sum(수량),)
c4


## 8) 품목별 계약건수
c4 %>%
  ggplot(aes(x=reorder(품명,-계약건수),y=계약건수,fill=품명)) +
  geom_bar(stat = "identity",alpha=0.6) +
  geom_text(aes(label=계약건수), position=position_stack(vjust=0.5),size=3 )+
  xlab("품명")+
  ylab("계약건수")+
  ggtitle("품목별 계약건수") +
  theme_set(theme_light(base_family="NanumGothic"))+
  theme(legend.title = element_blank()) 


## 9) 품목별 거래가격
c4 %>%  mutate(거래가격=거래가격/10000) %>% 
  ggplot(aes(x=reorder(품명,-거래가격),y=거래가격,fill=품명)) +
  geom_bar(stat = "identity",alpha=0.6) +
  xlab("품명")+
  ylab("총 거래가격")+
  ggtitle("품목별 총 거래가격") +
  theme_set(theme_light(base_family="NanumGothic"))+
  theme(legend.title = element_blank()) 


## 10) 품목별 총 거래량
c4 %>%  
  ggplot(aes(x=reorder(품명,-거래량),y=거래량,fill=품명)) +
  geom_bar(stat = "identity",alpha=0.6) +
  xlab("품명")+
  ylab("총 거래량")+
  ggtitle("품목별 총 거래량") +
  theme_set(theme_light(base_family="NanumGothic"))+
  theme(legend.title = element_blank())





### 품목별 내륙 물동량 EDA ###

# 0. 사전 작업

## 0.1 워킹 디렉토리 설정
CURRENT_WORKING_DIR <- dirname(rstudioapi::getActiveDocumentContext()$path)
CURRENT_WORKING_DIR
setwd(CURRENT_WORKING_DIR)


# 1. 데이터 불러오기 
data <- fread("품목별 내륙 물동량(2013-2019) 수정.csv") # 데이터 불러오기

# 1.1 데이터 조회
data %>% select(년도) %>% unique() #년도 조회
data %>% select(품목명) %>% unique() %>% arrange(품목명) #품목 조회
#05석탄광물 06석회석광물 09비금속광물 21비금속광물제품 22제1차금속제품 

# 1.2 데이터 전처리
## 열이름 변경
colnames(data) <- c("년도","품목명","총물동량","총물동량_순위","총물동량_비율","권역간물동량_출발권역","권역간물동량_도착권역","물동량","물동량_순위","물동량_비율")
data 

## 콤마 제거 및 데이터 속성 변경
data$총물동량 <- gsub(",","",data$총물동량)
data$물동량 <- gsub(",","",data$물동량)
data$총물동량 <- as.numeric(data$총물동량)
data$물동량 <- as.numeric(data$물동량)
data$년도 <- as.character(data$년도)
data$총물동량_순위 <- as.character(data$총물동량_순위)
data$총물동량_비율 <- as.character(data$총물동량_비율)
data$물동량_순위 <- as.character(data$물동량_순위)
data$물동량_비율 <- as.character(data$물동량_비율)


# 2. 데이터 시각화

## 1) 출발권역별 총 물동량
data %>% filter(권역간물동량_출발권역 !="") %>% group_by(권역간물동량_출발권역) %>% summarise(sum = sum(물동량,na.rm=T)) %>% 
  ggplot()+
  geom_bar(aes(x=reorder(권역간물동량_출발권역,sum), y=sum,fill=권역간물동량_출발권역),alpha=0.6,stat="identity") +
  theme(legend.position="none", axis.text.x = element_text(angle = 50, hjust =1))+
  xlab("출발권역")+
  ylab("총 물동량")+
  ggtitle("출발권역별 총 물동량")+
  geom_text(aes(x=reorder(권역간물동량_출발권역,sum), y=sum, label=sum), vjust=-0.3, size=3.5)


## 2) 비금속 광물제품 물동량
data %>% select(품목명) %>% unique()
data %>% filter(권역간물동량_출발권역 !="") %>% filter(품목명 == "09비금속광물") %>% group_by(권역간물동량_출발권역) %>% summarise(sum = sum(물동량,na.rm=T)) %>% 
  ggplot()+
  geom_bar(aes(x=reorder(권역간물동량_출발권역,sum), y=sum,fill=권역간물동량_출발권역),alpha=0.6,stat="identity") +
  theme(legend.position="none", axis.text.x = element_text(angle = 50, hjust =1))+
  xlab("출발권역")+
  ylab("총 물동량")+
  ggtitle("출발권역별 비금속광물제품 물동량")+
  geom_text(aes(x=reorder(권역간물동량_출발권역,sum), y=sum, label=sum), vjust=-0.3, size=3.5)


## 3) 비금속 광물 물동량
data %>% filter(권역간물동량_출발권역 !="") %>% filter(품목명 == "21비금속 광물제품") %>% group_by(권역간물동량_출발권역) %>% summarise(sum = sum(물동량,na.rm=T)) %>% 
  ggplot()+
  geom_bar(aes(x=reorder(권역간물동량_출발권역,sum), y=sum,fill=권역간물동량_출발권역),alpha=0.6,stat="identity") +
  theme(legend.position="none", axis.text.x = element_text(angle = 50, hjust =1))+
  xlab("출발권역")+
  ylab("총 물동량")+
  ggtitle("출발권역별 비금속광물제품 물동량")+
  geom_text(aes(x=reorder(권역간물동량_출발권역,sum), y=sum, label=sum), vjust=-0.3, size=3.5)





### 판매공고내역 EDA ###

# 0. 사전 작업

## 0.1 워킹 디렉토리 설정
setwd('C:/Users/user/Desktop/조달청 공모전/EDA')
getwd()


# 1. 데이터 불러오기 
data <- fread('data.csv',data.table = FALSE, encoding="UTF-8")

# 1.1 데이터 전처리
## 불필요한 세부품목 삭제
data <- data %>% filter(세부품명 != "보건용마스크") %>% filter(세부품명 != "기념품") %>% filter(세부품명 != "장도") %>%  filter(세부품명 != "선물세트") %>% filter(세부품명 != "기타무기금속염") %>% filter(세부품명 != "비말차단용마스크") %>% filter(세부품명 != "복각품") %>%  filter(세부품명 != "선물세트") %>% filter(세부품명 != "기타무기금속염") %>% filter(세부품명 != "비말차단용마스크")


# 2. 데이터 시각화

## 1) 품목별 판매현황
things <- data['세부품명'] %>% group_by(세부품명)  %>% summarise(count = n())

things%>% 
  ggplot(aes(x= reorder(세부품명, -count), y = count, fill = 세부품명)) +
  geom_bar(stat= "identity",alpha=0.6)+
  theme_set(theme_light(base_family="NanumGothic"))+
  labs(title="판매품목별 판매횟수", x="품목", y="판매횟수") +
  theme(legend.position="none", axis.text.x = element_text(angle = 50, hjust =1))






###########
### 다중회귀, 클러스터링
###########

##### 다중회귀분석 #####

# 0. 사전 작업

## 0.1 라이브러리 및 파일 불러오기 
library(gvlma)
library(car)
library(corrplot)
library(lmtest)

## 0.2 워킹 디렉토리 설정
setwd("~/Desktop/조달청 공모전/데이터")


# 1. 데이터 불러오기 
data5= fread('다중회귀.csv')
data6=fread('경남 회귀.csv')
demand <- fread('수요지수.csv',data.table = FALSE, encoding="UTF-8")

# 2. 전국 데이터에 대한 모델 생성
model1<-lm(공급지수~.,data5)

# 2.1 모델 분석

## 모델 요약
summary(model1)

## 상관계수 plot
par(family='AppleGothic')
corrplot(cor(data5), method='number')


# 3. 경상남도 데이터에 대한 모델 생성
model=lm(제조업_사업체수~물동량+풍수해_취약도+톨게이트_개수,data6)

# 3.1 모델 분석

## 모델 요약
summary(model)
#=> p-value=0.0001207로 통계적으로 유의하다.

## 상관계수 plot
corrplot(cor(data6), method='number')

## 잔차플랏
par(mfrow=c(2,2))
plot(model)

## Durbin-Watson 검정
dwtest(demand$사업체수 ~ demand$물동량 + demand$풍수해 + demand$톨게이트)
#=> DW값이 2에 가까워 자기상관을 무시한다.

## 다중공선성 검정
vif(model)

## 상관관계 
par(mfrow=c(1,1))
cor=cor(data[, 2:5])
par(family='AppleGothic')
corrplot(cor,method='shade',addCoef.col='black')

## 상관관계 plot
hep.data=data[,-1]
par(family='AppleGothic')
pairs(hep.data, panel=panel.smooth)





### 수요지수 생성 ###

# 0. 사전작업

# 0.1 라이브러리 불러오기
library(lavaan)


# 가중치산출

# 1. PCA 주성분분석

## 표준점수 구하기
round(scale(hep.data),2)

## 상관행렬
round(cor(hep.data),2)
var(data[,-1])

## 주성분 분석하기
hep.data.pca<-prcomp(hep.data,scale=TRUE)
#=> standard deviations: 는 표준편차를 의미하는것으로 각 변수가 얼마나 많은 부분을 차지하고 있는지를 알려준다.
#=> scale. = T는 수치간 표준화를 지정하는 것으로 각 변수당 단위가 다르기 때문에 표준화가 필요하다.
hep.data.pca

## 주성분 분석에 대한 결과 요약 설명
summary(hep.data.pca)

#=> Standard deviation : 표준편차
#=> Propertion of Variance : 분산비율, 각 주성분의 차지하는 비율을 말하며 클 수록 영향도가 그만큼 높다는 의미임
#=> Cumulative Proportion : 분산의 누적 합계

#=> 첫번째 주성분(PC1)의 누적기여율은 0.4677, 즉 46%에 해당한다
#=> PC1이 분석대상의 데이터가 가지고 있던 정보가 PC1 주성분에 집약되어 있는 크기를 설명함

## scree plot
screeplot(hep.data.pca, type="lines",pch=1,main="scree plot")
par(family='AppleGothic')
biplot(hep.data.pca)


# 2. 요인분석 (factor analysis)
data.factanal<-factanal(hep.data,
                        factors=1,
                        rotation='varimax',
                        scores='regression')

## loading에 물동량, 풍수해_취약도의 숫자가 비어있다. 아래처럼 cutoff를 조정하여 해결한다
print(data.factanal,cutoff=0)

#=> 요인의 개수는 1개면 충분하다고 나왔는데, 문제는 p-value가 0.873으로 엄청 높아서 통계적으로 유의하지 않음

# 2.1 시각화, factor scores plotting
par(family='AppleGothic')
plot(data.factanal$scores, main='Biplot of the first 1 factor')
text(data.factanal$scores[,1], labels=data$시군구,cex=0.7,pos=3, col='blue')
points(data.factanal$loadings,pch=10,col='red')


# 3. 회귀분석(회귀계수)

# 3.1 모델 생성
rg_model <- lm(사업체수~ ., data)

# 3.2 모델 분석
summary(rg_model)
#=> 전체 모델에 대한 p-value가 유의한 수준으로 나옴

## 모델 회귀 계수 추출
rg_model$coefficients


# 4. 경로분석(경로계수)

# 4.1 모델 생성
model_ana <- '사업체수 ~ 물동량 + 풍수해 + 톨게이트'
fit_analysis <- sem(model_ana, data = data)

# 4.2 모델 분석
summary(fit_analysis)

# 4.3 경로 시각화
diagram<-semPlot::semPaths(fit_analysis,
                           whatLabels="std", intercepts=FALSE, style="lisrel",
                           nCharNodes=0, 
                           nCharEdges=0,
                           rotation = 2,
                           edge.label.position=.43,
                           edge.label.cex = 1.2,
                           curveAdjacent = TRUE,title=TRUE, layout="tree2",curvePivot=TRUE)





##### 클러스터링 #####

# 0. 사전 작업

## 0.1 라이브러리 및 파일 불러오기
library(dbscan)


# 1. 데이터 불러오기
df <- fread('cluster.csv',data.table = FALSE, encoding="UTF-8")


# 2. DBscan 최적 eps 산출

# 2.1 데이터 전처리
df = df[,2:3]

# 2.2 sorted k-dist plot
dbscan::kNNdistplot(df, k=5)

## threshold 시각화
abline(h = 1100, lty = 2)