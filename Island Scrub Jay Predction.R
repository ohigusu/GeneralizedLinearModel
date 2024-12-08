load("/Users/k/Desktop/isj.Rdata")
str(isj)
###변수
#x,y : locations
#elev,forest,chap:3 habitat(elevation, percent forest cover, percent chaparral cover)
#isj: 1= presence, 0=absence of island scrub jay -> response variable
#To understand what type of landscape the island scrub jay prefers and to predcit island scrub jay occupancy accross the island.
fit <- glm(isj~.,family=binomial,data=isj)
summary(fit)

###na값 확인
na_counts <- colSums(is.na(isj))
na_counts_dat<- data.frame(Column = names(na_counts), NA_Count = na_counts)
print(na_counts_dat)

###시각화
library(ggplot2)
isj_nona<- isj[which(!is.na(isj[,1])),] #반응변수의 값이 NA가 아닌 데이터들 저장
isj_nona<- isj_nona[which(!is.na(isj_nona[,4])),]
ggplot(isj_nona, aes(x = factor(isj), fill = factor(isj))) +
  geom_bar() +
  labs(x = "Presence and Absence of Island Scrub Jay (0 = Absent, 1 = Present)",
       y = "Count",
       fill = "isj") +
  scale_fill_manual(values = c("0" = "orange", "1" = "skyblue")) +  # 색상 지정
  theme_minimal()

ggplot(isj_nona, aes(x = x, y = y, color = factor(isj))) +
  geom_point(alpha = 0.6, size = 3) +
  labs(x = "X",
       y = "Y",
       color = "Island Scrub Jay\nPresence (isj)") +
  theme_minimal()
ggplot(isj_nona, aes(x = factor(isj), y = elev, fill = factor(isj))) +
  geom_boxplot() +
  labs(x = "Island Scrub Jay (0 = Absent, 1 = Present)",
       y = "Elevation",
       fill = "isj") +
  scale_fill_manual(values = c("0" = "orange", "1" = "skyblue")) +  # 색상 지정
  theme_minimal()
ggplot(isj_nona, aes(x = factor(isj), y = forest, fill = factor(isj))) +
  geom_boxplot() +
  labs(x = "Island Scrub Jay (0 = Absent, 1 = Present)",
       y = "Forest Cover",
       fill = "isj") +
  scale_fill_manual(values = c("0" = "orange", "1" = "skyblue")) +  # 색상 지정
  theme_minimal()
ggplot(isj_nona, aes(x = factor(isj), y = chap, fill = factor(isj))) +
  geom_boxplot() +
  labs(x = "Island Scrub Jay (0 = Absent, 1 = Present)",
       y = "Chaparral Cover",
       fill = "isj") +
  scale_fill_manual(values = c("0" = "orange", "1" = "skyblue")) +  # 색상 지정
  theme_minimal()
#####################################################################
###step1. 변수 조합
library(dplyr)
data1 <- isj %>% mutate("x2"= x^2,
                        "y2"=y^2,
                        "elev2"=elev^2,
                        "forest2"=forest^2, 
                        "chap2"=chap^2,
                        "x.y"=x*y,
                        "x.elev"=x*elev,
                        "x.forest"=x*forest,
                        "x.chap"=x*chap,
                        "y.elev"=y*elev,
                        "y.forest"=y*forest,
                        "y.chap"=y*chap,
                        "elev.forest"=elev*forest,
                        "elev.chap"=elev*chap,
                        "forest.chap"=forest*chap)
#스케일링
isj_scaled <- data1
isj_scaled[, setdiff(names(data1), "isj")] <- scale(data1[, setdiff(names(data1), "isj")], 
                                                    center = TRUE, 
                                                    scale = TRUE)
isj_scaled[,'origincalX'] <- isj[,'x']
isj_scaled[,'origincalY'] <- isj[,'y']
#모든 열에 NA가 없는 데이터들을 따로 저장한다.
isj.complete<- isj_scaled[which(!is.na(isj_scaled[,1])),] #반응변수의 값이 NA가 아닌 데이터들 저장
isj.complete <- isj.complete[which(!is.na(isj.complete[,4])),] #그 중에서 4,5,6열 데이터 중 NA가 있는 데이터는 제거

#반응변수의 값만 NA이고 나머지 열은 데이터가 존재하는 데이터(행)들을 따로 저장한다. 
isj.na <- isj_scaled[which(is.na(isj_scaled[,1])),] #반응변수의 값이 NA인 데이터들 저장
isj.pred <- isj.na[which(!is.na(isj.na[,4])),] #그 중에서 4,5,6열 데이터 중 NA가 있는 데이터는 제거한다.
nrow(isj.pred)
#####################################################################
###step2.data 나누기 
#모델을 비교하기 위해 isj.complete에 저장된 데이터들을 train data와 test data를 나누어 train data활용하여 모델을 적합하고 test data의 결과로 모델들을 비교한다.
library(rsample)#data spliting
set.seed(123)
isj_split <- initial_split(isj.complete, prop = .7)#train-set:0.7, test-set:0.3
isj_train <- training(isj_split)
isj_test  <- testing(isj_split)

#classification threshold 정하기
nrow(isj.complete) #사용할 데이터의 총 개수
nrow(isj.complete[isj.complete[,1]==1,]) #isj=1인 데이터의 개수
#즉, 사용할 데이터 303개 중, isj=1인 데이터의 개수는 38로 데이터가 편중되어 있음을 알 수 있다.
#island scrub jay가 있는 곳을 더 많이 찾아내고자 하므로 threshold를 0.5가 아닌  0.3으로 조정한다.
# 이에 따라 island scrub jay가 없는 곳을 있는 곳이라고 판단할 오류는 더 커지겠지만, 놓치고 있던 장소를 발견함으로써 얻는 가치는 이를 감수할 수 있을 것이라고 생각하므로 모델이 positive class를 더 많이 예측할 수 있도록 한다.
#이때 반응변수가 1 또는 0이므로 binary regression이므로 우선 어떤 link function을 사용할지 정한다.

isj_train_use <- isj_train[, !names(isj_train) %in% c("origincalX", "origincalY")]
isj_test_use <- isj_test[, !names(isj_test) %in% c("origincalX", "origincalY")]

#####################################################################
###step3-1.
#제거 또는 선택할 변수들의 인덱스
7:ncol(isj_train_use) 
#idx.result에 for문에서 사용할 변수명을 저장
idx.result <- list() 
#error.result에 적합한 모델의 오분류율 저장
error.result <- data.frame("error"= 0) 

for(i in 1:15){ 
  combination <- combn(15,i) #15개의 변수들 중 i개의 변수들을 선택한다.
  count.col = ncol(combination) #combination의 결과값의 개수
  idx <- matrix(0,nrow = count.col,ncol=i) #제거할 변수의 인덱스를 저장하기 위해 idx 생성한다.
  
  for(j in 1:count.col){
    #제거할 변수의 인덱스를 idx에 저장
    idx[j,]<-combination[,j]
    
    #제거할 변수들을 제외한 train data와 test data를 생성한다.
    dat.train <- isj_train_use[,-(idx[j,]+6)]
    dat.test <- isj_test_use[,-(idx[j,]+6)]
    #모델에 사용할 변수의 이름 저장 
    idx.result[[j]] <- colnames(dat.train)
    
    #logit regression 
    #적합
    fit.logit <- glm(isj~.,family=binomial,data=dat.train)
    #예측
    pred.logit<- predict(fit.logit,newdata = dat.test,type="response")
    #분류 테이블 만든다.
    tab.logit <- table(ifelse(pred.logit>=0.3,1,0),isj_test$isj) 
    #오분류율 구한다.
    if(nrow(tab.logit)==1){
      error = tab.logit[1,2]/sum(tab.logit)
    }
    else{
      error = (tab.logit[1,2]+tab.logit[2,1])/sum(tab.logit)
    }
    error.result[j]<-error
  }
}

#가장 작은 오분류율 
error.result[which.min(error.result)]

#가장 작은 오분류율을 가지고 있는 모델의 변수명
result.variable = idx.result[[which.min(error.result)]]
result.variable

#결과를 통해 모델 적합한다.
#train데이터와 test데이터를 모두 합한 isj.compare데이터를 사용한다.
fit.logit <- glm(isj~.,family=binomial,data=isj.complete[,result.variable])
summary(fit.logit)
#####################################################################
###step4.
fit.logit1 <- glm(isj~x+y+elev+forest+chap,family=binomial,data=isj.complete)
summary(fit.logit1)
#####################################################################
###step3-2.
library(MASS)

model_full <- glm(isj ~ ., data = dat.train, family = binomial)  # 모든 변수 포함
model_null <- glm(isj ~ 1, data = dat.train, family = binomial) # 절편만 포함

# 단계적 선택 (전진 선택 방식)
stepwise_model <- stepAIC(model_null, scope = list(lower = model_null, upper = model_full), 
                          direction = "forward", trace = TRUE)

# 최종 모델 요약
summary(stepwise_model)
###step4.
fit.logit2 <- glm(isj~y,family=binomial,data=isj.complete)
summary(fit.logit2)

fit.logit3 <- glm(isj~y,family=binomial,data=dat.train)
pred.logit<- predict(fit.logit4,newdata = dat.test,type="response")
#분류 테이블 만든다.
tab.logit <- table(ifelse(pred.logit>=0.3,1,0),isj_test$isj) 
13/(78+13)

#####################################################################
###step5
#반응변수의 값이 NA가 아닌 데이터들(isj.complete)에서의 예측확률
pred.logit_complete <- predict(fit.logit,type="response")
ox_complete <- ifelse(pred.logit_complete>=0.3,1,4) #있으면 = 1, 없으면 4로 저장
isj.complete <- isj.complete %>% mutate("pred_prob"=pred.logit_complete,
                                        "ox"= ox_complete)
###다음은 반응변수의 값이 NA가 아닌 데이터들(isj.complete)에서의 예측확률을 나타낸 그래프들이다.
#1)Elevation
plot(isj[,2:3],type="n",main="Elevation",asp=1)
na.idx = which(is.na(isj$elev))
elev.star = isj$elev[-na.idx]
norm.elev = (elev.star-min(elev.star))/(max(elev.star)-min(elev.star))
points(isj[-na.idx,2:3],pch=20,col=grey(1-norm.elev))
points(isj.complete[,c(7,23)],pch=ox_complete,col="red")
for(i in 1:nrow(isj.complete)){
  text(isj.complete[i,7],isj.complete[i,23],round(isj.complete$pred_prob[i],3),pos=1,cex=0.35,font=4)
}
colnames(isj.complete)
#island scrub jay 존재하는 곳에만 o 표시를 하겠다.
#island scrub jay 존재하는 곳에 대한 x,y,예측확률을 따로 저장한다. 
only.o_complete <- isj.complete[isj.complete$ox==1,c(7,23,24)] 
plot(isj[,2:3],type="n",main="Elevation",asp=1)
points(isj[-na.idx,2:3],pch=20,col=grey(1-norm.elev))
points(isj.complete[isj.complete$ox==1,c(7,23)],pch=1,col="red")
for(i in 1:nrow(isj.complete)){
  text(only.o_complete[i,1],only.o_complete[i,2],round(only.o_complete$pred_prob[i],2),pos=1,cex=0.35,font=4)
}

#2)percent forest cover
plot(isj[,2:3],type="n",main="percent forest cover",asp=1)
na.idx = which(is.na(isj$forest))
forest.star = isj$forest[-na.idx]
norm.forest = (forest.star-min(forest.star))/(max(forest.star)-min(forest.star))
points(isj[-na.idx,2:3],pch=20,col=grey(1-norm.forest))
points(isj[1:307,2:3],pch=ox_complete,col="red")
for(i in 1:nrow(isj.complete)){
  text(isj.complete[i,7],isj.complete[i,23],round(isj.complete$pred_prob[i],3),pos=1,cex=0.35,font=4)
}

#존재하는 곳에만 o 표시를 하겠다.
plot(isj[,2:3],type="n",main="percent forest cover",asp=1)
points(isj[-na.idx,2:3],pch=20,col=grey(1-norm.forest))
points(isj.complete[isj.complete$ox==1,c(7,23)],pch=1,col="red")
for(i in 1:nrow(isj.complete)){
  text(only.o_complete[i,1],only.o_complete[i,2],round(only.o_complete$pred_prob[i],3),pos=1,cex=0.35,font=4)
}


#3)percent chaparral cover
plot(isj[,2:3],type="n",main="percent chaparral cover",asp=1)
na.idx = which(is.na(isj$chap))
chap.star = isj$chap[-na.idx]
norm.chap = (chap.star-min(chap.star))/(max(chap.star)-min(chap.star))
points(isj[-na.idx,2:3],pch=20,col=grey(1-norm.chap))
points(isj[1:307,2:3],pch=ox_complete,col="red")
for(i in 1:nrow(isj.complete)){
  text(isj.complete[i,7],isj.complete[i,23],round(isj.complete$pred_prob[i],3),pos=1,cex=0.35,font=4)
}


plot(isj[,2:3],type="n",main="percent chaparral cover",asp=1)
points(isj[-na.idx,2:3],pch=20,col=grey(1-norm.chap))
points(isj.complete[isj.complete$ox==1,c(7,23)],pch=1,col="red")
for(i in 1:nrow(isj.complete)){
  text(only.o_complete[i,1],only.o_complete[i,2],round(only.o_complete$pred_prob[i],3),pos=1,cex=0.35,font=4)
}
#데이터 isj.complete에 의해 적합된 모델을 데이터 isj.na에 적용하여 island scrub jay가 있을 확률을 예측한다.
#반응변수의 값이 NA인 데이터들(isj.na)에서의 예측확률
pred.logit_na <- predict(fit.logit,newdata = isj.pred[,result.variable],type="response")
ox_na <- ifelse(pred.logit_na>=0.3,1,4) #있으면 = 1, 없으면 4로 저장
isj.na <- isj.pred %>% mutate("pred_prob"=pred.logit_na,
                            "ox"=ox_na)
colnames(isj.na)
##################################################################
###2484개의 데이터셋으로 예측
#다음은 isj.na를 활용한 예측확률을 나타낸 그래프들이다.
#1)Elevation
#island scrub jay 존재하는 곳에 대한 x,y,예측확률을 따로 저장한다. 
View(isj.na[isj.na$ox==1,c(7,23,24)])
only.o_na <- isj.na[isj.na$ox==1,c(7,23,24)]
plot(isj[,2:3],type="n",main="Elevation_prediction",asp=1)
points(isj[-na.idx,2:3],pch=20,col=grey(1-norm.elev))
points(isj.na[isj.na$ox==1,c(7,23)],pch=1,col="red") 
for(i in 1:nrow(isj.complete)){
  text(only.o_na[i,1],only.o_na[i,2],round(only.o_na$pred_prob[i],3),pos=1,cex=0.35,font=4)
}
#2)percent forest cover
plot(isj[,2:3],type="n",main="percent forest cover_prediction",asp=1)
points(isj[-na.idx,2:3],pch=20,col=grey(1-norm.forest))
points(isj.na[isj.na$ox==1,c(7,23)],pch=1,col="red")
for(i in 1:nrow(isj.complete)){
  text(only.o_na[i,1],only.o_na[i,2],round(only.o_na$pred_prob[i],3),pos=1,cex=0.35,font=4)
}

#3)percent chaparral cover
plot(isj[,2:3],type="n",main="percent forest cover_prediction",asp=1)
points(isj[-na.idx,2:3],pch=20,col=grey(1-norm.chap))
points(isj.na[isj.na$ox==1,c(7,23)],pch=1,col="red")
for(i in 1:nrow(isj.complete)){
  text(only.o_na[i,1],only.o_na[i,2],round(only.o_na$pred_prob[i],3),pos=1,cex=0.35,font=4)
}
