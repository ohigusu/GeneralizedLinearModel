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
###step4.최적의 설명변수 조합 찾기
features <- setdiff(names(isj_train), c("isj", "originalX", "originalY"))
#제거 또는 선택할 변수들의 인덱스
2:ncol(isj_train_use) 
#idx.result에 for문에서 사용할 변수명을 저장
best_features <- NULL
#error.result에 적합한 모델의 오분류율 저장
best_error <- Inf
best_model <- NULL
best_aic <- Inf

for (num_features in 1:length(features)) {
  feature_combinations <- combn(features, num_features, simplify = FALSE)
  
  for (combo in feature_combinations) {
    train_subset <- isj_train_use %>% select(isj, all_of(combo))
    test_subset <- isj_test_use %>% select(isj, all_of(combo))
    
    # fitting
    fit.logit <- glm(isj ~ ., family = binomial, data = train_subset)
    
    # 예측
    pred.logit <- predict(fit.logit, newdata = test_subset, type = "response")
    predictions <- ifelse(pred.logit >= 0.3, 1, 0)
    
    # 오분류율 계산
    confusion_table <- table(predictions, test_subset$isj)
    if (nrow(confusion_table) == 1) {
      error_rate <- confusion_table[1, 2] / sum(confusion_table)
    } else {
      error_rate <- (confusion_table[1, 2] + confusion_table[2, 1]) / sum(confusion_table)
    }
    
    # AIC 계산
    model_aic <- AIC(fit.logit)
    
    if (error_rate < best_error) {
      best_error <- error_rate
      best_aic <- model_aic
      best_model <- fit.logit
      best_features <- combo
    } else if (error_rate == best_error && model_aic < best_aic) {
      best_aic <- model_aic
      best_model <- fit.logit
      best_features <- combo
    }
  }
}

cat("Best Features:", best_features, "\n")
cat("Best Misclassification Rate:", best_error, "\n")
cat("Best AIC:", best_aic, "\n")
summary(best_model)

selected_features <- c('x', 'forest', 'chap', 'forest2', 'x.y', 'y.chap', 'elev.chap', 'forest.chap')
formula_str <- paste("isj ~", paste(selected_features, collapse = " + "))
fit.logit <- glm(as.formula(formula_str), family = binomial,data=isj.complete)
summary(fit.logit)

#####################################################################
###step5
#반응변수의 값이 NA가 아닌 데이터들(isj.complete)에서의 예측확률
pred.logit_complete <- predict(fit.logit,type="response")
ox_complete <- ifelse(pred.logit_complete>=0.3,1,4)  #있으면 = 1, 없으면 4로 저장
isj.complete <- isj.complete %>% mutate("pred_prob"=pred.logit_complete,
                                        "ox"= ox_complete)

###다음은 반응변수의 값이 NA가 아닌 데이터들(isj.complete)에서의 예측확률을 나타낸 그래프들이다.
#1)Elevation
plot(isj[,2:3],type="n",main="Elevation",asp=1)
na.idx = which(is.na(isj$elev))
elev.star = isj$elev[-na.idx]
norm.elev = (elev.star-min(elev.star))/(max(elev.star)-min(elev.star))
points(isj[-na.idx,2:3],pch=20,col=grey(1-norm.elev))
points(isj.complete[,c(22,23)],pch=ox_complete,col="red")
for(i in 1:nrow(isj.complete)){
  text(isj.complete[i,22],isj.complete[i,23],round(isj.complete$pred_prob[i],3),pos=1,cex=0.35,font=4)
}

#island scrub jay 존재하는 곳에만 o 표시를 하겠다.
#island scrub jay 존재하는 곳에 대한 x,y,예측확률을 따로 저장한다. 
only.o_complete <- isj.complete[isj.complete$ox==1,c(22,23,24)] 
plot(isj[,2:3],type="n",main="Elevation",asp=1)
points(isj[-na.idx,2:3],pch=20,col=grey(1-norm.elev))
points(isj.complete[isj.complete$ox==1,c(22,23)],pch=1,col="red")
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
  text(isj.complete[i,22],isj.complete[i,23],round(isj.complete$pred_prob[i],3),pos=1,cex=0.35,font=4)
}

#존재하는 곳에만 o 표시를 하겠다.
plot(isj[,2:3],type="n",main="percent forest cover",asp=1)
points(isj[-na.idx,2:3],pch=20,col=grey(1-norm.forest))
points(isj.complete[isj.complete$ox==1,c(22,23)],pch=1,col="red")
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
  text(isj.complete[i,22],isj.complete[i,23],round(isj.complete$pred_prob[i],3),pos=1,cex=0.35,font=4)
}


plot(isj[,2:3],type="n",main="percent chaparral cover",asp=1)
points(isj[-na.idx,2:3],pch=20,col=grey(1-norm.chap))
points(isj.complete[isj.complete$ox==1,c(22,23)],pch=1,col="red")
for(i in 1:nrow(isj.complete)){
  text(only.o_complete[i,1],only.o_complete[i,2],round(only.o_complete$pred_prob[i],3),pos=1,cex=0.35,font=4)
}
#데이터 isj.complete에 의해 적합된 모델을 데이터 isj.na에 적용하여 island scrub jay가 있을 확률을 예측한다.
#반응변수의 값이 NA인 데이터들(isj.na)에서의 예측확률
pred.logit_na <- predict(fit.logit,newdata = isj.pred[,selected_features],type="response")
ox_na <- ifelse(pred.logit_na>=0.3,1,4) #있으면 = 1, 없으면 4로 저장
isj.na <- isj.pred %>% mutate("pred_prob"=pred.logit_na,
                              "ox"=ox_na)
colnames(isj.na)
##################################################################
###2484개의 데이터셋으로 예측
#다음은 isj.na를 활용한 예측확률을 나타낸 그래프들이다.
#1)Elevation
#island scrub jay 존재하는 곳에 대한 x,y,예측확률을 따로 저장한다. 
View(isj.na[isj.na$ox==1,c(22,23,24)])
only.o_na <- isj.na[isj.na$ox==1,c(22,23,24)]
plot(isj[,2:3],type="n",main="Elevation_prediction",asp=1)
points(isj[-na.idx,2:3],pch=20,col=grey(1-norm.elev))
points(isj.na[isj.na$ox==1,c(22,23)],pch=1,col="red") 


for(i in 1:nrow(isj.complete)){
  text(only.o_na[i,1],only.o_na[i,2],round(only.o_na$pred_prob[i],3),pos=1,cex=0.35,font=4)
}
#2)percent forest cover
plot(isj[,2:3],type="n",main="percent forest cover_prediction",asp=1)
points(isj[-na.idx,2:3],pch=20,col=grey(1-norm.forest))
points(isj.na[isj.na$ox==1,c(22,23)],pch=1,col="red")
for(i in 1:nrow(isj.complete)){
  text(only.o_na[i,1],only.o_na[i,2],round(only.o_na$pred_prob[i],3),pos=1,cex=0.35,font=4)
}

#3)percent chaparral cover
plot(isj[,2:3],type="n",main="percent chaparral cover_prediction",asp=1)
points(isj[-na.idx,2:3],pch=20,col=grey(1-norm.chap))
points(isj.na[isj.na$ox==1,c(22,23)],pch=1,col="red")
for(i in 1:nrow(isj.complete)){
  text(only.o_na[i,1],only.o_na[i,2],round(only.o_na$pred_prob[i],3),pos=1,cex=0.35,font=4)
}
