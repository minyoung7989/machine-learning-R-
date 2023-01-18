library(readxl)
library(dplyr)
library(fmsb)
library(NbClust)
library(flexclust)
library(caret)
dat <- read.csv("pattern.csv")

#결측값
colSums(is.na(dat))
#중복변수
sum(duplicated(dat))

a <- ifelse(dat$diagnosis == "M",1,0)
ndat <- dat[,-c(1,2)]

cor(ndat)
###vif 10이상인 지수 제거 
ndat2 <- my_func(ndat, thresh = 10, trace = T)

ndat3 <- ndat[, ndat2]
ndat4 <- scale(ndat3)

ndat_names <- names(data.frame(ndat4))

###t-test 1차변수 선택
t.test_p.value_df <- data.frame()

for (i in 1:length(ndat_names)) {
  
  t.test_p.value <- t.test(dat[,ndat_names[i]] ~ dat$diagnosis, var.equal = TRUE)$p.value
  
  t.test_p.value_df[i,1] <- ndat_names[i]
  
  t.test_p.value_df[i,2] <- t.test_p.value

}

t.test_p.value_df

##순서 나열 p값기준
arrange(t.test_p.value_df, V2)

aa <- t.test_p.value_df$V2 < 0.05
ndat_names2 <- ndat_names[aa]

ndat5 <- data.frame(ndat4[, ndat_names2])
ndat5 <- data.frame(a, ndat5)

###로지스틱으로 후진소거법으로 8개까지 줄이기
ldat <- glm(a ~ ndat5$texture_mean + ndat5$smoothness_mean + ndat5$points_mean + ndat5$symmetry_mean +
      ndat5$perimeter_se + ndat5$compactness_se + ndat5$concavity_se +ndat5$points_se + ndat5$area_worst +
      ndat5$smoothness_worst + ndat5$symmetry_worst + ndat5$dimension_worst, family = binomial)

ldat2 <- summary(step(ldat, direction = "backward"))

ndat6 <- data.frame(a, ndat5$texture_mean, ndat5$points_mean, ndat5$perimeter_se, ndat5$compactness_se,
                    ndat5$area_worst, ndat5$smoothness_worst, ndat5$symmetry_worst, ndat5$concavity_se)


###K-평균 군집분석
a <- ifelse(dat$diagnosis == "M",2,1)
a <- as.factor(a)

ndat6 <- data.frame(a, ndat5$texture_mean, ndat5$points_mean, ndat5$perimeter_se, ndat5$compactness_se,
                    ndat5$area_worst, ndat5$smoothness_worst, ndat5$symmetry_worst, ndat5$concavity_se)

#####트레이닝 데이터
set.seed(1714)
train <- createDataPartition(y = ndat6$a, p = 0.7, list = F)
training <- ndat6[train,]
testing <- ndat6[-train,]



trainingdata <- scale(training[-1])

ndat6.kmeans <- kmeans(trainingdata[,-1], centers = 2, iter.max = 10000)
ndat6.kmeans

training$cluster <- as.factor(ndat6.kmeans$cluster)
table(training$a, training$cluster)


trainingdata <- as.data.frame(trainingdata)
model1 <- train(x = trainingdata, y = training$cluster, method = "rpart")
######## 테스팅 데이터
testingdata <- as.data.frame(scale(testing[-1]))
testpred <- predict(model1, testingdata)
tab.dat <- table(testpred, testing$a)

mean(testpred == testing$a)



testing2 <- scale(testing[-1])
testing3 <- kmeans(testing2, 2, nstart = 20)


####k 개수 결정
nbdat <- NbClust(trainingdata, min.nc = 2, max.nc = 10, method = "kmeans")
barplot(table(nbdat$Best.nc[1,]))



wssplot <- function(data, nc=10, seed=1234) {
  wss <- (nrow(data)-1)*sum(apply(data, 2, var))
  for (i in 2:nc) {
    set.seed(seed)
    wss[i]<- sum(kmeans(data,centers = i)$withinss)
  }
  plot(1:nc, wss, type = "b", xlab="클러스터 개수")
}

wssplot(trainingdata)

barplot(table(nbdat$Best.nc[1,]), xlab = "클러스터 개수")

####테스팅 데이터를 가지고 클러스터링 플랏
plot(testing2, col = testing3$cluster)
points(testing3$centers, col="blue", pch=8, cex=1.5)

####20번반복

abc <- c(0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9294118, 0.9294118, 0.8294118, 0.9058824, 0.8705882
         ,0.9058824,0.8647059,0.8705882,0.8941176,0.8464706,0.9058824,0.8823529)

abcd <- data.frame(mean(abc), sd(abc))
names(abcd) <- c("평균", "표준편차")

###수정된 순위지수
randIndex(tab.dat)
