library(tidyverse)
library(corrplot)
library(gridExtra)
library(ggplot2)

setwd("C:/Users/SAMSUNG/Desktop/비어플/팀프로젝트 - 심리 성향 예측/open data")
getwd()
train <- read.csv('train.csv')

table(duplicated(train[,-1]))
str(train)
colnames(train)

train_new <- subset(train, select=-index)
train_new <- train_new[,c(seq(1,40,2),seq(2,40,2),41:77)]
str(train_new)

#### ==== 파생 변수 생성 1 Q_A ==== ####
# positive :  2 3  8 10 13 15 19       / b c h j m o s
# negative :  5 6 11 17 18             / e f k q r
# secret   :  1 4  7  9 12 14 16 20    / a d g i l n p t
# secret_n :  1 4  7  9 14             / a d g i n
# Tactics  :  3 6 15 18 19 7 12 14 20  / c f o r s / g l n t
# Views    :  2 5  8 10 13 17 1 4 16   / b e h j m q / a d p
# Morality : 11 9                      / k / n

corrplot(cor(train_new[1:20]), method='color')

train_new[c(5,6,11,17,18)] <- 6-train_new[c(5,6,11,17,18)]
corrplot(cor(train_new[1:20]), method='color')

train_new[c(1,4,7,9,14)] <- 6-train_new[c(1,4,7,9,14)]
corrplot(cor(train_new[1:20]), method='color')

train_new$Mach_score <- apply(train_new[1:20],1,mean)
summary(train_new$Mach_score)
ggplot(train_new, aes(Mach_score))+geom_boxplot()
boxplot(train_new$Mach_score)

corrplot(cor(train_new[c(3,6,15,18,19,2,5,8,10,13,17,11,1,4,7,9,12,14,16,20)]), method='number')


train_new$Tactic <- apply(train_new[c(3,6,15,18,19)],1,mean)
train_new$Views <- apply(train_new[c(2,5,8,10,13,17)],1,mean)
train_new$Morality <- apply(train_new[c(11)],1,mean)

train_new <- train_new[,-c(1:20)]

#### ==== 파생 변수 생성 2 TIPI ==== ####
# + 5  3 1 7 9
# - 10 8 6 2 4
#   O  C E A N

str(train_new[30:39])

train_new[30:39] <- 7-train_new[30:39]

train_new[which(train_new[30]==0,30)] <- 4
train_new[which(train_new[31]==0,31)] <- 4
train_new[which(train_new[32]==0,32)] <- 4
train_new[which(train_new[33]==0,33)] <- 4
train_new[which(train_new[34]==0,34)] <- 4
train_new[which(train_new[35]==0,35)] <- 4
train_new[which(train_new[36]==0,36)] <- 4
train_new[which(train_new[37]==0,37)] <- 4
train_new[which(train_new[38]==0,38)] <- 4
train_new[which(train_new[39]==0,39)] <- 4

train_new[seq(31,39,2)] <- 8-train_new[seq(31,39,2)]

train_new$O <- (train_new$tp05+train_new$tp10)/2
train_new$C <- (train_new$tp03+train_new$tp08)/2
train_new$E <- (train_new$tp01+train_new$tp06)/2
train_new$A <- (train_new$tp02+train_new$tp07)/2
train_new$N <- (train_new$tp04+train_new$tp09)/2

train_new <- train_new[,-c(30:39)]
str(train_new)

#### ==== 파생 변수 생성 3 wr, wf ==== ####
str(train_new[35:47])
str(train_new[32:34])

train_new$wr_mean <- apply(train_new[35:47],1,mean)
train_new$wf_mean <- apply(train_new[32:34],1,mean)

ggplot(data=train_new, aes(x=as.factor(round(wr_mean,3)),y=..count..,
                           fill=as.factor(round(wr_mean,3))))+
  geom_bar()+
  geom_text(stat="count", aes(label=..count..),vjust=-0.5)
ggplot(data=train_new, aes(x=as.factor(round(wf_mean,3)),y=..count..,
                           fill=as.factor(round(wf_mean,3))))+
  geom_bar()+
  geom_text(stat="count", aes(label=..count..),vjust=-0.5)
train_new <- train_new[,-c(32:47)]

#### ==== 파생 변수 생성 4 QE_mean ==== ####
str(train_new[1:20])
train_new$QE_mean <- apply(train_new[,c(1:20)],1,mean)
str(train_new)
train_new <- train_new %>% arrange(QE_mean)
boxplot(train_new$QE_mean)
ggplot(train_new, aes(QE_mean))+geom_boxplot()

train_new <- train_new[c(1:45150),]
boxplot(train_new$QE_mean)
ggplot(train_new, aes(QE_mean))+geom_boxplot()

for(i in 1:20){
  print(fivenum(log(train_new[,i])))
}# -inf -> 0 값 median


which(train_new$QhE==0)
which(train_new$QiE==0)
which(train_new$QkE==0)
which(train_new$QjE==0)
which(train_new$QoE==0)
which(train_new$QpE==0)
which(train_new$QqE==0)

train_new$QhE[29656]<-median(train_new$QhE)
train_new$QiE[29656]<-median(train_new$QiE)
train_new$QkE[18424]<-median(train_new$QkE)
train_new$QkE[40918]<-median(train_new$QkE)
train_new$QjE[26174]<-median(train_new$QjE)
train_new$QoE[33078]<-median(train_new$QoE)
train_new$QpE[9325]<-median(train_new$QpE)
train_new$QqE[9245]<-median(train_new$QqE)

for(i in 1:20){
  print(fivenum(log(train_new[,i])))
}
summary(train_new[,1:20])
boxplot(train_new$QE_mean)
ggplot(train_new, aes(QE_mean))+geom_boxplot()

train_new$QE_mean <- apply(train_new[,c(1:20)],1,mean)


str(train_new)
train_new <- train_new[,-c(1:20)]
train_new$QE_mean <- log(train_new$QE_mean)
boxplot(train_new$QE_mean)
ggplot(train_new, aes(QE_mean))+geom_boxplot()

str(train_new)

#### ==== 범주형 변수 재범주화 1 age_group ==== ####
table(train_new$age_group,train_new$voted)
ggplot(train_new %>% filter(age_group=='60s'|age_group=='+70s'), aes(voted, fill=as.factor(voted)))+geom_bar()+facet_wrap(age_group~.)

train_new$age_group <- ifelse(train_new$age_group=='10s',1,
                       ifelse(train_new$age_group=='20s',2,
                       ifelse(train_new$age_group=='30s',3,
                       ifelse(train_new$age_group=='40s',4,
                       ifelse(train_new$age_group=='50s',5,6)))))
table(train_new$age_group)

#### ==== 범주형 변수 재범주화 2 gender ==== ####
table(train_new$gender)

train_new$gender <- ifelse(train_new$gender=='Female',1,2)

table(train_new$gender)

#### ==== 범주형 변수 재범주화 3 race ==== ####
table(train_new$race)
train_new$race <- ifelse(train_new$race=='Arab',1,
                         ifelse(train_new$race=='Asian',2,
                                ifelse(train_new$race=='Black',3,
                                       ifelse(train_new$race=='Indigenous Australian',4,
                                              ifelse(train_new$race=='Native American',5,
                                                     ifelse(train_new$race=='White',6,7))))))
table(train_new$race)

#### ==== 범주형 변수 재범주화 4 religion ==== ####
table(train_new$religion)

train_new$religion <- ifelse(train_new$religion=='Agnostic',1,
                             ifelse(train_new$religion=='Atheist',2,
                                    ifelse(train_new$religion=='Buddhist',3,
                                           ifelse(train_new$religion=='Christian_Catholic',4,
                                                  ifelse(train_new$religion=='Christian_Protestant',5,
                                                         ifelse(train_new$religion=='Christian_Mormon',6,
                                                                ifelse(train_new$religion=='Christian_Other',7,
                                                                       ifelse(train_new$religion=='Hindu',8,
                                                                              ifelse(train_new$religion=='Jewish',9,
                                                                                     ifelse(train_new$religion=='Muslim',10,
                                                                                            ifelse(train_new$religion=='Sikh',11,12)))))))))))
table(train_new$religion)
str(train_new)

#### ==== 이상치 제거 1 family size ==== ####
table(train_new$familysize)

ggplot(train_new, aes(x=as.factor(familysize),y=..count..,fill=as.factor(familysize)))+
  geom_bar()+
  geom_text(stat="count", aes(label=..count..),vjust=-0.5)


train_new <- train_new[which(train_new$familysize<16),]
str(train_new)


#### ==== 이상치 제거 2 age_group & education ==== ####
table(train_new$age_group, train_new$education)
origin <- train_new
tea <- train_new %>% filter((age_group==1)&((education==0)|(education==3)|(education==4)))
train_new <- setdiff(train_new,tea) #차집합

dim(train_new)
dim(tea)

set.seed(1234)
table(tea$age_group, tea$education)
idxage <- sample(nrow(tea), 0.67*nrow(tea))

tea1 <- tea[-idxage,]
tea2 <- tea[idxage,]
tea1$education <- 1
tea2$education <- 2
table(tea1$education)
table(tea2$education)

tea <- rbind(tea1,tea2)
table(tea$education)
train_new <- rbind(train_new, tea)
str(train_new)

train_new[which(train_new$education==0&train_new$age_group==2),'education']<-3
train_new[which(train_new$education==0&train_new$age_group==3),'education']<-3
train_new[which(train_new$education==0&train_new$age_group==4),'education']<-3
train_new[which(train_new$education==0&train_new$age_group==5),'education']<-3
train_new[which(train_new$education==0&train_new$age_group==6),'education']<-4
table(train_new$age_group, train_new$education)

#### ==== 결측치 처리 engnat, hand, married, urban ==== ####

str(train_new)

table(train_new$engnat)
table(train_new$hand)
table(train_new$married)
table(train_new$urban)

train_new[which(train_new$engnat==0),'engnat']<-1
train_new[which(train_new$hand==0),'hand']<-1
train_new[which(train_new$married==0),'married']<-1
train_new[which(train_new$urban==0),'urban']<-2

table(train_new$engnat)
table(train_new$hand)
table(train_new$married)
table(train_new$urban)
str(train_new)
train_new$voted <- as.factor(train_new$voted)

ggplot(data=train_new %>% filter(religion==c(3,8,9,10,11,12)), aes(x=voted,fill=voted))+
  geom_bar()+facet_wrap(religion~.)

ggplot(data=train_new %>% filter(race==c(2,6,7)), aes(voted,fill=voted))+geom_bar()+facet_wrap(race~.)

train_new$gender <- as.factor(train_new$gender)


ggplot(train_new %>% filter(O>5.5&voted==1),aes(x=voted,fill=gender))+geom_bar()+coord_polar(theta='y')
o2 <- ggplot(train_new %>% filter(O>5.5&voted==2),aes(x=voted,fill=gender))+geom_bar()+coord_polar('y',start=0)

c1 <- ggplot(train_new %>% filter(C>5.5&voted==1),aes(x=voted,fill=gender))+geom_bar()+coord_polar('y',start=0)
c2 <- ggplot(train_new %>% filter(C>5.5&voted==2),aes(x=voted,fill=gender))+geom_bar()+coord_polar('y',start=0)

e1 <- ggplot(train_new %>% filter(E>5.5&voted==1),aes(x=voted,fill=gender))+geom_bar()+coord_polar('y',start=0)
e2 <- ggplot(train_new %>% filter(E>5.5&voted==2),aes(x=voted,fill=gender))+geom_bar()+coord_polar('y',start=0)

a1 <- ggplot(train_new %>% filter(A>5.5&voted==1),aes(x=voted,fill=gender))+geom_bar()+coord_polar('y',start=0)
a2 <- ggplot(train_new %>% filter(A>5.5&voted==2),aes(x=voted,fill=gender))+geom_bar()+coord_polar('y',start=0)

n1 <- ggplot(train_new %>% filter(N>5.5&voted==1),aes(x=voted,fill=gender))+geom_bar()+coord_polar('y',start=0)
n2 <- ggplot(train_new %>% filter(N>5.5&voted==2),aes(x=voted,fill=gender))+geom_bar()+coord_polar('y',start=0)

grid.arrange(o1,o2,ncol=2)
grid.arrange(c1,c2,ncol=2)
grid.arrange(e1,e2,ncol=2)
grid.arrange(a1,a2,ncol=2)
grid.arrange(n1,n2,ncol=2)


ggplot(train_new, aes(O, fill=gender))+geom_bar()+facet_wrap(voted~.)
  coord_polar('y')


table(train_new$gender)



#### ==== ==== ####

# 모델링 training set 생성 ####
# 1_1) race - Asian, White, Black, Other 01-08 ####
train_race1 <- train_new
table(train_race1$race)
train_race1[which(train_race1$race==1),'race']<-7
train_race1[which(train_race1$race==4),'race']<-7
train_race1[which(train_race1$race==5),'race']<-7
table(train_race1$race)
train_race1[which(train_race1$race==2),'race']<-1
train_race1[which(train_race1$race==3),'race']<-2
train_race1[which(train_race1$race==6),'race']<-3
train_race1[which(train_race1$race==7),'race']<-4
table(train_race1$race)

train_race1$race <- as.factor(train_race1$race)

train01<-train_race1
train02<-train_race1
train03<-train_race1
train04<-train_race1
train05<-train_race1
train06<-train_race1
train07<-train_race1
train08<-train_race1

# 1_2) race - Asian, White, Other        09-16 ####
train_race2<-train_new
table(train_race2$race)
train_race2[which(train_race2$race==1), 'race']<-7
train_race2[which(train_race2$race==3), 'race']<-7
train_race2[which(train_race2$race==4), 'race']<-7
train_race2[which(train_race2$race==5), 'race']<-7
table(train_race2$race)
train_race2[which(train_race2$race==2), 'race']<-1
train_race2[which(train_race2$race==6), 'race']<-2
train_race2[which(train_race2$race==7), 'race']<-3
table(train_race2$race)

train_race2$race <- as.factor(train_race2$race)

train09<-train_race2
train10<-train_race2
train11<-train_race2
train12<-train_race2
train13<-train_race2
train14<-train_race2
train15<-train_race2
train16<-train_race2

################################################################################
# 2_1) 10대 voted yes 제거 O 1 2 3 4  9 10 11 12 ####
train_age1 <- train01
table(train_age1$age_group, train_age1$voted)
train_age1[which(train_age1$age_group==1&train_age1$voted==1),'voted']<-2
table(train_age1$age_group, train_age1$voted)

train_age1$age_group <- as.factor(train_age1$age_group)

train01<-train_age1
train02<-train_age1
train03<-train_age1
train04<-train_age1

train_age1 <- train09
table(train_age1$age_group, train_age1$voted)
train_age1[which(train_age1$age_group==1&train_age1$voted==1),'voted']<-2
table(train_age1$age_group, train_age1$voted)

train_age1$age_group <- as.factor(train_age1$age_group)
train09<-train_age1
train10<-train_age1
train11<-train_age1
train12<-train_age1

# 2_2) 10대 voted yes 제거 X 5 6 7 8 13 14 15 16 ####
# train05
# train06
# train07
# train08
# train13
# train14
# train15
# train16

################################################################################
# 3_1) 크리스찬 병합 O 1  2  3  4  5  6  7  8 ####


train01[which(train01$religion==5|train01$religion==6|train01$religion==7),'religion']<-4
train02[which(train02$religion==5|train02$religion==6|train02$religion==7),'religion']<-4
train03[which(train03$religion==5|train03$religion==6|train03$religion==7),'religion']<-4
train04[which(train04$religion==5|train04$religion==6|train04$religion==7),'religion']<-4
train05[which(train05$religion==5|train05$religion==6|train05$religion==7),'religion']<-4
train06[which(train06$religion==5|train06$religion==6|train06$religion==7),'religion']<-4
train07[which(train07$religion==5|train07$religion==6|train07$religion==7),'religion']<-4
train08[which(train08$religion==5|train08$religion==6|train08$religion==7),'religion']<-4


# 3_2) 크리스찬 병합 X 9 10 11 12 13 14 15 16 ####
# :)
################################################################################
# 4_1) other 병합 O 1 2 3 4  9 10 11 12 ####
train01[which(train01$religion==3|train01$religion==8|train01$religion==9|
            train01$religion==10|train01$religion==11),'religion']<-12
train02[which(train02$religion==3|train02$religion==8|train02$religion==9|
            train02$religion==10|train02$religion==11),'religion']<-12
train03[which(train03$religion==3|train03$religion==8|train03$religion==9|
            train03$religion==10|train03$religion==11),'religion']<-12
train04[which(train04$religion==3|train04$religion==8|train04$religion==9|
            train04$religion==10|train04$religion==11),'religion']<-12
train09[which(train09$religion==3|train09$religion==8|train09$religion==9|
            train09$religion==10|train09$religion==11),'religion']<-12
train10[which(train10$religion==3|train10$religion==8|train10$religion==9|
            train10$religion==10|train10$religion==11),'religion']<-12
train11[which(train11$religion==3|train11$religion==8|train11$religion==9|
            train11$religion==10|train11$religion==11),'religion']<-12
train12[which(train12$religion==3|train12$religion==8|train12$religion==9|
            train12$religion==10|train12$religion==11),'religion']<-12
# 4_2) other 병합 X 5 6 7 8 13 14 15 16 ####
# :)

str(train12)
train12 <- train12[,-23]

train12$education <- as.factor(train12$education)
train12$engnat <- as.factor(train12$engnat)
train12$gender <- as.factor(train12$gender)
train12$hand <- as.factor(train12$hand)
train12$married <- as.factor(train12$married)
train12$religion <- as.factor(train12$religion)
train12$urban <- as.factor(train12$urban)


ggplot(train12, aes(voted,fill=voted))+geom_bar()+facet_wrap(religion~.)
