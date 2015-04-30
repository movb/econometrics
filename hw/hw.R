# Homework

library("memisc")
library("dplyr")
library("lmtest")
library("sjPlot")
library("sgof")
library("ggplot2")
library("foreign")
library("car")
library("hexbin")
library("rlms")
library("devtools")
library("psych")

devtools::install_github("bdemeshev/rlms")

h <- read.rlms("r22i_os25a.sav")
# rj13.2 - ЗП
# rh6 - год рождения
# rh5 - пол
# r_diplom - образование
# rj5a - с какого года работаете
# rj6.0 - количество подчиненных
data <- select(h, rj13.2, rh6, rh5, r_diplom, rj6.0, rj5a)
data <- mutate(data, age = 2013 - rh6, experience = 2013 - rj5a)
data <- rename(data, wage=rj13.2, sex=rh5, subordinates=rj6.0, education=r_diplom)
data <- filter(data, education != "ЗАТРУДНЯЮСЬ ОТВЕТИТЬ", education != "ОТКАЗ ОТ ОТВЕТА", education!="НЕТ ОТВЕТА")

glimpse(data)
levels(data$education) <- factor(data$education)
levels(data$education) <- c("_no_school", "_special", 
                            "_school", "_no_school",
                            "_no_school", "_high")
#data <- na.omit(data)

dummy <- model.matrix( ~ education-1, data=data)
data$education_no_school<-dummy[,1]
data$education_special<-dummy[,2]
data$education_school<-dummy[,3]
data$education_high<-dummy[,4]

data <- select(data, wage, age, sex, education_no_school,
               education_school, education_special,
               education_high, subordinates, experience)

describe(data)
qplot(data=data, wage,xlab="Заработная плата", ylab="Количество", main="Распределение заработной платы")
qplot(data=data, age, xlab="Возраст", ylab="Количество", main="Распределение возраста респондентов")

model1 = lm(data=data,wage~age+sex+education_school+education_special+education_high+
              subordinates+experience)
summary(model1)
