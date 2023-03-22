library(MASS)
library(ISLR)
library(corrplot)
library(ggcorrplot)
library(readr)
library(ggplot2)


#Import data set
Healthcare <- read.csv("healthcare-dataset-stroke-data.csv")
attach(Healthcare)


#Ridding the data of N/A values
dim(Healthcare)
sum(is.na(Healthcare$bmi))
Healthcare.new <- na.omit(Healthcare)
dim(Healthcare.new)
sum(is.na(Healthcare.new))
summary(Healthcare.new)
#Deleting "Other" under the gender column
table(Healthcare.new$gender)
which(Healthcare.new$gender=='Other')
na.omit(Healthcare.new[2971,])

#Changing to factors
Healthcare.new$stroke <- as.factor(Healthcare.new$stroke)
Healthcare.new$gender=as.factor(Healthcare.new$gender)
Healthcare.new$ever_married=as.factor(Healthcare.new$ever_married)
Healthcare.new$work_type=as.factor(Healthcare.new$work_type)
Healthcare.new$Residence_type=as.factor(Healthcare.new$Residence_type)
Healthcare.new$smoking_status=as.factor(Healthcare.new$smoking_status)


# EDA Section


#Relationship between stroke and bmi
boxplot1<- boxplot(Healthcare.new$bmi~Healthcare.new$stroke)
#Median bmi for no stroke & stroke
median_bmi <- boxplot1$stats[3,]
median_bmi

#Relationship between stroke and average glucose level
boxplot2 <- boxplot(Healthcare.new$avg_glucose_level~Healthcare.new$stroke)
#Median blood sugar level for no stroke & stroke
median_glucose <- boxplot2$stats[3,]
median_glucose

#Relationship between stroke and age
boxplot3 <- boxplot(Healthcare.new$age~Healthcare.new$stroke)
#Median age for no stroke & stroke
median_age <- boxplot3$stats[3,]
median_age


#Categorical variable relationships (after conversion to factors)

healthcare <- Healthcare.new

#Ever married
data2 <- data.frame(table(healthcare$stroke,healthcare$ever_married))
names(data2) <- c("stroke","ever_married","Count")
married_stroke_plot <- ggplot(data=data2, aes(x=stroke, y=Count, fill=ever_married)) + geom_bar(stat="identity") + labs(title="Ever_married Stroke", x="Stroke", y="# of Cases") + theme_bw() + guides(color=guide_legend("Ever_married")) + scale_fill_discrete(labels=c('No','Yes')) + scale_x_discrete(name= "Stroke",labels=c("0" = "No Stroke", "1"= "Stroke")) 
married_stroke_plot
#Work type
data3 <- data.frame(table(healthcare$stroke,healthcare$work_type))
names(data3) <- c("stroke","work_type","Count")
worktype_stroke_plot <- ggplot(data=data3, aes(x=stroke, y=Count, fill=work_type)) + geom_bar(stat="identity") + labs(title="Work_type Stroke", x="Stroke", y="# of Cases") +  theme_bw() + guides(color=guide_legend("Work_type")) + scale_x_discrete(name= "Stroke",labels=c("0" = "No Stroke", "1"= "Stroke")) 
worktype_stroke_plot
#Residence type
data4 <- data.frame(table(healthcare$stroke,healthcare$Residence_type))
  names(data4) <- c("stroke","Residence_type","Count")
residence_stroke_plot <- ggplot(data=data4, aes(x=stroke, y=Count, fill=Residence_type)) + geom_bar(stat="identity") + labs(title="Residence_type Stroke", x="Stroke", y="# of Cases") +   theme_bw() + guides(color=guide_legend("Residence_type")) + scale_x_discrete(name= "Stroke",labels=c("0" = "No Stroke", "1"= "Stroke")) 
residence_stroke_plot 
#Smoking status
data6 <- data.frame(table(healthcare$stroke,healthcare$smoking_status))
  names(data6) <- c("stroke","smoking_status","Count") 
smoke_stroke_plot <- ggplot(data=data6, aes(x=stroke, y=Count, fill=smoking_status)) + geom_bar(stat="identity") + labs(title="Smoking_status Stroke", x="Stroke", y="# of Cases") +  theme_bw() + guides(color=guide_legend("smoking_status")) + scale_x_discrete(name= "Stroke",labels=c("0" = "No Stroke", "1"= "Stroke")) 
smoke_stroke_plot  

#Chi Square Test 
#H0: the variables are independent, there is no relationship between the two categorical variables. Knowing the value of one variable does not help to predict the value of the other variable
#H1: the variables are dependent, there is a relationship between the two categorical variables. Knowing the value of one variable helps to predict the value of the other variable

table(healthcare$stroke, healthcare$hypertension)
chisq.test(healthcare$stroke, healthcare$hypertension, correct = FALSE)
table(healthcare$stroke, healthcare$heart_disease)
chisq.test(healthcare$stroke, healthcare$heart_disease, correct = FALSE)


#Logistic Regression before balancing data
fit.full <- glm(stroke~gender+age+hypertension+heart_disease+ever_married+work_type+Residence_type+avg_glucose_level+bmi+smoking_status, data = healthcare, family = binomial())
summary(fit.full)
fit.reduced <- glm(stroke~age+hypertension+heart_disease+avg_glucose_level, data = healthcare, family = binomial ())
summary(fit.reduced)

#Using glm() to test different age groups/// EXPERIMENTAL // Before balancing the data // Results not very useful

#Patients under age 40
train_age1 = (age < 40)
healthcare.40 = healthcare[!train_age1,]
dim(healthcare.40)
glm.fits=glm(stroke~age, data = healthcare, family = binomial, subset = train_age1)
summary(glm.fits)
stroke.prob_age=predict(glm.fits, healthcare.40, type = "response")
stroke.pred_age=rep(1, length(stroke.prob_age))
stroke.pred_age[stroke.prob_age>0]=0
table(stroke.pred_age, healthcare.40$stroke)
forty_accuracy <- mean(stroke.pred_age == healthcare.40$stroke)
print(forty_accuracy)
#Patients between ages 40 and 59
train_age2 = c(age>39 & age<60)
healthcare.60 = healthcare[!train_age2,]
table(healthcare.60$stroke)
glm.fits2 <- glm(stroke~age, data = healthcare, family = binomial, subset = train_age2)
summary(glm.fits2)
stroke.prob_age1=predict(glm.fits, healthcare.60, type = "response")
stroke.pred_age1=rep(1, length(stroke.prob_age1))
stroke.pred_age1[stroke.prob_age1>0]=0
table(stroke.pred_age1, healthcare.60$stroke)
sixty_accuracy <- mean(stroke.pred_age1 == healthcare.60$stroke)
print(sixty_accuracy)
#Patients 60 and over
train_age3 = (age>59)
healthcare.over60 = healthcare[!train_age3,]
table(healthcare.over60$age)
glm.fits3 <- glm(stroke~age, data = healthcare, family = binomial, subset = train_age3)
summary(glm.fits3)
stroke.prob_age3=predict(glm.fits, healthcare.over60, type = "response")
stroke.pred_age3=rep(1, length(stroke.prob_age3))
stroke.pred_age3[stroke.prob_age3>0]=0
table(stroke.pred_age3, healthcare.over60$stroke)
over60_accuracy <- mean(stroke.pred_age3 == healthcare.over60$stroke)
print(over60_accuracy)
#A fasting blood sugar level of 99 mg/dL or lower is normal
train_blood1 = (avg_glucose_level <= 99)
healthcare.normal = healthcare[!train_blood1,]
table <- table(healthcare.normal$stroke)
print(table)
glm.fits3=glm(stroke~avg_glucose_level, data = healthcare, family = binomial, subset = train_blood1)
summary(glm.fits3)
stroke.prob=predict(glm.fits3, healthcare.normal, type = "response")
stroke.pred=rep(1, length(stroke.prob))
stroke.pred[stroke.prob>0]=0
table(stroke.pred, healthcare.normal$stroke)
accuracy <- mean(stroke.pred == healthcare.normal$stroke)
print(accuracy)
#100 to 125 mg/dL indicates you have pre-diabetes
train_blood2 = c(avg_glucose_level >99 & avg_glucose_level < 126)
healthcare.normal2 = healthcare[!train_blood2,]
table2 <- table(healthcare.normal2$stroke)
print(table2)
glm.fits4=glm(stroke~avg_glucose_level, data = healthcare, family = binomial, subset = train_blood2)
summary(glm.fits4)
stroke.prob2=predict(glm.fits4, healthcare.normal2, type = "response")
stroke.pred2=rep(1, length(stroke.prob2))
stroke.pred2[stroke.prob2>0]=0
table(stroke.pred2, healthcare.normal2$stroke)
accuracy2 <- mean(stroke.pred2 == healthcare.normal2$stroke)
print(accuracy2)
#126 mg/dL or higher indicates you have diabetes
train_blood3 = (avg_glucose_level >125)
healthcare.normal3 = healthcare[!train_blood3,]
table3 <- table(healthcare.normal3$stroke)
print(table3)
glm.fits5 = glm(stroke~avg_glucose_level, data = healthcare, family = binomial, subset = train_blood3)
summary(glm.fits5)
stroke.prob3=predict(glm.fits5, healthcare.normal3, type = "response")
stroke.pred3=rep(1, length(stroke.prob3))
stroke.pred3[stroke.prob3>0]=0
table(stroke.pred3, healthcare.normal3$stroke)
accuracy3 <- mean(stroke.pred3 == healthcare.normal3$stroke)
print(accuracy3)

# End EDA







#package installation
##package for data frame
install.packages("data.table")
require(data.table)
library(data.table)
##package for data selection
install.packages("magrittr")
install.packages("dplyr")    # alternative installation of the %>%
##package for undersampling
install.packages("ROSE")     # Install ROSE

#loading library
##library for undersampling
library(ROSE)
##library for data selection
library(magrittr) 
library(dplyr)               # alternatively, this also loads %>%
library(MASS)

#loading the dataset
##working directory
setwd("path/to/the/dataset/file")
##loading from csv file
healthcare <- read.csv("healthcare-dataset-stroke-data-na-empty.csv", header=TRUE, stringsAsFactors=F)

#data pre-processing
##omiting the NA value
healthcare <- na.omit(healthcare)
##Changing the dataset to factors
healthcare.new <- healthcare
healthcare.new$stroke <- as.factor(healthcare.new$stroke)
healthcare.new$gender=as.factor(healthcare.new$gender)
healthcare.new$ever_married=as.factor(healthcare.new$ever_married)
healthcare.new$work_type=as.factor(healthcare.new$work_type)
healthcare.new$Residence_type=as.factor(healthcare.new$Residence_type)
healthcare.new$smoking_status=as.factor(healthcare.new$smoking_status)
healthcare.new$hypertension=as.factor(healthcare.new$hypertension)
##splitting the train:test 90:10
train90 <- healthcare %>% dplyr::sample_frac(.9)
test10  <- dplyr::anti_join(healthcare, train90, by = 'id')
##undersampling
data_balance_under <- ovun.sample(stroke ~ ., data = healthcare, method = "under", p = 0.4, seed = 1)$data
table(data_balance_under$stroke)
##loading the undersampling result into the training set variable
train90forundersampling<-data_balance_under
train90forundersampling
train90 <- train90forundersampling    #This is for training set after undersampling
##this part is just for handling the distinct attribute value (gender='other')
genderother <- healthcare[which(healthcare$gender=='Other'),]
train90 <- rbind(train90, genderother)
##selecting numeric data only for kNN
test10num<-test10 %>% dplyr::select(3, 4, 5, 9, 10)
train90num<-train90 %>% dplyr::select(3, 4, 5, 9, 10)


#exploratory data analysis
##plotting class distribution before undersampling
barplot(prop.table(table(healthcare$stroke)),
        col = rainbow(2),
        ylim = c(0, 0.7),
        main = "Class Distribution: Label = healthcare$stroke")
##plotting class distribution after undersampling
barplot(prop.table(table(data_balance_under$stroke)),
        col = rainbow(2),
        ylim = c(0, 0.7),
        main = "Class Distribution After Undersampling: Label = data_balance_under$stroke")

#Logistic Regression
##Fitting/Training the model with the training set
glmfitall<-glm(stroke~.-id, data=train90,family=binomial)
##Predict with the testing set
stroke.prob = predict(glmfitall, test10, type = "response")
stroke.pred = rep(1, length(stroke.prob))
stroke.pred[stroke.prob > 0] = 0
table(stroke.pred,test10$stroke)
mean(stroke.pred == test10$stroke)

#QDA
##Fitting/Training the model with the training set
qdafitall=qda(stroke~.-id-gender-work_type,data=train90)
##Predict with the testing set
qdapredstroke=predict(qdafitall,test10)$class
qdapredstrokedf <- as.data.frame(lapply(qdapredstroke, unlist))
table(as.matrix(qdapredstroke),as.matrix(test10$stroke))
mean(as.matrix(qdapredstroke)==as.matrix(test10$stroke))

#LDA
##Fitting/Training the model with the training set
ldafitall<-lda(stroke~.-id,data=train90,family=binomial)
##Predict with the testing set
ldapredstroke<-predict(ldafitall,test10)
ldapredstrokedf <- as.data.frame(lapply(ldapredstroke, unlist))
table(as.matrix(ldapredstrokedf$class),as.matrix(test10$stroke))
mean(as.matrix(ldapredstrokedf$class)==as.matrix(test10$stroke))

#kNN
##Fitting, Training, and Testing
strokeknnpred=knn(train90num,test10num,train90$stroke,k=5)
table(strokeknnpred,test10$stroke)
mean(strokeknnpred == test10$stroke)

