install.packages("caret")
install.packages("randomForest")
install.packages("HSAUR")
install.packages("fpc")
install.packages("lattice")
install.packages("rpart")
install.packages("kernlab")

library(datasets)
library(caret)
library(randomForest)
library(moments)
library(tidyverse)
library(knitr)
library(finalfit)
library(ggplot2)
library(ggthemes)
library(ggcharts)
library(DataExplorer)
library(cluster)
library(HSAUR)
library(fpc)
bank_data<-read.csv("~/Desktop/NYU_File/2021Fall/CS-GY6923 Machine Learning/data-society-bank-marketing-data/bank-full.csv",
                    header = TRUE,sep = ";",stringsAsFactors = FALSE)
names(bank_data)
dim(bank_data)
sum(is.na(bank_data))
sum(duplicated(bank_data))
sum(!complete.cases(bank_data))
all.empty = rowSums(is.na(bank_data))==ncol(bank_data)
sum(all.empty)


bank_data$age <- as.numeric(bank_data$age)
bank_data$job<-as.factor(bank_data$job)
bank_data$marital <- as.factor(bank_data$marital)
bank_data$education <-as.factor(bank_data$education)
bank_data$default <- as.factor(bank_data$default)
bank_data$balance <- as.numeric(bank_data$balance)
bank_data$housing <- as.factor(bank_data$housing)
bank_data$loan <- as.factor(bank_data$loan)
bank_data$contact <-as.factor(bank_data$contact)
bank_data$day <- as.numeric(bank_data$day)
bank_data$month <- as.factor(bank_data$month)
bank_data$duration <-as.numeric(bank_data$duration)
bank_data$campaign <- as.numeric(bank_data$campaign)
bank_data$pdays <-as.numeric(bank_data$pdays)
bank_data$previous <- as.numeric(bank_data$previous)
bank_data$poutcome <-as.factor(bank_data$poutcome)
bank_data$y <-as.factor(bank_data$y)
summary(bank_data)          
str(bank_data)

BankData<-data.frame(as.numeric(as.factor(bank_data$age)),
                     as.numeric(as.factor(bank_data$job)),
                     as.numeric(as.factor(bank_data$marital)),
                     as.numeric(as.factor(bank_data$education)),
                     as.numeric(as.factor(bank_data$default)),
                     as.numeric(as.factor(bank_data$balance)),
                     as.numeric(as.factor(bank_data$housing)),
                     as.numeric(as.factor(bank_data$loan)),
                     as.numeric(as.factor(bank_data$contact)),
                     as.numeric(as.factor(bank_data$duration)),
                     as.numeric(as.factor(bank_data$campaign)),
                     as.numeric(as.factor(bank_data$pdays)),
                     as.numeric(as.factor(bank_data$previous)),
                     as.numeric(as.factor(bank_data$poutcome)),
                     as.factor(as.factor(bank_data$y)))
                  
                     

                     
colnames(BankData) <- c ("Age", "Job", "Marital", "Education", "Default",
                         "Balance", "Housing","Loan", "Contact","Duration",
                         "Campaige","Pdays","Previous","Poutcome","Y") 
names(BankData)
dim(BankData)
str(BankData)

#random Forest 
set.seed(43)
rf = randomForest(Y~., data = BankData, ntree = 500)
print(rf)
importance(rf)
varImpPlot(rf)

#glm 
top5feature <- BankData [,c("Y","Duration", "Balance","Age", "Job", "Campaige")]
top5feature <- top5feature[complete.cases(top5feature),]

set.seed(43)
split_data <-sample(32211,13000,replace = FALSE)
test_data <- top5feature[split_data,]
train_data <- top5feature[-split_data,]
dim(test_data)
dim(train_data)
head(train_data)

glm_top5 <-glm(Y~., data = train_data, family = binomial())
glm_predict <- predict(glm_top5, test_data[, 2:6], type = "response");
library(pROC)
auc(test_data$Y, glm_predict)

#PCA
pca_top5 <-prcomp(top5feature[, 2:6], center = TRUE, scale. = TRUE)
summary(pca_top5)
install.packages("devtools")
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
ggbiplot(pca_top5)
ggbiplot(pca_top5, group = top5feature$Y)
# now let go over the some of the table in it self 
# first check the normality in age
summary(bank_data$age)
hist(bank_data$age,col="blue")
skewness(bank_data$age)
kurtosis(bank_data$age)
set.seed(43)
bank_data_norm<-rnorm(45211,mean = mean(bank_data$age), sd = sd(bank_data$age))
ks.test(bank_data$age,bank_data_norm)
                           #Distribution of age based on target group (Y, whether get subscribe)
bank_data %>%
  group_by(y) %>%
  summarise(mean = mean(age), sd = sd(age), median = median(age),IQR = IQR(age))
bank_data%>%
  ggplot(
    aes(x = age, fill = age)
  ) + geom_histogram(binwidth = 1,color = "black", fill = "red")+facet_wrap(~ y)+theme_classic()
boxplot(formula = age ~ y, data = bank_data, col = "turquoise3")
wilcox.test(bank_data$age ~ bank_data$y, mu=0, alternative = "two.sided", conf.level = 0.95, var.equal = F)




#second check the job description vs subscription 
summary(bank_data$job)
bank_data %>%
  summary_factorlist(dependent = "y",
                     explanatory = "job",
                     add_dependent_label = T,
                     cont_cut = 1)
bank_data%>%
  ggplot(aes(x=job, fill=job)) +
    geom_bar() +
      ggtitle("Term Deposit Subscription based on job title") +
        xlab("Job Title") +
          facet_wrap( ~ y)+
            theme(legend.title = element_blank())
chisq.test(bank_data$y, bank_data$job,correct = F)

#third check the matrial description vs subscription
summary(bank_data$marital)
bank_data %>%
  summary_factorlist(dependent = "y",
                     explanatory = "marital",
                     add_dependent_label = T,
                     cont_cut = 1)

bank_data%>%
  ggplot(aes(x=marital, fill=marital)) +
  geom_bar() +
  ggtitle("Term Deposit Subscription based on marital status ") +
  xlab("Marital Status") +
  facet_wrap( ~ y)+
  theme(legend.title = element_blank())
chisq.test(bank_data$y, bank_data$marital,correct = F)

#fourth check the education level vs subscription
summary(bank_data$education)
bank_data %>%
  summary_factorlist(dependent = "y",
                     explanatory = "education",
                     add_dependent_label = T,
                     cont_cut = 1)
bank_data%>%
  ggplot(aes(x=education, fill=education)) +
  geom_bar() +
  ggtitle("Term Deposit Subscription based on education level ") +
  xlab("Education Level") +
  facet_wrap( ~ y)+
  theme(legend.title = element_blank())
chisq.test(bank_data$y, bank_data$education,correct = F)

#fifth check the default vs subscription 
summary(bank_data$default)
bank_data %>%
  summary_factorlist(dependent = "y",
                     explanatory = "default",
                     add_dependent_label = T,
                     cont_cut = 1)
bank_data%>%
  ggplot(aes(x=default, fill=default)) +
  geom_bar() +
  ggtitle("Term Deposit Subscription based on default (credit) status") +
  xlab("default credit status ") +
  facet_wrap( ~ y)+
  theme(legend.title = element_blank())
chisq.test(bank_data$y, bank_data$default,correct = F)

#sixth check the balance vs subscription 
summary(bank_data$balance)
hist(bank_data$balance,col="blue")
skewness(bank_data$balance)
kurtosis(bank_data$balance)
set.seed(43)
bank_data_norm<-rnorm(45211,mean = mean(bank_data$balance), sd = sd(bank_data$balance))
ks.test(bank_data$balance,bank_data_norm)

bank_data %>%
  group_by(y) %>%
  summarise(mean = mean(balance), sd = sd(balance), median = median(balance),IQR = IQR(balance))
boxplot(formula = balance ~ y, data = bank_data, col = "turquoise3")
wilcox.test(bank_data$balance ~ bank_data$y, mu=0, alternative = "two.sided", conf.level = 0.95, var.equal = F)

#seventh check the housing status vs subscription
summary(bank_data$housing)
bank_data %>%
  summary_factorlist(dependent = "y",
                     explanatory = "housing",
                     add_dependent_label = T,
                     cont_cut = 1)
bank_data%>%
  ggplot(aes(x=housing, fill=housing)) +
  geom_bar() +
  ggtitle("Term Deposit Subscription based on housing status") +
  xlab("housing status ") +
  facet_wrap( ~ y)+
  theme(legend.title = element_blank())
chisq.test(bank_data$y, bank_data$housing,correct = F)

#eighth check the loan status vs subscription
summary(bank_data$loan)
bank_data %>%
  summary_factorlist(dependent = "y",
                     explanatory = "loan",
                     add_dependent_label = T,
                     cont_cut = 1)
bank_data%>%
  ggplot(aes(x=loan, fill=loan)) +
  geom_bar() +
  ggtitle("Term Deposit Subscription based on loan status") +
  xlab("loan status ") +
  facet_wrap( ~ y)+
  theme(legend.title = element_blank())
chisq.test(bank_data$y, bank_data$loan,correct = F)

#ninth check the contact methods vs subscription
summary(bank_data$contact)
bank_data %>%
  summary_factorlist(dependent = "y",
                     explanatory = "contact",
                     add_dependent_label = T,
                     cont_cut = 1)
bank_data%>%
  ggplot(aes(x=contact, fill=contact)) +
  geom_bar() +
  ggtitle("Term Deposit Subscription based on contact method") +
  xlab("contact method") +
  facet_wrap( ~ y)+
  theme(legend.title = element_blank())
chisq.test(bank_data$y, bank_data$contact,correct = F)

#tenth check the duration vs subscription 
summary(bank_data$duration)
hist(bank_data$duration,col="blue")
skewness(bank_data$duration)
kurtosis(bank_data$duration)
set.seed(43)
bank_data_norm<-rnorm(45211,mean = mean(bank_data$duration), sd = sd(bank_data$duration))
ks.test(bank_data$duration,bank_data_norm)


bank_data %>%
  group_by(y) %>%
  summarise(mean = mean(duration), sd = sd(duration), median = median(duration),IQR = IQR(duration))
bank_data%>%
  ggplot(
    aes(x = duration, fill = duration)
  ) + geom_histogram(binwidth = 2,color = "black", fill = "red")+facet_wrap(~ y)+theme_classic()
boxplot(formula = duration ~ y, data = bank_data, col = "turquoise3")
wilcox.test(bank_data$duration ~ bank_data$y, mu=0, alternative = "two.sided", conf.level = 0.95, var.equal = F)



