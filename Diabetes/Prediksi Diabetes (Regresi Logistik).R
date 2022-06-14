#Data Preparation
# package yang diperlukan
install.packages("gtools")
install.packages("ggcorrplot")
install.packages("corrplot")
install.packages("PerformanceAnalytics")
install.packages("ROCR")
install.packages("psych")
install.packages("GGally")
install.packages("caret")
library(readr)
library(tidyverse)
library(gtools)
library(ggplot2)                  
library(ggcorrplot)
library(corrplot)
library(PerformanceAnalytics)     
library(ROCR)
library(tidyverse)
library(psych)                    
library(GGally)
library(caret)
library(readr)

#import dataset
diabetes_dataset <- read_csv("diabetes-dataset.csv")
View(diabetes_dataset)

#cek type Data
str(diabetes_dataset)
summary(diabetes_dataset)

#cek dimensi
dim(diabetes_dataset)

#cek missing value
diabetes_dataset %>% 
  is.na() %>% 
  colSums()

#Konversikan Outcome pada db : 0 = No  &  1 = Yes
diabetes_dataset$Outcome <- as.factor(diabetes_dataset$Outcome)
levels(diabetes_dataset$Outcome) <- c("No","Yes")

#Data Exploration
#Analisis Kelompok Usia
ggplot(aes(x = Age), data=diabetes_dataset) +
  geom_histogram(binwidth=1, color='white', fill = "#FF00FF") +
  scale_x_continuous(limits=c(20,90), breaks=seq(20,90,5)) +
  xlab("Usia") +
  ylab("Jumlah Orang Berdasarkan Usia")

# Membuat kolom kategori usia
diabetes_dataset$Age_Cat <- ifelse(diabetes_dataset$Age < 21, "<21", 
                     ifelse((diabetes_dataset$Age>=21) & (diabetes_dataset$Age<=25), "21-25", 
                            ifelse((diabetes_dataset$Age>25) & (diabetes_dataset$Age<=30), "25-30",
                                   ifelse((diabetes_dataset$Age>30) & (diabetes_dataset$Age<=35), "30-35",
                                          ifelse((diabetes_dataset$Age>35) & (diabetes_dataset$Age<=40), "35-40",
                                                 ifelse((diabetes_dataset$Age>40) & (diabetes_dataset$Age<=50), "40-50",
                                                        ifelse((diabetes_dataset$Age>50) & (diabetes_dataset$Age<=60), "50-60",">60")))))))
diabetes_dataset$Age_Cat <- factor(diabetes_dataset$Age_Cat, levels = c('<21','21-25','25-30','30-35','35-40','40-50','50-60','>60'))

# Barplot by Age_Cat
ggplot(aes(x = Age_Cat), data = diabetes_dataset) +
  geom_bar(fill='#FF00FF')

#Analisis Korelasi
diabetes_dataset_cor <- round(cor(diabetes_dataset[1:8]),1)
ggcorrplot(diabetes_dataset_cor)
corrplot.mixed(corr = cor(diabetes_dataset[1:8]))
ggcorr(diabetes_dataset[,-9], name = "corr", label = TRUE) + theme(legend.position="none")+
  labs(title="Korelasi Variansi ") + theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=12))

#Spiliting Data
RNGkind(sample.kind = "Rounding")
set.seed(417) 

index_diabetes_dataset <- sample(x = nrow(diabetes_dataset) , size = nrow(diabetes_dataset)*0.75) 
train <- diabetes_dataset[index_diabetes_dataset , ]
test <- diabetes_dataset[-index_diabetes_dataset, ] # tanda - berarti negasi

#cek spliting data train dan test
nrow(train)
nrow(test)

# re-check class imbalance
train$Outcome %>% 
  table() %>% 
  prop.table()

#Model Fitting
#fitting model dengan semua variabel independen
model <- glm(Outcome ~ ., data = train, family = binomial)
summary(model)
model2 <- step(object = model, direction="backward", trace = 0)
summary(model2)
exp(coef(model2))

#Prediksi Pada Data Train
#prediksikan outcome pada data train

pred_train <- predict(model2, type = "response")
tapply(pred_train, train$Outcome, mean)

#Treshold 0.3
TH_0.3 <- table(train$Outcome, pred_train > 0.3)
TH_0.3
acc_0.3 <- round(sum(diag(TH_0.3))/sum(TH_0.3),2)
sensitivity0.3 <- round(161/(40+161),2)
specificity0.3 <- round(270/(270+105),2)
sprintf("Nilai Akurasi sebesar %s", acc_0.3)
sprintf("Nilai Sensitivitas sebesar %s", sensitivity0.3)
sprintf("Nilai Spesifikasi sebesar %s", specificity0.3)

#Treshold 0.5
TH_0.5 <- table(train$Outcome, pred_train > 0.5)
TH_0.5
acc_0.5 <- round(sum(diag(TH_0.5))/sum(TH_0.5),2)
sensitivity0.5 <- round(180/(21+180),2)
specificity0.5 <- round(215/(215+160),2)
sprintf("Nilai Akurasi sebesar %s", acc_0.5)
sprintf("Nilai Sensitivitas sebesar %s", sensitivity0.5)
sprintf("Nilai Spesifikasi sebesar %s", specificity0.5)

#ROC
ROCRprediction = prediction(pred_train, train$Outcome)
ROCRperfomance = performance(ROCRprediction, "tpr", "fpr")

# Adding threshold labels
plot(ROCRperfomance, colorize=TRUE, print.cutoffs.at = seq(0,1,0.1), text.adj = c(-0.2, 1.7))
abline(a=0, b=1)

auc_train <- round(as.numeric(performance(ROCRprediction, "auc")@y.values),2)
legend(.8, .2, auc_train, title = "AUC", cex=1)

#Prediksi pada data test
pred_test <- predict(model2, type = "response", newdata = test)

# Berdasarkan kurva ROC di atas, dipilih ambang batas 0,5
test_tab <- table(test$Outcome, pred_test > 0.5)
test_tab
acc_test <- round(sum(diag(test_tab))/sum(test_tab),2)
sprintf("Nilai Akurasi pada Data Test sebesar %s", acc_test)

# ROC pada Data Test
ROCRPredTest = prediction(pred_test, test$Outcome)
auc = round(as.numeric(performance(ROCRPredTest, "auc")@y.values),2)
sprintf("Nilai AUC pada Data Test sebesar %s", auc)
