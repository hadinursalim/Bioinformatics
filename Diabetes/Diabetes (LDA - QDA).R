# Persiapan Library
library(heplots)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(broom)
install.packages("AUC")
library(AUC)
library(caret)
library(klaR)
library(biotools)
library(candisc)
library(DFA.CANCOR)
library(MVN)    #Uji multivariate normal
library(MASS)   #Fungsi diskriminan analisis

# Dataset Diabetes
diabetes <- read.csv("D:/Kuliah/R dan Python/diabetes.csv") 
head(diabetes, 5)

dim(diabetes)
diabetes <- dplyr::rename(diabetes, Pedigree = DiabetesPedigreeFunction)

## Saat memeriksa kerangka data, terlihat bahwa ada beberapa orang dengan tingkat insulin nol.
## Kadar insulin mungkin sangat penting dalam menentukan apakah seseorang menderita diabetes atau tidak dan kadar insulin 0 tidak mungkin.
## Oleh karena itu, diputuskan untuk membuang semua pengamatan yang memiliki tingkat insulin 0.
ins.zero <- subset(diabetes, Insulin == 0)
nrow(ins.zero)

nrow(diabetes)

## Mengingat bahwa sekarang hanya terdapat 768 pengamatan,
## sangat menyakitkan bahwa harus membuang hampir setengah dari pengamatan yang ada.
## Seseorang dapat melakukan analisis tanpa insulin atau menghitung kadar insulin yang hilang berdasarkan fakta bahwa seseorang menderita diabetes atau tidak.
## Selain nilai nol di kolom insulin, ada juga nilai nol di kolom dua sampai enam.
## Akan menghapus nilai-nilai ini juga.

# replaces all zero values from column two to six with NA
diabetes[, 2:6][diabetes[, 2:6] == 0] <- NA 
# now we omit all NA values
diabetes <- na.omit(diabetes)
# at the end, we are left with 392 observations for our analysis
nrow(diabetes)
## [1] 392
# we substitute ones with "Diabetes" and zeros with "No Diabetes"
diabetes$Outcome <- ifelse(diabetes$Outcome == 1, "Diabetes", "No Diabetes")

## Setelah beberapa pembersihan data, di mana telah membuang nilai yang hilang, maka siap untuk di analisis.
# Pengecekan Asumsi LDA vs QDA
# Pengecekan Asumsi matriks Equal Variance-Covariance
## Akan digunakan LDA dan QDA untuk mengklasifikasikan pengamatan ini menjadi diabetes dan tidak diabetes.
## Dari sini, diketahui asumsi tentang LDA dan QDA, jadi mari kita periksa.

# Memeriksa Asumsi Equal Variance
## Pertama, untuk memahami data kita dan jika kita memiliki varians yang sama di antara setiap kelas,
## kita dapat menggunakan boxplot.
plot <- list()
box_variables <- c("Pregnancies", "Age", "Glucose")
for(i in box_variables) {
  plot[[i]] <- ggplot(diabetes, 
                      aes_string(x = "Outcome", 
                                 y = i, 
                                 col = "Outcome", 
                                 fill = "Outcome")) + 
    geom_boxplot(alpha = 0.2) + 
    theme(legend.position = "none") + 
    scale_color_manual(values = c("blue", "red")) +
    scale_fill_manual(values = c("blue", "red"))
}
do.call(grid.arrange, c(plot, nrow = 1))

## Tiga plot kotak yang berbeda menunjukkan kepada kita bahwa panjang setiap plot jelas berbeda.
## Ini merupakan indikasi varians non-sama.

## Kita dapat menguji lebih lanjut asumsi homogenitas matriks varians-kovarians dengan memplot elips kovarians.
# Memeriksa Asumsi Equal Covariance Ellipse
heplots::covEllipses(diabetes[,1:8], 
                     diabetes$Outcome, 
                     fill = TRUE, 
                     pooled = FALSE, 
                     col = c("blue", "red"), 
                     variables = c(1:5, 8), 
                     fill.alpha = 0.05)

ggplot(diabetes, aes(x = Glucose, y = Insulin, col = Outcome)) + 
  geom_point() + 
  stat_ellipse() + 
  scale_color_manual(values = c("blue", "red"))
## Dari scatter plot ini, kita dapat melihat dengan jelas bahwa varians untuk kelompok diabetes jauh lebih besar daripada varians dari kelompok non-diabetes.
## Hal ini dikarenakan titik biru memiliki spread yang lebih luas.
## Titik merah kontras tidak memiliki spread selebar titik biru.

# Akan digunakan uji BoxM untuk memeriksa asumsi tentang homogenitas matriks varians-kovarians.
## = Matriks kovarians dari variabel hasil sama di semua kelompok
## = Matriks kovarians dari variabel hasil berbeda untuk setidaknya satu kelompok
# using columns 1 to 5 and 8 
boxm <- heplots::boxM(diabetes[, c(1:5, 8)], diabetes$Outcome)
boxm

## Ketika memilih alpha menjadi 0,05 maka dari hasil ini,
## dapat disimpulkan bahwa terdapat masalah matriks heterogenitas varians-kovarians.
## Plot di bawah ini memberikan informasi tentang bagaimana kelompok berbeda dalam komponen yang masuk ke uji Box M.
plot(boxm)

## Penentu log dipesan sesuai dengan ukuran elips yang kita lihat di plot elips kovarians.
## Plot ini mengkonfirmasi visualisasi bahwa terdapat elips dengan ukuran berbeda
## dan oleh karena itu, tidak ada matriks varians-kovarians yang sama.
## Perlu dicatat bahwa uji Box M sensitif dan dapat mendeteksi penyimpangan kecil dari homogenitas.
leveneTest(Pregnancies ~ Outcome, diabetes)

## Dari tes ini, kita dapat melihat bagaimana varians kelompok berbeda untuk Kehamilan.
## Mereka juga berbeda untuk variabel Usia, Glukosa, dan Insulin (tidak ditampilkan).
leveneTest(BloodPressure ~ Outcome, diabetes)

## Untuk BloodPressure, variansnya tampaknya sama.
## Ini juga berlaku untuk SkinThickness, dan BMI (tidak ditampilkan).

diab.yes <- subset(diabetes, Outcome == "Diabetes") 
diab.no <- subset(diabetes, Outcome == "No Diabetes")

# Pemeriksaan Asumsi LDA vs. QDA – Memeriksa Asumsi Normalitas
## Dengan qqplots berikut, kami memeriksa bahwa distribusi prediktor terdistribusi normal dalam kelompok diabetes dan kelompok non-diabetes.

# Kelompok Diabetes
variable_1 <- c("Pregnancies", "Glucose", "Insulin", "Age") 
par(mfrow = c(2, 2)) 
for(i in variable_1) { 
  qqnorm(diab.yes[[i]]); qqline(diab.yes[[i]], col = 2) 
}

## Ketika melihat distribusi Glukosa, kita dapat melihat bahwa itu secara kasar didistribusikan secara normal karena titik-titik jatuh pada garis merah.
## Kita dapat melihat bahwa ekor-ekor sebaran lebih tebal dari pada ekor-ekor dari sebaran normal karena titik-titik menyimpang dari garis di kiri dan di kanan dari plot.

## Sebagai pemeriksaan tambahan, kita dapat melakukan uji Levene untuk memeriksa varians yang sama.

## Kita dapat melihat bahwa distribusi variabel Kehamilan tidak berdistribusi normal karena titik-titiknya sangat menyimpang dari garis.

## Insulin dan Umur juga tidak terdistribusi secara normal.
## Pola kurva pada plot berbentuk busur yang menunjukkan kemiringan.
## Titik-titiknya berada di atas garis, lalu di bawahnya, dan kemudian di atasnya lagi.
## Ini menunjukkan bahwa kemiringannya ke kanan.
variable_2 <- c("BMI", "SkinThickness", "BloodPressure", "Pedigree") 
par(mfrow = c(2, 2)) 
for(i in variable_2) { 
  qqnorm(diab.yes[[i]]); qqline(diab.yes[[i]], col = 2) 
}

## Variabel SkinThickness dan BloodPressure terlihat seperti terdistribusi normal.
## Ayunan ke bawah di kiri dan ayunan ke atas di kanan plot untuk variabel tekanan darah menunjukkan bahwa distribusinya lebih berekor lebih berat daripada distribusi normal teoretis.

## Untuk variabel BMI, poin berayun ke atas secara substansial di sebelah kanan plot. Poin-poin ini mungkin outlier tetapi kami tidak dapat menyimpulkannya dengan melihat plot-plot ini.
## Distribusi untuk variabel PedigreeFunction terlihat lagi miring ke kanan dan tidak terdistribusi normal.

## Jika seseorang ingin menguji normalitas, fungsi shapito.test adalah pilihan.
## Berikut adalah contoh cara menggunakannya:
shapiro.test(diab.yes$SkinThickness)

## Hipotesis nolnya adalah bahwa data terdistribusi secara normal.
## Berdasarkan hasil, kami gagal menolak hipotesis nol dan menyimpulkan bahwa data terdistribusi normal untuk SkinThickness.

# Non-Diabetes Group
par(mfrow = c(2, 2)) 
for(i in variable_1) { 
  qqnorm(diab.no[[i]]); qqline(diab.no[[i]], col = 2) 
}

## Semua variabel tampaknya tidak terdistribusi normal
## dan semua distribusi tampak miring ke kanan berdasarkan bentuk busur titik-titik di keempat plot.
par(mfrow = c(2, 2)) 
for(i in variable_2) { 
  qqnorm(diab.no[[i]]); qqline(diab.no[[i]], col = 2) 
}

## BMI, SkinThickness, dan BloodPressure tampaknya secara kasar didistribusikan secara normal sedangkan PedigreeFunction memiliki bentuk busur ini yang menunjukkan bahwa itu miring ke kanan.
# Teknik visualisasi lainnya adalah dengan memplot kepadatan prediktor untuk setiap kelompok.
## Melalui plot di bawah ini, kita dapat mendeteksi apakah prediktor di setiap kelompok terdistribusi normal dan kita juga dapat memeriksa varians yang sama.
plot <- list() 
for(i in names(diabetes)[-9]) { 
  plot[[i]] <- ggplot(diabetes, aes_string(x = i, y = "..density..", col = "Outcome")) + 
    geom_density(aes(y = ..density..)) + 
    scale_color_manual(values = c("blue", "red")) + 
    theme(legend.position = "none") 
} 
do.call(grid.arrange, c(plot, nrow = 4))

## Dari plot di atas kita dapat menyimpulkan, bahwa banyak distribusi yang miring ke kanan dan variansnya sering juga berbeda.

# Linear Discriminant Analysis
lda_model <- MASS::lda(Outcome ~., data = diabetes) 
preds <- predict(lda_model) 
head(preds$posterior)

## Output di atas menunjukkan kemungkinan diklasifikasikan ke dalam kelompok "Diabetes" atau "Tanpa Diabetes".
## Misalnya, observasi satu belum diuji positif diabetes dengan probabilitas 98%.
## Pengamatan dua telah didiagnosis menderita diabetes dengan probabilitas 87%.
## Model ini menggunakan ambang 50% untuk probabilitas posterior.
lda_model

## Dari output, dapat membaca probabilitas sebelumnya yang pertama 0,332 dan kedua 0,668
## Ini berarti bahwa sekitar 33% dari kumpulan data mencakup orang yang telah didiagnosis dengan diabetes
## dan 66,8% yang belum didiagnosis dengan diabetes.

## Di bawah probabilitas sebelumnya, kita dapat melihat rata-rata grup dari setiap prediktor dalam setiap kelas.
## Rata-rata orang yang terdiagnosis diabetes hamil 4,5 kali, memiliki kadar glukosa 145,2, tekanan darah 74,1 (mm hg), ketebalan lipatan kulit trisep 33 mm, kadar insulin 207 mu U/ml, a BMI 35,8 (berat dalam kg/(tinggi dalam m)^2),
## fungsi silsilah diabetes 0,63, dan berusia 36 tahun.
# Skor di bawah rata-rata kelompok digunakan untuk mengklasifikasikan pengamatan menjadi “Diabetes” dan “Tanpa Diabetes”.
plot(lda_model)

## Lebih khusus, skor, atau koefisien output dari diskriminan linier, adalah kombinasi linier yang membentuk aturan keputusan LDA.
## Ketika kombinasi linier dari koefisien-koefisien ini negatif, maka kemungkinan meningkat bahwa pengamatan menderita diabetes (lihat plot),
## sedangkan bila kombinasi liniernya positif, pengamatan lebih mungkin termasuk dalam kelompok “Tanpa Diabetes”.
diabetes <- data.frame(diabetes, predicted = preds$class) 
xtabs(~ predicted + Outcome, data = diabetes)

# prediction accuracy 
round((232+76)/(392), 4)

# error 
round((30+54)/(392), 4)

## Kami mendapatkan kesalahan prediksi sekitar 21,4%. Sensitivitas kami (tingkat positif sejati) hanya 76/(76+54) = 58,5%.
## Kami memiliki spesifisitas (tingkat negatif benar) dari 232/(232+30) = 88,5%.

# ROC Curve
## Dengan kurva ROC, kita bisa bermain sedikit dengan sensitivitas dan spesifisitas kita.
## Ingat, kami menggunakan ambang 50% untuk probabilitas posterior kami.
## Kurva ROC adalah plot dari true positive rate (TPR) versus false-positive rate (FPR) saat kita memvariasikan ambang batas.
## Jadi kami memiliki trade-off antara tingkat positif benar dan palsu di mana kami dapat mengubah ambang batas.
## Ambang batas ideal akan berada di sudut kiri atas plot ROC.
## Oleh karena itu, kami ingin memilih ambang batas sehingga titik tertentu pada kurva ini paling dekat dengan sudut kiri atas.
posterior_yes <- preds$posterior[, "Diabetes"]
true_yes <- (diabetes$Outcome == "Diabetes") %>% 
  as.numeric() %>%  
  factor()
ROC_res <- roc(posterior_yes, true_yes)
tidy_ROC <- tidy(ROC_res)
ggplot(tidy_ROC, aes(x = fpr, y = tpr)) + 
  geom_point(pch = ".") + 
  geom_vline(xintercept = 0.15)

tidy_ROC %>% 
  filter(fpr >= 0.148, fpr <= 0.151)

n <- nrow(diabetes) 
thresh <- 0.3620957 
dclass <- rep("No Diabetes", n) 
dclass[posterior_yes > thresh] <- "Diabetes" 
outcome <- data.frame(diabetes,prednew = dclass)
xtabs(~ prednew + Outcome, data = outcome)

(223+92)/(392)

## Di atas, kami memilih ambang batas sekitar 0,36.
## Tingkat positif kami yang sebenarnya telah meningkat dari 76 menjadi 92 pengamatan yang diklasifikasikan dengan benar sebagai kelompok "Diabetes".
## Tingkat negatif palsu kami telah menurun dan kami salah mengklasifikasikan 39 orang ke dalam kelompok "Diabetes",
## meskipun mereka belum didiagnosis menderita diabetes.
## Perhatikan, bahwa secara keseluruhan, bagaimanapun, kami telah menurunkan tingkat kesalahan kami.

# Quadratic Discriminant Analysis
qda_model <- qda(Outcome ~., data = diabetes)
preds <- predict(qda_model)
head(preds$posterior)

diabetes <- data.frame(diabetes, predicted = preds$class) 
xtabs(~ predicted + Outcome, data = diabetes)

(230+86)/(392)

posterior_yes <- preds$posterior[, "Diabetes"]
true_yes <- (diabetes$Outcome == "Diabetes") %>% 
  as.numeric() %>% 
  factor()
ROC_res <- roc(posterior_yes, true_yes)
tidy_ROC <- tidy(ROC_res)
ggplot(tidy_ROC, aes(x = fpr, y = tpr)) + 
  geom_point(pch = ".") + 
  geom_vline(xintercept = 0.135)
posterior_yes <- preds$posterior[, "Diabetes"] 
true_yes <- (diabetes$Outcome == "Diabetes") %>% 
  as.numeric() %>% 
  factor() 
ROC_res <- roc(posterior_yes, true_yes) 
tidy_ROC <- tidy(ROC_res) 
ggplot(tidy_ROC, aes(x = fpr, y = tpr)) + 
  geom_point(pch = ".") + 
  geom_vline(xintercept = 0.135)

tidy_ROC %>% 
  filter(fpr >= 0.13, fpr <= 0.14)

n <- nrow(diabetes)
thresh <- 0.4432782 
dclass <- rep("No Diabetes", n) 
dclass[posterior_yes > thresh] <- "Diabetes"
outcome <- data.frame(diabetes, prednew = dclass) 
xtabs(~ prednew + Outcome, data = outcome)

(91+226)/(392)

## Untuk analisis diskriminan kuadrat, tidak ada yang jauh berbeda dari analisis diskriminan linier dalam hal kode.
## Algoritma analisis diskriminan kuadrat menghasilkan tingkat klasifikasi terbaik.

## Kami bermain lagi dengan kurva ROC dan menentukan ambang batas yang menghasilkan tingkat klasifikasi yang lebih baik bagi kami.
## Ini mungkin karena fakta bahwa matriks kovarians berbeda atau karena batas keputusan yang sebenarnya tidak linier.
