# Inizialitation
# Load libraries
library('kableExtra') # To print table in report
library('ggsci') # For color palettes 
library('glue') # For improved print
library('DataExplorer') # For Exploratory Data Analysis
library('patchwork') # To easily plot multiple graphs
library('ROSE') # For oversampling
library('FactoMineR') # For PCA Analysis
library('factoextra') # For PCA Analysis
library('tidyverse') # For Data Cleaning, EDA, Data Visualization
library('dplyr')

# Load the dataset
df <- read_csv("D:/Kuliah/R dan Python/Clustering/train_2v.csv")

# Create output directory
dir.create("D:/Kuliah/R dan Python/Clustering/EDA")
dir.create("D:/Kuliah/R dan Python/Clustering/Independent_variables")
dir.create("D:/Kuliah/R dan Python/Clustering/Logistic_regression")
dir.create("D:/Kuliah/R dan Python/Clustering/PCA")
dir.create("D:/Kuliah/R dan Python/Clustering/Table")

# Environement and packages informations
# Print the version of the essential packages used for the analysis
packages_list <- c('tidyverse', 'DataExplorer', 'FactoMineR', 'factoextra', 'patchwork', 'caret', 'ROSE')

Name <- 'tidyverse'
Version <- getNamespaceVersion('tidyverse')

packages_informations <- tibble(Name, Version)

packages_list <- packages_list[2:length(packages_list)]

for(package in packages_list){
  Name <- package
  Version <- getNamespaceVersion(package)
  db <- tibble(Name, Version)
  packages_informations <- rbind(packages_informations, db)
}

packages_informations <- packages_informations %>% arrange(Name) 
write_csv(packages_informations, 'D:/Kuliah/R dan Python/Clustering/packages_informations.csv')

# Exploratory Data Analysis
# Check if the dataset is tidy
df %>% head()

# Is there missing value in the dataset?
pdf('D:/Kuliah/R dan Python/Clustering/EDA/plot_missing.pdf')        
plot_missing(df, ggtheme = theme_minimal())
dev.off()

# Strategy for missing values
df <- df %>% filter(!is.na(smoking_status)) # Exclude from the analysis patient with unknown smoking status
df <- df %>% mutate(bmi = ifelse(is.na(bmi), median(bmi, na.rm=TRUE), bmi)) # replace missing bmi value by the median of the bmi
df <- df %>% filter(gender == 'Male' | gender == 'Female') #
df$id <- NULL # Remove the ID column

df <- df %>%
  mutate(
    stroke = ifelse(stroke == 0, 'no stroke', 'stroke'),
    hypertension = ifelse(hypertension == 0, 'no hypertension', 'hypertension'),
    heart_disease  = ifelse(heart_disease == 0, 'no heart disease', 'heart disease')
  )

# Trasnfrom categorical variable as factor
df$stroke <- as.factor(df$stroke)
df$gender <-  as.factor(df$gender)
df$ever_married <- as.factor(df$ever_married)
df$work_type <- as.factor(df$work_type)
df$Residence_type <- as.factor(df$Residence_type)
df$smoking_status <- as.factor(df$smoking_status)
df$hypertension <- as.factor(df$hypertension)
df$heart_disease <- as.factor(df$heart_disease)

# How many patients have stroke?
total_observation <- nrow(df)
disease_tables <- df %>%
  group_by(stroke) %>%
  dplyr::count() %>% # Be carefull count() to not work because of MatrixStat package
  mutate(proportion = n/total_observation * 100) 
write_csv(disease_tables, 'D:/Kuliah/R dan Python/Clustering/Table/disease_tables_df.csv')

# Are the gender balanced?
# Calculate the table of gender repartition by disease status
total_observation <- nrow(df)
t1 <- df %>%
  group_by(gender, stroke) %>%
  dplyr::count() %>%
  mutate(
    percentage = n/total_observation * 100)

# Summarize the age per gender and disease status
t2 <- df %>%
  group_by(gender, stroke) %>%
  summarise(
    age_mean = mean(age),
    age_sd = sd(age),
    n = n()
  )
t3 <- inner_join(t1, t2)
t3

# Visualize Data Distribution, Asses normality
glue("{ncol(df)} variables and {nrow(df)} observatoins")
glue("Variable names:")
glue("{colnames(df)}")
categorical_data <- c("gender", "hypertension", "heart_disease", "ever_married", "work_type", "Residence_type", "smoking_status", "stroke")
pdf('D:/Kuliah/R dan Python/Clustering/EDA/categorical_variables.pdf')
plot_bar(df[categorical_data], ggtheme = theme_minimal())
dev.off()

continous_data <- c("age", "avg_glucose_level", "bmi")
pdf('D:/Kuliah/R dan Python/Clustering/EDA/continous_variables.pdf')
plot_density(df[continous_data], ggtheme = theme_minimal())
dev.off()

pdf('D:/Kuliah/R dan Python/Clustering/EDA/QQ_plots.pdf')
plot_qq(df[continous_data], ggtheme = theme_minimal(), sampled_rows = 100L)
dev.off()

df <- df %>% mutate(bmi = log(bmi))

pdf('D:/Kuliah/R dan Python/Clustering/EDA/Log_BMI_QQ_plot.pdf')
plot_qq(df[c('bmi')], ggtheme = theme_minimal(), sampled_rows = 100L)
dev.off()

# Solving Sampling Issue: ROSE
# Over sample the data so stroke represent 50% of the total case.
data_oversampled <- ovun.sample(stroke ~ ., data = df, method = "over", N = 58940) # Increase the amount of stroke observation until it reach 29470, 58940 total observation
df_rose <- data_oversampled$data

# table(df_rose$stroke)
total_observation <- nrow(df_rose)
disease_tables <- df_rose %>%
  group_by(stroke) %>%
  dplyr::count() %>% # Be carefull count() to not work because of MatrixStat package
  mutate(proportion = n/total_observation * 100) 
write_csv(disease_tables, 'D:/Kuliah/R dan Python/Clustering/Table/disease_tables_df_rose.csv')

# FAMD - Factor Analysis of Mixed Data in R
res.famd <- FAMD(df_rose, graph = FALSE)
p1 <- fviz_screeplot(res.famd)
ggsave('D:/Kuliah/R dan Python/Clustering/PCA/scree_plot.png', p1)

# Plot of variables
p1 <- fviz_famd_var(res.famd, repel = TRUE)

# Contribution to the first dimension
p2 <- fviz_contrib(res.famd, "var", axes = 1)

# Contribution to the second dimension
p3 <- fviz_contrib(res.famd, "var", axes = 2)

ggsave('D:/Kuliah/R dan Python/Clustering/PCA/variables_FAMD.png', p1)

p4 <- (p2 / p3)
ggsave('D:/Kuliah/R dan Python/Clustering/PCA/contribution_of_variables.png', p4)
plot(p4)

# quanti.var <- get_famd_var(res.famd, "quanti.var")
# quanti.var 
p1 <- fviz_famd_var(res.famd, "quanti.var", col.var = "contrib", 
                    gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                    repel = TRUE)
ggsave('D:/Kuliah/R dan Python/Clustering/PCA/quantitative_variables.png', p1)

quali.var <- get_famd_var(res.famd, "quali.var")

p1 <- fviz_famd_var(res.famd, "quali.var", col.var = "contrib",
                    gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                    repel = TRUE
)
ggsave('D:/Kuliah/R dan Python/Clustering/PCA/qualitative_variables.png', p1)
plot(p1)

famd.stroke <- fviz_mfa_ind(res.famd, 
                            habillage = "stroke", # color by groups
                            geom = c('point'),
                            palette = pal_simpsons("springfield", alpha = 0.6)(16),
                            title = "Stroke Status"
) 
ggsave('D:/Kuliah/R dan Python/Clustering/PCA/famd.stroke.png', famd.stroke)
plot(famd.stroke)

famd.smoking <- fviz_mfa_ind(res.famd, 
                             habillage = "smoking_status", # color by groups
                             geom = c('point'),
                             palette = pal_simpsons("springfield", alpha = 0.6)(16),
                             title = "Smoking Status"
) 
ggsave('D:/Kuliah/R dan Python/Clustering/PCA/famd.smoking.png', famd.smoking)

famd.married <- fviz_mfa_ind(res.famd, 
                             habillage = "ever_married", # color by groups
                             geom = c('point'),
                             palette = pal_simpsons("springfield", alpha = 0.6)(16),
                             title = "Martial Status"
) 
ggsave('D:/Kuliah/R dan Python/Clustering/PCA/famd.married.png', famd.married)

famd.work_type <- fviz_mfa_ind(res.famd, 
                               habillage = "work_type", # color by groups
                               geom = c('point'), 
                               palette = pal_simpsons("springfield", alpha = 0.6)(16),
                               title = "Work Status"
) 
ggsave('D:/Kuliah/R dan Python/Clustering/PCA/famd.work_type.png', famd.work_type)

famd.residence <- fviz_mfa_ind(res.famd, 
                               habillage = "Residence_type", # color by groups
                               geom = c('point'), # rempove labels
                               palette = pal_simpsons("springfield", alpha = 0.6)(16), # use a color blind friendly palette 
                               title = "Residence type"
) 
ggsave('D:/Kuliah/R dan Python/Clustering/PCA/famd.residence.png', famd.residence)

famd.heart_disease <- fviz_mfa_ind(res.famd, 
                                   habillage = "heart_disease", # color by groups
                                   geom = c('point'), 
                                   palette = pal_simpsons("springfield", alpha = 0.6)(16),
                                   title = "Heart Disease Status"
)
ggsave('D:/Kuliah/R dan Python/Clustering/PCA/famd.heart_disease.png', famd.heart_disease)

famd.hypertension <- fviz_mfa_ind(res.famd, 
                                  habillage = "hypertension", # color by groups
                                  geom = c('point'),
                                  palette = pal_simpsons("springfield", alpha = 0.6)(16),
                                  title = "Hypertension Status"
)
ggsave('D:/Kuliah/R dan Python/Clustering/PCA/famd.hypertension.png', famd.hypertension)

all_FAMD <- (famd.smoking + famd.married) / (famd.work_type + famd.residence) / (famd.heart_disease + famd.hypertension)
ggsave('D:/Kuliah/R dan Python/Clustering/PCA/all_FAMD.png', all_FAMD)
plot(all_FAMD)

famd.gender <- fviz_mfa_ind(res.famd, 
                            habillage = "gender", # color by groups
                            geom = c('point'),
                            palette = pal_simpsons("springfield", alpha = 0.6)(16),
                            title = "Gender"
) 
ggsave('D:/Kuliah/R dan Python/Clustering/PCA/famd.gender.png', famd.gender)

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
scale_fill_manual(values=cbPalette)

# Stroke incidence vs age
p1 <- df_rose %>%
  ggplot(aes(y = stroke, x = age, fill = stroke)) +
  geom_boxplot(notch = TRUE) +     
  scale_fill_manual(values=cbPalette) +
  theme_bw() +
  guides(fill='none') +
  ylab('') +
  ggtitle('Transformed dataset')

p2 <- df %>%
  ggplot(aes(y = stroke, x = age, fill = stroke)) +
  geom_boxplot(notch = TRUE) +     
  scale_fill_manual(values=cbPalette) +
  theme_bw() +
  guides(fill='none') +
  ylab('') +
  ggtitle('Original dataset')

p3 <- p1 | p2
ggsave('D:/Kuliah/R dan Python/Clustering/Independent_variables/stroke_vs_age.png', p3)
plot(p3)

age_summary <- df %>%
  group_by(stroke) %>%
  summarise(
    mean = mean(age),
    sd = sd(age),
    n = n()
  )

write_csv(age_summary, 'D:/Kuliah/R dan Python/Clustering/Independent_variables/age_summary.csv')

# Stroke incidence vs average glucose level
p1 <- df_rose %>%
  ggplot(aes(y = stroke, x = avg_glucose_level, fill = stroke)) +
  geom_boxplot(notch = TRUE) +     
  scale_fill_manual(values=cbPalette) +
  theme_bw() +
  guides(fill='none') +
  ylab('') +
  xlab('Glucose Level') +
  ggtitle('Transformed dataset')


p2 <- df %>%
  ggplot(aes(y = stroke, x = avg_glucose_level, fill = stroke)) +
  geom_boxplot(notch = TRUE) +     
  scale_fill_manual(values=cbPalette) +
  theme_bw() +
  guides(fill='none') +
  ylab('') +
  xlab('Glucose Level') +
  ggtitle('Original dataset')

p3 <- p1 | p2
ggsave('D:/Kuliah/R dan Python/Clustering/Independent_variables/stroke_vs_avg_glucose_level.png', p3)
plot(p3)

avg_glucose_level_summary <- df %>%
  group_by(stroke) %>%
  summarise(
    mean = mean(avg_glucose_level),
    sd = sd(avg_glucose_level),
    n = n()
  )
write_csv(avg_glucose_level_summary, 'D:/Kuliah/R dan Python/Clustering/Independent_variables/avg_glucose_level_summary.csv')

# Stroke incidence vs gender
p1 <- df_rose %>%
  ggplot(aes(x = gender, fill = stroke)) +
  geom_bar(position = 'fill') +
  scale_fill_manual(values=cbPalette) +
  theme_bw() +
  xlab('Gender') +
  ggtitle('Transformed dataset')

p2 <- df %>%
  ggplot(aes(x = gender, fill = stroke)) +
  geom_bar(position = 'fill') +
  scale_fill_manual(values=cbPalette) +
  theme_bw() +
  xlab('Gender') +
  ggtitle('Original dataset')

p3 <- p1 | p2
ggsave('D:/Kuliah/R dan Python/Clustering/Independent_variables/stroke_vs_gender.png', p3)
plot(p3)

# Stroke incidence vs marital status
p1 <- df_rose %>%
  ggplot(aes(x = ever_married, fill = stroke)) +
  geom_bar(position = 'fill') +
  scale_fill_manual(values=cbPalette) +
  theme_bw() +
  xlab('Ever Married') +
  ggtitle('Transformed dataset')

p2 <- df %>%
  ggplot(aes(x = ever_married, fill = stroke)) +
  geom_bar(position = 'fill') +
  scale_fill_manual(values=cbPalette) +
  theme_bw() +
  xlab('Ever Married') +
  ggtitle('Original dataset')

p3 <- p1 | p2
ggsave('D:/Kuliah/R dan Python/Clustering/Independent_variables/stroke_vs_ever_married.png', p3)
plot(p3)

p1 <- df_rose %>%
  ggplot(aes(y = ever_married, x = age, fill = ever_married)) +
  geom_boxplot(notch = TRUE) +     
  scale_fill_manual(values=cbPalette) +
  theme_bw() +
  guides(fill='none') +
  ylab('Ever Married') +
  xlab('Age') +
  ggtitle('Transformed dataset')


p2 <- df %>%
  ggplot(aes(y = ever_married, x = age, fill = ever_married)) +
  geom_boxplot(notch = TRUE) +     
  scale_fill_manual(values=cbPalette) +
  theme_bw() +
  guides(fill='none') +
  ylab('Ever Married') +
  xlab('Age') +
  ggtitle('Original dataset')

p3 <- p1 | p2
ggsave('D:/Kuliah/R dan Python/Clustering/Independent_variables/stroke_vs_age.png', p3)
plot(p3)

age_summary <- df %>%
  group_by(ever_married) %>%
  summarise(
    mean = mean(age),
    sd = sd(age),
    n = n()
  )

write_csv(age_summary, 'D:/Kuliah/R dan Python/Clustering/Independent_variables/age_ever_married_summary.csv')

# Stroke incidence vs smoking status
p1 <- df_rose %>%
  ggplot(aes(x = smoking_status, fill = stroke)) +
  geom_bar(position = 'fill') +
  scale_fill_manual(values=cbPalette) +
  theme_bw() +
  xlab('Smoking Status') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle('Transformed dataset')

p2 <- df %>%
  ggplot(aes(x = smoking_status, fill = stroke)) +
  geom_bar(position = 'fill') +
  scale_fill_manual(values=cbPalette) +
  theme_bw() +
  xlab('Smoking Status') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle('Original dataset')

p3 <- p1 | p2
ggsave('D:/Kuliah/R dan Python/Clustering/Independent_variables/stroke_vs_smoking_status.png', p3)
plot(p3)

p1 <- df_rose %>%
  ggplot(aes(y = smoking_status, x = age, fill = smoking_status)) +
  geom_boxplot(notch = TRUE) +     
  scale_fill_manual(values=cbPalette) +
  theme_bw() +
  guides(fill='none') +
  ylab('Smoking Status') +
  xlab('Age') +
  ggtitle('Transformed dataset')


p2 <- df %>%
  ggplot(aes(y = smoking_status, x = age, fill = smoking_status)) +
  geom_boxplot(notch = TRUE) +     
  scale_fill_manual(values=cbPalette) +
  theme_bw() +
  guides(fill='none') +
  ylab('Smoking Status') +
  xlab('Age') +
  ggtitle('Original dataset')

p3 <- p1 | p2
ggsave('D:/Kuliah/R dan Python/Clustering/Independent_variables/stroke_vs_age.png', p3)
plot(p3)

age_summary <- df %>%
  group_by(ever_married) %>%
  summarise(
    mean = mean(age),
    sd = sd(age),
    n = n()
  )

write_csv(age_summary, 'D:/Kuliah/R dan Python/Clustering/Independent_variables/age_smoking_status_summary.csv')

p1 <- df_rose %>%
  mutate(Ever_Smoked = ifelse((smoking_status == 'smokes' | smoking_status == 'formerly smoked'), 'Yes', 'No'))  %>%
  ggplot(aes(x = Ever_Smoked, fill = stroke)) +
  geom_bar(position = 'fill') +
  scale_fill_manual(values=cbPalette) +
  theme_bw() +
  xlab('Ever Smoked') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle('Transformed dataset')

p2 <- df %>%
  mutate(Ever_Smoked = ifelse((smoking_status == 'smokes' | smoking_status == 'formerly smoked'), 'Yes', 'No'))  %>%
  ggplot(aes(x = Ever_Smoked, fill = stroke)) +
  geom_bar(position = 'fill') +
  scale_fill_manual(values=cbPalette) +
  theme_bw() +
  xlab('Ever Smoked') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle('Original dataset')

p3 <- p1 | p2
plot(p3)

ggsave('D:/Kuliah/R dan Python/Clustering/Independent_variables/stroke_vs_ever_smoked.png', p3)

# Stroke incidence vs hypertension
p1 <- df_rose %>%
  ggplot(aes(x = hypertension, fill = stroke)) +
  geom_bar(position = 'fill') +
  scale_fill_manual(values=cbPalette) +
  theme_bw() +
  xlab('Hypertension') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle('Transformed dataset')

p2 <- df %>%
  ggplot(aes(x = hypertension, fill = stroke)) +
  geom_bar(position = 'fill') +
  scale_fill_manual(values=cbPalette) +
  theme_bw() +
  xlab('Hypertension') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle('Original dataset')

p3 <- p1 | p2
ggsave('D:/Kuliah/R dan Python/Clustering/Independent_variables/stroke_vs_hypertension.png', p3)

p1 <- df_rose %>%
  ggplot(aes(y = hypertension, x = age, fill = hypertension)) +
  geom_boxplot(notch = TRUE) +     
  scale_fill_manual(values=cbPalette) +
  theme_bw() +
  guides(fill='none') +
  ylab('') +
  xlab('Age') +
  ggtitle('Transformed dataset')


p2 <- df %>%
  ggplot(aes(y = hypertension, x = age, fill = hypertension)) +
  geom_boxplot(notch = TRUE) +     
  scale_fill_manual(values=cbPalette) +
  theme_bw() +
  guides(fill='none') +
  ylab('') +
  xlab('Age') +
  ggtitle('Original dataset')

p3 <- p1 | p2
ggsave('D:/Kuliah/R dan Python/Clustering/Independent_variables/hypertension_vs_age.png', p3)
plot(p3)

write_csv(age_summary, 'D:/Kuliah/R dan Python/Clustering/Independent_variables/hypertension_summary.csv')

# Stroke incidence vs heart disease
p1 <- df_rose %>%
  ggplot(aes(x = heart_disease, fill = stroke)) +
  geom_bar(position = 'fill') +
  scale_fill_manual(values=cbPalette) +
  theme_bw() +
  xlab('Hypertension') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle('Transformed dataset')

p2 <- df %>%
  ggplot(aes(x = heart_disease, fill = stroke)) +
  geom_bar(position = 'fill') +
  scale_fill_manual(values=cbPalette) +
  theme_bw() +
  xlab('Hypertension') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle('Original dataset')

p3 <- p1 | p2
ggsave('D:/Kuliah/R dan Python/Clustering/Independent_variables/stroke_vs_heart_disease.png', p3)
plot(p3)

# Find variable associated with stroke - Logistic regression
df_selected <- df_rose %>%
  mutate(ever_smoked = ifelse((smoking_status == 'smokes' | smoking_status == 'formerly smoked'), 'Yes', 'No'))

df_selected <- df_selected %>% 
  select(age, gender, avg_glucose_level, ever_smoked, hypertension, heart_disease, stroke)

df_selected$ever_smoked <- as.factor(df_selected$ever_smoked)

df_selected_features <- df_selected %>% select(-stroke) %>% mutate_if(is.factor, as.numeric) # Recode all categorical data to numeric
df_selected_target <- df_selected %>% select(stroke) 

df_selected <- bind_cols(df_selected_features, df_selected_target)

levels(df_selected$stroke)

model <- glm(stroke ~ age + gender + avg_glucose_level + ever_smoked + hypertension + heart_disease, data = df_selected, family = binomial)
# exp(coef(model))
summary(model)

## Now calculate the overall "Pseudo R-squared" and its p-value
ll.null <- model$null.deviance/-2
ll.proposed <- model$deviance/-2

## McFadden's Pseudo R^2 = [ LL(Null) - LL(Proposed) ] / LL(Null)
glue("McFadden's Pseudo R^2: {(ll.null - ll.proposed) / ll.null}")

## The p-value for the R^2
glue("R^2 p-value: {1 - pchisq(2*(ll.proposed - ll.null), df=(length(model$coefficients)-1))}")

predicted.data <- data.frame(
  probability.of.stroke=model$fitted.values,
  stroke=df_selected$stroke)

predicted.data <- predicted.data[
  order(predicted.data$probability.of.stroke, decreasing=FALSE),]
predicted.data$rank <- 1:nrow(predicted.data)

p1 <- ggplot(data=predicted.data, aes(x=rank, y=probability.of.stroke)) +
  geom_point(aes(color=stroke), alpha=1, shape=4, stroke=2) +
  scale_colour_manual(values=cbPalette) +
  xlab("Index") +
  ylab("Predicted probability of getting a stroke") +
  theme_bw()
ggsave('D:/Kuliah/R dan Python/Clustering/Logistic_regression/logisitc_regression_evaluation.png', p1)
plot(p1)

# Visualize the risk of stroke for the 5 risk factors identified
# Save a plot of the 5 selected risk factors from the transformed data frame

p1 <- df_rose %>%
  ggplot(aes(y = stroke, x = age, fill = stroke)) +
  geom_boxplot(notch = TRUE) +     
  scale_fill_manual(values=cbPalette) +
  theme_bw() +
  guides(fill=FALSE) +
  xlab('Age') +
  ylab('Stroke') +
  ggtitle('Age')

p2 <- df_rose %>%
  ggplot(aes(y = stroke, x = avg_glucose_level, fill = stroke)) +
  geom_boxplot(notch = TRUE) +     
  scale_fill_manual(values=cbPalette) +
  theme_bw() +
  guides(fill='none') +
  ylab('Stroke') +
  xlab('Avegrage glucose Level') +
  ggtitle('Glucose Level')

p3 <- df_rose %>%
  mutate(Ever_Smoked = ifelse((smoking_status == 'smokes' | smoking_status == 'formerly smoked'), 'Yes', 'No'))  %>%
  ggplot(aes(x = Ever_Smoked, fill = stroke)) +
  geom_bar(position = 'fill') +
  scale_fill_manual(values=cbPalette) +
  theme_bw() +
  xlab('Ever Smoked') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle('Smoking history')

p4 <-  df_rose %>%
  ggplot(aes(x = hypertension, fill = stroke)) +
  geom_bar(position = 'fill') +
  scale_fill_manual(values=cbPalette) +
  theme_bw() +
  xlab('Hypertension') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle('Hypertension')

p5 <- df_rose %>%
  ggplot(aes(x = heart_disease, fill = stroke)) +
  geom_bar(position = 'fill') +
  scale_fill_manual(values=cbPalette) +
  theme_bw() +
  xlab('Heart Disesae') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle('Heart Disesae History')

p6 <- df_rose %>%
  ggplot(aes(y = stroke, x = bmi, fill = stroke)) +
  geom_boxplot(notch = TRUE) +     
  scale_fill_manual(values=cbPalette) +
  theme_bw() +
  guides(fill='none') +
  xlab('BMI') +
  ylab('Stroke') +
  ggtitle('BMI')

p7 <- (p1 + p2) / (p3 + p4) / (p5 + plot_spacer())
ggsave('D:/Kuliah/R dan Python/Clustering/Independent_variables/risks_factors.png', p7)
plot(p7)
