install.packages("openxlsx")
library(openxlsx)

biomarkers <- read.xlsx("biomarkers.xlsx", sheet = 1)
covariates <- read.xlsx("covariates.xlsx", sheet = 1)

install.packages("tidyr")
library(tidyr)

biomarkers %>% separate(Biomarker,
                        into = c("PatientID",
                                 "timepoint"),
                        sep = "-") -> biomarkers

install.packages("dplyr")
library(dplyr)

options(max.print = 10000)
biomarkers %>% mutate(PatientID =
                        as.numeric(PatientID)) %>% 
  arrange(PatientID)

data_merged <- merge(biomarkers, covariates, 
                     all.x = TRUE,
                     by = "PatientID")

# Hypothesis Test and bonferroni correction
x <- data_merged[ , c(15)]
y <- data_merged[ , c(16)]
t.test(x, y)

pairwise.t.test(x, y, p.adjust = "bonferroni")

# Linear Regression 
#part(1) 80% of the patients from 1 to 278
sex <-data_merged[1:278, c(13)]
sex <-factor(sex,levels = c(1,2),labels = c("Male","Female"))
str(sex)

smoker <-data_merged[1:278, c(14)]
smoker <-factor(smoker, levels=c(1,2),labels=c("Yes","No"))
str(smoker)

df <- data.frame(y1=data_merged[1:278,c(16)],
                 x1=data_merged[1:278,c(12)],
                 x2=sex,
                 x3=smoker,
                 x4=data_merged[1:278,c(15)])
                 
model <- lm(y1 ~ x1 + x2 + x3 + x4, data=df)
summary(model)

#part(2) Predicting 20% of the patients from 279 to 348
sex <-data_merged[279:348, c(13)]
smoker <-data_merged[279:348, c(14)]
df2 <- data.frame(y2=data_merged[279:348, c(16)],
                  z1=data_merged[279:348, c(12)],
                  z2=sex,
                  z3=smoker,
                  z4=data_merged[279:348, c(15)])

model2 <- lm(y2 ~ z1 + z2 + z3 + z4, data=df2)
predict(model2, df2, interval="prediction")


