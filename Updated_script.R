## load libraries
#to process data
library(tidyverse)
#to do basic stats
library(car)
#to do post hoc tests
library(emmeans)
#to load data
library(readr)
#to make pretty graphs
library(ggplot2)


#load data
#it helps to not have spaces or special symbols in your col names
MBC_CNP_data <- read.csv("~/R/Niraj_data/MBC_CNP_data.csv")
XRF_Data <- read.csv("~/R/Niraj_data/Pot_ExP_Data_For_ph_ec_soil.csv")
names(XRF_Data)[34]<-'Si_perc'
names(XRF_Data)[79]<-'Zn_ppm'
names(XRF_Data)[3]<-'Plant_type'

## read and inspect data (use read_csv because of symbols in column names)
head(XRF_Data)
colnames(XRF_Data)

## zeros in the dataframe, all from high N treatments -- should these be NAs?
#seems like you changed this manually
# x <- which(rowSums(select(XRF_Data, Si_perc:Zn_ppm)) == 0)
# XRF_Data[x, ]
# XRF_Data <- XRF_Data[-x, ]  # drop those rows

# Check if 'species' variable exists in your dataset
print(names(XRF_Data))

# Ensure that the 'species' variable is correctly spelled and formatted
# If not, correct it
# For example, if 'Species' is the correct variable name:
# XRF_Data$Species<- XRF_Data$Species

# Clean your data if necessary
# For example, removing rows with missing values
# XRF_Data <- na.omit(XRF_Data)
## plot P data by Plant.type, tissue and treatment
ggplot(XRF_Data, aes(x=group, y=P_.ppm., colour=Dose)) + 
  geom_boxplot() + 
  facet_wrap(~Plant_type, scale='free')


## fit model and compare treatments
# fit model
m1 <- lm(P_.ppm. ~ Dose * Plant_type, data=XRF_Data)
# check residuals to see if transformation needed
residualPlot(m1)
qqPlot(m1)
# test significance
Anova(m1)
# vif(m1)

# comparing means by dose within species (ignores sample.identity)
m1.emm <- emmeans(m1, ~ Dose | Plant_type)
letters<-multcomp::cld(m1.emm,alpha = 0.05, Letters = letters)

model_means_cld<-subset(letters, select = c(Sterilized,Pythium,.group))


