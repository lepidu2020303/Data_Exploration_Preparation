library(tidyr)
library(ggplot2)
library(grid)
library(zoo)

# Load the package "FSelector"
install.packages("FSelector")
install.packages("rJava")

library(FSelector)


# Loading Data
# Set your Path Here
path<-"file:///C:/Users/admin/Documents/GitHub/Data_Exploration_Preparation/owid-covid-data.csv" 

install.packages("mlbench")

library(mlbench)
Dataset<-read.csv(path, sep = ",", dec = ".",row.names = 1)
Dataset=Dataset[-4] 
str(Dataset)
head(Dataset)