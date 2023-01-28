
# Install all required packages, if needed.
if (!require("tidyverse", "ggpubr")) install.packages("tidyverse","ggpubr")

# Load the required packages 
library("ggpubr","dpylr", "ggplot2" )

df <- read.csv("C:/Users/ta19acu/Desktop/7com1079-group175/Life Expectancy Data.csv")

mean = mean(df$Life.expectancy, na.rm = T)
sd = sd(df$Life.expectancy, na.rm = T)


jpeg(file = "C:/Users/ta19acu/Desktop")

