#install.packages("tidyverse")

# Load the required packages 
library("tidyverse")


#GGplot uses a different mode of saving pdf,
#so we are setting this as the pdf in Rjust incase the script want the keyword "pdf"
#pdf <- ggsave

#Opening a pdf file to save our graph
pdf("fff.pdf")

# load the data
df <- read.csv("C:/Users/ta19acu/Desktop/Life Expectancy Data.csv")

#Setting required columns to a variable
Life_Exp_Column <- df$Life.expectancy
GDP_Column <- df$Life.expectancy

# Suprressing warn that might occur due to scaling which is actually negligible
#options(warn=-1)

#Calculating the mean and std for normal distribution
mean = mean(Life_Exp_Column, na.rm = T)
sd = sd(Life_Exp_Column, na.rm = T)

#Extracting ten years data from the dataframe (2006 - 2015)
ten_years <- df$Year >= 2006L & df$Year <= 2015L

#This is our customized theme
my_theme <- theme_bw()+  theme(plot.title = element_text (hjust = 0.5, size = 14, face="bold"),
                               axis.text.x = element_text(color = "black", size = 12),
                               axis.text.y = element_text(color = "black", size = 12),
                               axis.title.y = element_text(color = "black", size = 13),
                               axis.title.x = element_text(color = "black", size = 13))


#This function breaks the log-axis evenly using the base-R
log_break <- function(n = 10){
  function(x) {
    axisTicks(log10(range(x, na.rm = TRUE)), log = TRUE, n = n)
  }
}

# This function calculates the P-VALUE for the graph usage
corr_eqn_p_value <- function(digits = 9100) {
  p_value <- cor.test(df$Life.expectancy, df$GDP,use = "complete.obs", method = "spearman", exact = FALSE)$p.value
  p_coef <- round(p_value, digits = digits)
  paste("p < ",p_coef)
}
labels_p = data.frame(x = 40, y = 40, label_p = corr_eqn_p_value ())

# This function calculates the CORRELATION COEFFICIENT for the graph usage
corr_eqn_r <- function() {
  r <- cor(df$Life.expectancy, df$GDP,use = "complete.obs", method = "spearman")
  corr_coef <- round(r,2)
  paste("r == ", corr_coef)
}
labels_r = data.frame(x = 40, y = 42, label_r = corr_eqn_r())

#This section draws the correlation graph
scatter_plot <- df %>%
                filter(ten_years) %>%
                ggplot() +
                aes(x = GDP, y = Life.expectancy, size = Population) +
                geom_point(size = 1L, colour = "black") +
                geom_smooth(method = "lm", span = 4L, color = "blue") +
                scale_x_continuous(trans= "log", breaks = log_break(), labels = prettyNum) +
                scale_y_continuous(trans = "log") +
                labs(y = "LIFE EXPECTANCY AT BIRTH (YEARS)", x = "GDP per CAPITA (USD - $)")+
                ggtitle("LIFE EXPECTANCY vs GDP per CAPITA (2006 - 2015) [LOG-SCALED]")+
                my_theme +
                geom_text(data = labels_p, size = 5, aes(x = x, y = y, label = labels_p[1,3]), parse = TRUE)+
                geom_text(data = labels_r, size = 5, aes(x = x, y = y, label = labels_r[1,3]), parse = TRUE)
              

#This section draws the histogram
hist_plot <- df %>%
            filter(ten_years) %>%
            ggplot() +
            aes(x = Life.expectancy,)+ 
            geom_histogram(aes(y=..density..),breaks=seq(35,90,5),bins = 20L, na.rm = TRUE,fill = "lightblue", color = "darkblue") +
            my_theme + scale_y_continuous(breaks = seq(0,0.05,0.01))+
            labs(x = "LIFE EXPECTANCY AT BIRTH (YEARS)", y = "FREQUENCY DENSITY") + ggtitle("HISTOGRAM OF LIFE EXPECTANCY AT BIRTH (2006 - 2015)")+
            geom_vline(aes(color = "Mean", xintercept = mean(Life.expectancy, na.rm = TRUE)),linetype = "dashed", size = 1L)+
            stat_function(aes(color = "Normal Dist"), fun=dnorm , args = list(mean,sd), size = 1) +
            scale_colour_manual("LEGEND", values = c("black", "red"))


print(scatter_plot)
print(hist_plot)

dev.off()








