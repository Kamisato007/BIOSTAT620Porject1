library(readxl)
library(tidyverse)
library(car)

ST00_ps = read_excel(path= "Screen Time_Hengde.xlsx",
                     col_types = c("date" , "text" , "numeric", "text", "numeric", "numeric", "date"))
ST00_ps[10,]$Total.ST.min = 114
ST00_ps[10,]$Social.ST.min = 67


ST00_ps_yuan = read_excel(path= "ScreenTime_Yuan.xlsx",
                          col_types = c("date"  , "numeric", "numeric","date", "numeric" ))


ST00_ps_cw = read.csv("screen time scw.csv")
ST00_ps_cw <- ST00_ps_cw[,-1]

colnames(ST00_ps_cw) <- c("Date", "Total.ST","Total.ST.min",
                          "Social.ST", "Social.ST.min","Pickups","Pickup.1s",
                          "if_weekend","Prop.ST","Duration")
colnames(ST00_ps_yuan) <- c("Date", "Total.ST.min", "Social.ST.min","Pickups.1s","Pickups",
                            "weekdays","if_weekend")


# Prop.ST & Duration & If Weekend
class(ST00_ps$Date)
class(ST00_ps_cw$Date)
class(ST00_ps_yuan$Date)
ST00_ps$Prop.ST <- ST00_ps$Social.ST.min/ST00_ps$Total.ST.min
ST00_ps$Duration <- ST00_ps$Total.ST.min/ST00_ps$Pickups
ST00_ps$weekday = weekdays(ST00_ps$Date , abbreviate = T)
ST00_ps = ST00_ps %>% mutate (if_weekend = weekday %in% c("Sun", "Sat"))

ST00_ps_cw$Prop.ST <- ST00_ps_cw$Social.ST.min/ST00_ps_cw$Total.ST.min
ST00_ps_cw$Duration <- ST00_ps_cw$Total.ST.min/ST00_ps_cw$Pickups
ST00_ps_cw$Date <- as.POSIXct(ST00_ps_cw$Date, format = "%Y-%m-%d", tz = "UTC")
ST00_ps_cw$weekday = weekdays(ST00_ps_cw$Date , abbreviate = T)
ST00_ps_cw = ST00_ps_cw %>% mutate (if_weekend = weekday %in% c("Sun", "Sat"))

ST00_ps_yuan$Prop.ST <- ST00_ps_yuan$Social.ST.min/ST00_ps_yuan$Total.ST.min
ST00_ps_yuan$Duration <- ST00_ps_yuan$Total.ST.min/ST00_ps_yuan$Pickups
ST00_ps_yuan$weekday = weekdays(ST00_ps_yuan$Date , abbreviate = T)
ST00_ps_yuan = ST00_ps_yuan %>% mutate (if_weekend = weekday %in% c("Sun", "Sat"))



# Check column names
colnames(ST00_ps_cw)
colnames(ST00_ps)
colnames(ST00_ps_yuan)


# Time Series Plots in Section 2 (Data Description)
library(ggplot2)
library(dplyr)

# Function to plot social screen time
plot_social_time <- function(data) {
  ggplot(data, aes(x = Date, y = Social.ST.min, color = if_weekend)) +
    geom_line() + # Draw lines connecting data points
    geom_point() + # Add data points
    scale_color_manual(values = c("TRUE" = "#156077", "FALSE" = "#f46f20")) + # Custom color for weekend/weekday
    labs(x = "", y = "Social Screen Time (min)", color = "Weekend") + # Labeling
    theme_minimal() + # Minimal theme for cleaner appearance
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Function to plot the proportion of social screen time to total screen time
plot_prop_st <- function(data) {
  ggplot(data, aes(x = Date, y = Prop.ST, color = if_weekend)) +
    geom_line() + # Draw lines connecting data points
    geom_point() + # Add data points
    scale_color_manual(values = c("TRUE" = "#156077", "FALSE" = "#f46f20")) + # Custom color for weekend/weekday
    labs(x = "", y = "Proportion of Social Time", color = "Weekend") + # Labeling
    theme_minimal() + # Minimal theme for cleaner appearance
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Function to plot the total number of pickups
plot_durations <- function(data) {
  ggplot(data, aes(x = Date, y = Duration, color = if_weekend)) +
    geom_line() + # Draw lines connecting data points
    geom_point() + # Add data points
    scale_color_manual(values = c("TRUE" = "#156077", "FALSE" = "#f46f20")) + # Custom color for weekend/weekday
    labs(x = "", y = "Average Duration for Each Pickup", color = "Weekend") + # Labeling
    theme_minimal() + # Minimal theme for cleaner appearance
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Subject 1
plot1 <- plot_social_time(ST00_ps)
plot2 <- plot_prop_st(ST00_ps)
plot3 <- plot_durations(ST00_ps)
# Subject 2
plot4 <- plot_social_time(ST00_ps_cw)
plot5 <- plot_prop_st(ST00_ps_cw)
plot6 <- plot_durations(ST00_ps_cw)
# Subject 3
plot7 <- plot_social_time(ST00_ps_yuan)
plot8 <- plot_prop_st(ST00_ps_yuan)
plot9 <- plot_durations(ST00_ps_yuan)


library(gridExtra)
library(patchwork)
# Combine
combined_plot <- (plot1 | plot2 | plot3) / 
                 (plot4 | plot5 | plot6) / 
                 (plot7 | plot8 | plot9) +
                 plot_layout(guides = 'collect') & 
                 theme(legend.position = 'bottom', 
                       legend.text = element_text(size = 15),
                       legend.title = element_text(size = 15))

print(combined_plot)

# Save
ggsave("9_panel_plot.png", combined_plot, width = 20, height = 15)


# Federal Learning


# Summary Statistics for Hengde Ouyang

X_ho <- model.matrix(Social.ST.min~if_weekend+Pickups+Total.ST.min,data = ST00_ps)
y_ho <- ST00_ps$Social.ST.min
SSX_ho <- t(X_ho)%*%X_ho
SSY_ho <- t(y_ho)%*%y_ho
SSXY_ho <-  t(X_ho)%*%y_ho



# Summary Statistics for Yuan Feng

X_yf <- model.matrix(Social.ST.min ~if_weekend+Pickups+Total.ST.min,data = ST00_ps_yuan)
y_yf <- ST00_ps_yuan$Social.ST.min 
SSX_yf <- t(X_yf)%*%X_yf
SSY_yf <- t(y_yf)%*%y_yf
SSXY_yf <-  t(X_yf)%*%y_yf


# Summary Statistics for Congwei Shi

X_cw <- model.matrix(Social.ST.min~if_weekend+Pickups+Total.ST.min,data = ST00_ps_cw)
y_cw <- ST00_ps_cw$Social.ST.min 
SSX_cw <- t(X_cw)%*%X_cw
SSY_cw <- t(y_cw)%*%y_cw
SSXY_cw <-  t(X_cw)%*%y_cw



# Calculate Beta and Sigma

n1 = dim(ST00_ps)[1]
n2 = dim(ST00_ps_yuan)[1]
n3 = dim(ST00_ps_cw)[1]


n = dim(ST00_ps)[1]+dim(ST00_ps_cw)[1]+dim(ST00_ps_yuan)[1]
p = dim(X_ho)[2]
sum_SSX <- SSX_cw+SSX_ho+SSX_yf
sum_SSXY <- SSXY_cw+SSXY_ho+SSXY_yf
sum_SSY <- SSY_cw+SSY_ho+SSY_yf
BETA <- solve(sum_SSX)%*%(sum_SSXY)
SIGMA <- sqrt((((sum_SSY)-2*t(BETA)%*%(sum_SSXY)+t(BETA)%*%(sum_SSX)%*%BETA)/(n-p)))


# T-test

SE_BETA <- c(SIGMA)*sqrt(diag(solve(sum_SSX)))
BETA/SE_BETA
qt(0.975,n-p)


# AIC/RSS

RSS <- sum_SSY-2*t(BETA)%*%(sum_SSXY)+t(BETA)%*%(sum_SSX)%*%BETA

ybar = (n1*mean(y_ho)+n2*mean(y_yf)+n3*mean(y_cw))/n
TSS <- sum(y_ho^2)+sum(y_yf^2)+sum(y_cw^2)-n*ybar^2
R2_a <- 1- (RSS/(n-p))/(TSS/(n-1))
AIC <- n*log(RSS)+2*p

result <- list(all_value= data.frame(beta=BETA,
                     SE = SE_BETA,
                     t = BETA/SE_BETA,
                     p =  2*pt(abs(BETA/SE_BETA),n-p,lower.tail = F)),
               AIC = AIC,
               R2_a = R2_a)
               

result






# Comfirmation Analysis

y_all <- c(y_ho,y_yf,y_cw)
X_all <- rbind(X_ho,X_yf,X_cw)
combined_data <- data.frame(weekend=X_all[,2],
                            Pickups=X_all[,3],
                            Total.ST.min = X_all[,4],
                            Social.ST.min = y_all)
all.fit <- lm(Social.ST.min~weekend+Pickups+Total.ST.min,data=combined_data)
summary(all.fit)




BETA
SE_BETA
R2_a
AIC







# Testing LINE

## Testing L

avPlots(all.fit)


## Testing I
res <- all.fit$residuals
plot(1:length(res),res,xlab = "index",ylab = "Residuals")
# Independent assumption seems to be violated


## Testing N


hist(res,main = "Histogram of residuals",xlab = "Residuals")
qqPlot(res,ylab = "Residuals")


## Testing E

residualPlots(all.fit,type="response")



