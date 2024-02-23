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

result <- data.frame(beta=BETA,
                     SE = SE_BETA,
                     t = BETA/SE_BETA)

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



