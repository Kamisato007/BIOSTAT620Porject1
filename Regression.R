library(readxl)
library(tidyverse)
library(car)

ST00_ps = read_excel(path= "Screen Time_Hengde.xlsx",
                     col_types = c("date" , "text" , "numeric", "text", "numeric", "numeric", "date"))



ST00_ps_yuan = read_excel(path= "ScreenTime_Yuan.xlsx",
                          col_types = c("date"  , "numeric", "numeric","date", "numeric" ))


ST00_ps_cw = read_excel(path= "Screen Time_Chongwei.xlsx",
                        col_types = c("date" , "text" , "numeric", "text", "numeric", "numeric", "date"
                                      ,"numeric"))




ST00_ps$weekday = weekdays(ST00_ps$Date , abbreviate = T)
ST00_ps = ST00_ps %>% mutate ( if_weekend = weekday %in% c("Sun", "Sat"))
ST00_ps_yuan$weekday = weekdays(ST00_ps_yuan$Date , abbreviate = T)
ST00_ps_yuan = ST00_ps_yuan %>% mutate ( if_weekend = weekday %in% c("Sun", "Sat"))




# Summary Statistics for Hengde Ouyang

X_ho <- model.matrix(Social.ST.min~if_weekend+Pickups,data = ST00_ps)
y_ho <- ST00_ps$Social.ST.min
SSX_ho <- t(X_ho)%*%X_ho
SSY_ho <- t(y_ho)%*%y_ho
SSXY_ho <-  t(X_ho)%*%y_ho



# Summary Statistics for Yuan Feng

X_yf <- model.matrix(Social.ST ~if_weekend+Pickups,data = ST00_ps_yuan)
y_yf <- ST00_ps_yuan$Social.ST 
SSX_yf <- t(X_yf)%*%X_yf
SSY_yf <- t(y_yf)%*%y_yf
SSXY_yf <-  t(X_yf)%*%y_yf


# Summary Statistics for Congwei Shi

colnames(ST00_ps_cw) <- colnames(ST00_ps)[c(1:7,9)]
X_cw <- model.matrix(Social.ST.min~if_weekend+Pickups,data = ST00_ps_cw)
y_cw <- ST00_ps_cw$Social.ST.min 
SSX_cw <- t(X_cw)%*%X_cw
SSY_cw <- t(y_cw)%*%y_cw
SSXY_cw <-  t(X_cw)%*%y_cw



# Calculate Beta and Sigma


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

RSS <- (n-p)*SIGMA

y_all <- c(y_ho,y_yf,y_cw)
TSS <- sum((y_all-mean(y_all))^2) 
R2_a <- 1- (SIGMA)/(TSS/(n-1))
AIC <- n*log(RSS)+2*p





# Comfirmation Analysis


X_all <- rbind(X_ho,X_yf,X_cw)
combined_data <- data.frame(weekend=X_all[,2],
                            Pickups=X_all[,3],
                            Social.ST = y_all)
all.fit <- lm(Social.ST~weekend+Pickups,data=combined_data)
summary(all.fit)

BETA
SE_BETA



res <- all.fit$residuals




# Testing LINE

## Testing L

avPlots(all.fit)


## Testing I

plot(1:length(res),res,xlab = "index",ylab = "Residuals")
# Independent assumption seems to be violated


## Testing N

res <- all.fit$residuals
hist(res,main = "Histogram of residuals",xlab = "Residuals")
qqPlot(res,ylab = "Residuals")


## Testing E

residualPlots(all.fit,type="response")

