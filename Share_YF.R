

library(readxl)

ST00_ps_yuan = read_excel(path= "ScreenTime_Yuan.xlsx",
                          col_types = c("date"  , "numeric", "numeric","date", "numeric" ))
colnames(ST00_ps_yuan) <- c("Date", "Total.ST.min", "Social.ST.min","Pickups.1s","Pickups",
                            "weekdays","if_weekend")

ST00_ps_yuan$Prop.ST <- ST00_ps_yuan$Social.ST.min/ST00_ps_yuan$Total.ST.min
ST00_ps_yuan$Duration <- ST00_ps_yuan$Total.ST.min/ST00_ps_yuan$Pickups
ST00_ps_yuan$weekday = weekdays(ST00_ps_yuan$Date , abbreviate = T)
ST00_ps_yuan = ST00_ps_yuan %>% mutate (if_weekend = weekday %in% c("Sun", "Sat"))


X_yf <- model.matrix(Social.ST.min ~if_weekend+Pickups+Total.ST.min,data = ST00_ps_yuan)
y_yf <- ST00_ps_yuan$Social.ST.min 
y_yf_bar <- sum(y_yf)/dim(X_yf)[1]
SSX_yf <- t(X_yf)%*%X_yf
SSY_yf <- t(y_yf)%*%y_yf
SSXY_yf <-  t(X_yf)%*%y_yf




summary_list_yf <- list(y_yf_bar = y_yf_bar,
                        n_yf = dim(X_yf)[1],
                        SSX_yf = SSX_yf,
                        SSY_yf= SSY_yf,
                        SSXY_yf  = SSXY_yf)


saveRDS(summary_list_yf, "Summary_Statistics_YF.rds")

