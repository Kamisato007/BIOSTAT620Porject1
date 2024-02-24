



ST00_ps_cw = read.csv("screen time scw.csv")
ST00_ps_cw <- ST00_ps_cw[,-1]
colnames(ST00_ps_cw) <- c("Date", "Total.ST","Total.ST.min",
                          "Social.ST", "Social.ST.min","Pickups","Pickup.1s",
                          "if_weekend","Prop.ST","Duration")

ST00_ps_cw$Prop.ST <- ST00_ps_cw$Social.ST.min/ST00_ps_cw$Total.ST.min
ST00_ps_cw$Duration <- ST00_ps_cw$Total.ST.min/ST00_ps_cw$Pickups
ST00_ps_cw$Date <- as.POSIXct(ST00_ps_cw$Date, format = "%Y-%m-%d", tz = "UTC")
ST00_ps_cw$weekday = weekdays(ST00_ps_cw$Date , abbreviate = T)
ST00_ps_cw = ST00_ps_cw %>% mutate (if_weekend = weekday %in% c("Sun", "Sat"))



X_cw <- model.matrix(Social.ST.min~if_weekend+Pickups+Total.ST.min,data = ST00_ps_cw)
y_cw <- ST00_ps_cw$Social.ST.min 
y_cw_bar <- sum(y_cw)/dim(X_cw)[1]
SSX_cw <- t(X_cw)%*%X_cw
SSY_cw <- t(y_cw)%*%y_cw
SSXY_cw <-  t(X_cw)%*%y_cw



summary_list_cw <- list(y_cw_bar = y_cw_bar,
                        n_cw = dim(X_cw)[1],
                        SSX_cw = SSX_cw,
                        SSY_cw= SSY_cw,
                        SSXY_cw  = SSXY_cw)


saveRDS(summary_list_cw, "Summary_Statistics_CW.rds")



