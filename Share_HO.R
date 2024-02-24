

library(readxl)

ST00_ps = read_excel(path= "Screen Time_Hengde.xlsx",
                     col_types = c("date" , "text" , "numeric", "text", "numeric", "numeric", "date"))
ST00_ps[10,]$Total.ST.min = 114
ST00_ps[10,]$Social.ST.min = 67
ST00_ps$Prop.ST <- ST00_ps$Social.ST.min/ST00_ps$Total.ST.min
ST00_ps$Duration <- ST00_ps$Total.ST.min/ST00_ps$Pickups
ST00_ps$weekday = weekdays(ST00_ps$Date , abbreviate = T)
ST00_ps = ST00_ps %>% mutate (if_weekend = weekday %in% c("Sun", "Sat"))


X_ho <- model.matrix(Social.ST.min~if_weekend+Pickups+Total.ST.min,data = ST00_ps)
y_ho <- ST00_ps$Social.ST.min
y_ho_bar <- sum(y_ho)/dim(X_ho)[1]
SSX_ho <- t(X_ho)%*%X_ho
SSY_ho <- t(y_ho)%*%y_ho
SSXY_ho <-  t(X_ho)%*%y_ho




summary_list_ho <- list(y_ho_bar = y_ho_bar,
                        n_ho = dim(X_ho)[1],
                           SSX_ho = SSX_ho,
                           SSY_ho= SSY_ho,
                           SSXY_ho  = SSXY_ho)


saveRDS(summary_list_ho, "Summary_Statistics_HO.rds")






