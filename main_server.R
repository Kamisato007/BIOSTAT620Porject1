



HO <- readRDS("Summary_Statistics_HO.rds")
CW <- readRDS("Summary_Statistics_CW.rds")
YF <- readRDS("Summary_Statistics_YF.rds")



n1 <- HO$n
n2 <- YF$n_yf
n3 <- CW$n_cw

n <- n1+n2+n3
p <- dim(HO$SSX_ho)[1]

SSX_cw <- CW$SSX_cw
SSY_cw <- CW$SSY_cw
SSXY_cw <- CW$SSXY_cw

SSX_ho <- HO$SSX_ho
SSXY_ho <- HO$SSXY_ho
SSY_ho <- HO$SSY_ho

SSX_yf <- YF$SSX_yf
SSXY_yf <- YF$SSXY_yf
SSY_yf <- YF$SSY_yf

sum_SSX <- SSX_cw+SSX_ho+SSX_yf
sum_SSXY <- SSXY_cw+SSXY_ho+SSXY_yf
sum_SSY <- SSY_cw+SSY_ho+SSY_yf
BETA <- solve(sum_SSX)%*%(sum_SSXY)
SIGMA <- sqrt((((sum_SSY)-2*t(BETA)%*%(sum_SSXY)+t(BETA)%*%(sum_SSX)%*%BETA)/(n-p)))



SE_BETA <- c(SIGMA)*sqrt(diag(solve(sum_SSX)))
BETA/SE_BETA
qt(0.975,n-p)




RSS <- sum_SSY-2*t(BETA)%*%(sum_SSXY)+t(BETA)%*%(sum_SSX)%*%BETA

ybar = (n1*HO$y_ho_bar+n2*YF$y_yf_bar+n3*CW$y_cw_bar)/n

AIC <- n*log(RSS)+2*p
TSS <- sum_SSY-n*ybar^2
R2_a <- 1- (RSS/(n-p))/(TSS/(n-1))

result <- list(all_value= data.frame(beta=BETA,
                                     SE = SE_BETA,
                                     t = BETA/SE_BETA,
                                     p =  2*pt(abs(BETA/SE_BETA),n-p,lower.tail = F)),
               AIC = AIC,
               R2_a = R2_a)


result



