#' Estimate age and prediction interval using multiple linear regression
#' See Brown (1993:Sec. 5.2)
#'
#' @param area The desired area for prediction interval,
#' default = 0.95 for 95% prediction interval
#'
#' @references
#' Brown, P. J. (1993). Measurement, Regression, and Calibration.
#' New York: Oxford University Press.
#'
est_MLR<-function(area=0.95)
{
est=as.numeric(x.new%*%parms$Bx.y)
hf=as.numeric(x.new%*%parms$inv.XX%*%x.new)
se.pred=sqrt(parms$MSE*(1+hf))
t.value=-qt((1-area)/2,170)
lower=est-t.value*se.pred
upper=est+t.value*se.pred
sto=c(lower,est,upper)
names(sto)=c('lwr','fit','upr')
return(sto)
}
