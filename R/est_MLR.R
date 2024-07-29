#' @title Find multiple regression estimate and prediction interval for age
#' @description This script finds multiple regression estimate of age
#' and the prediction interval with the desired width.
#' @details This script takes as input the gpower transformed OPD and average
#' complete secondary osteon diameter in mm. from the mid-shaft 6th rib.
#' This data comes from the vector x.new.  This vector was written by the
#' interactive script convert().  The profile likelihood method is given in
#' Brown (1993: Sec. 5.2).
#' @param perc Desired prediction interval.  The default value is perc=95
#' meaning the 95 percent prediction interval. There is the constraint that
#' 0<perc<100
#' @return returns lower bound of the prediction interval, the estimated
#' value, and the upper bound of the prediction intervals.
#' @references Brown, P. J. (1993). \emph{Measurement, Regression, and Calibration}.
#' New York: Oxford University Press.
#' 82(2), 199-220.

est_MLR<-function(perc=95)
{
area=perc/100
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
