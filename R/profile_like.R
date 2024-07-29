#' @title Find profile likelihood estimate and prediction interval for age
#' @description This script finds the profile likelihood estimate of age
#' and the prediction interval with the desired width.
#' @details This script takes as input the gpower transformed OPD and average
#' complete secondary osteon diameter in mm. from the mid-shaft 6th rib.
#' This data comes from the vector x.new.  This vector was written by the
#' interactive script convert().  The profile likelihood method is given in
#' Brown (1993: Sec. 5.8) and in this case with m=1 (no replication).
#' @param perc Desired prediction interval.  The default value is perc=95,
#' meaning the 95 percent prediction interval. There is the constraint that
#' 0<perc<100
#' @return returns lower bound of the prediction interval, the estimated
#' value, and the upper bound of the prediction intervals.
#' @references Brown, P. J. (1993). \emph{Measurement, Regression, and Calibration}.
#' New York: Oxford University Press.
#' 82(2), 199-220.

profile_like<-function (perc=95)
{
area=perc/100
c2n=function(epsilon){
     return(1+1/173+epsilon^2/(62940.3121387283))
}

z2=x.new[-1]

lnLK=function(epsilon){
  vec=z2-parms$y.mu-parms$by.x*epsilon
  quad=t(vec)%*%parms$S.inv%*%vec
  at.epsi=c2n(epsilon)
  return((174)/2*log(at.epsi/(at.epsi+quad)))
}

mle=optimize(lnLK,interval=c(-500,1000),maximum=T)$max

chi.sq=qchisq(area,1)/2

find.pred=function(try) lnLK(try)-lnLK(mle)+chi.sq
left=uniroot(find.pred,interval=c(mle-500,mle))$root
right=uniroot(find.pred,interval=c(mle,mle+500))$root
sto=parms$x.mu+c(left,mle,right)
names(sto)=c('lwr','fit','upr')
return(sto)
}
