#' Estimate age and prediction interval using profile likelihood method
#'
#' @param area The desired area for prediction interval,
#'             default is 0.95 for 95% interval
#'
#' @references
#' Brown, P. J. (1993). Measurement, Regression, and Calibration.
#' New York: Oxford University Press.
#'
profile_like<-function (area=0.95)
{

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
