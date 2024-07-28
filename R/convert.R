#' Write "x.new" from input OPD and osteon diameter
#'
#' Write x.new that contains the "gpower"
#' transformed OPD and the average complete
#' secondary osteon diameter in mm.  This is
#' interactive and consequenty takes its input
#' from your entries.
#'
#'@references
#'
#'  #For the "gpower" transformation
#'
#'  Kelmansky, D. M., Martínez, E. J., & Leiva, V. (2013).
#'  A new variance stabilizing transformation for gene expression
#'  data analysis. Statistical Applications in Genetics and
#'  Molecular Biology, 12(6), 653-666.
#'  https://doi.org/10.1515/sagmb-2012-0030

convert=function ()
{
  gpower=function(y,lambda=-0.08102)
  {
    g.OPD=((y+sqrt(y^2+1))^lambda-1)/lambda
    return(g.OPD)
  }

  cat('\nEnter OPD:\n')
  trait1=scan(file='',n=1)
  g.OPD=gpower(trait1)
  cat('\nEnter average complete secondary osteon diameter in mm.\n')
  DIAM=scan(file='',n=1)
  sto=c(1,g.OPD,DIAM)
  x.new<<-sto
  sto=sto[-1]
  names(sto)=c('g.OPD','Diameter')
  cat('\n')
  return(sto)
}
