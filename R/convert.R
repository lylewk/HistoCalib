#' @title Write "x.new" from input OPD and osteon diameter
#' @description This is an interactive script that will have you enter
#' the OPD and average complete secondary osteon diameter in mm.
#' @details After entering your values for these two variables, the script
#' will apply the "gpower" transformation (Kelansky et al. 2013: eqn. 5) to
#' OPD and leave the diameter variable unchanged. NOTE: This script must be run
#' before any of the other scripts in this package.
#' @return returns a vector "x.new".  This will display on the screen but,
#' more importantly, will also write to your workspace as input for all further
#' scripts.  The vector has 1.0 as its first element (for the intercept in
#' multiple regression).
#' @references Kelmansky, D. M., Martínez, E. J., & Leiva, V. (2013).
#' A new variance stabilizing transformation for gene expression data
#' analysis. \emph{Statistical Applications in Genetics and Molecular Biolog}y,
#' 12(6), 653-666. https://doi.org/10.1515/sagmb-2012-0030
#'@examples
#'#Not run
#'# convert()
#'#  Enter OPD:
#'# 1: 23
#'# Read 1 item
#'#
#'# Enter average complete secondary osteon diameter in mm.
#'# 1: .12
#'# Read 1 item
#'#
#'# g.OPD Diameter
#'# 3.292091 0.120000

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
