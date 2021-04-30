##############################
#
# Open ELM/CLM lists of R arrays and plot 
#
# AWalker
# April 2021
#
##############################

rm(list=ls())

library(lattice)
library(grid)
library(viridis)



### Initialise
#######################################

# paths
wd_out       <- '/Volumes/disk2/Research_Projects/FATES/runs/tests/FACEconly_exptest/raw_output/FACEconly_exptest_processed'
wd_out       <- '/Volumes/disk2/Research_Projects/FATES/runs/tests/FACEconly_exptest/raw_output/FACEconly_exptest_vcmax70_processed'
# directory where to save output
wd_out_plots <- NULL

# filename variables
caseidprefix <- 'FACEconly_exptest'
caseidprefix <- 'FACEconly_exptest_vcmax70'
sites        <- 'US-DUK'
cases        <- c('spins', 'trans' )
case_xlabs   <- c(spins='Spin-up', trans='Transient' )


plotlist <- list(
  p0.1 = list(
    vvars = c('TBOT'),
    ylab  = expression('Air Temperature ['^o*'C]')
  ),
  p0.2 = list(
    vvars = c('RAIN'),
    ylab  = expression('Rain [mm]')
  ),
  p0.3 = list(
    vvars = c('FSDS'),
    ylab  = expression('Incoming short-wave radiation ['*W*m^-2*']')
  ),
  p0.4 = list(
    vvars = c('QBOT'),
    ylab  = expression('Air specific huumidity ['*kg*kg^-1*']')
  ),
  p0 = list(
    vvars = c('GPP','NPP','AR'),
    ylab  = expression('C Flux [gC '*m^-2*' timestep'^-1*']'),
    sum_vars = NULL,
    subtract_vars = NULL
  ),
  p1 = list(
    vvars = c('NPP_CROOT','NPP_FROOT','NPP_LEAF','NPP_SEED','NPP_STEM','NPP_STOR'),
    ylab  = expression('C Flux [gC '*m^-2*' timestep'^-1*']'),
    sum_vars = NULL,
    subtract_vars = NULL
  ),
  p2 = list(
    vvars = c('ED_balive','ED_bdead','ED_bfineroot','ED_biomass','ED_bleaf','ED_bsapwood','ED_bstore'),
    ylab  = expression('C Pool [gC '*m^-2*']'),
    sum_vars = NULL,
    subtract_vars = NULL
  ),
  p3 = list(
    vvars = c('RAIN','QRUNOFF','QDRAI','QVEGT','QVEGE'),
    ylab  = expression('H2O Flux [kg '*H[2]*O*' '*m^-2*' timestep'^-1*']'),
    sum_vars = NULL,
    subtract_vars = NULL
  ),
  p4 = list(
    vvars = c('SOILWATER_10CM'),
    ylab  = 'Soil Water Content [%]',
    sum_vars = NULL,
    subtract_vars = NULL
  ),
  p5 = list(
    vvars = c('BTRAN'),
    ylab  = 'Plant water status [0-1]',
    sum_vars = NULL,
    subtract_vars = NULL
  ),
  p6 = list(
    vvars = c('TLAI','TRIMMING'),
    ylab  = expression('LAI ['*m^2*m^-2*'], LAI trimming [0-1]'),
    sum_vars = NULL,
    subtract_vars = NULL
  ),
  p7 = list(
    vvars = c('DEMOTION_CARBONFLUX','MORTALITY_CARBONFLUX_CANOPY','MORTALITY_CARBONFLUX_UNDERSTORY'),
    ylab  = expression('C Flux [gC '*m^-2*' timestep'^-1*']'),
    sum_vars = NULL,
    subtract_vars = NULL
  ),
  p8 = list(
    vvars = c('DISTURBANCE_RATE_FIRE','DISTURBANCE_RATE_LOGGING','DISTURBANCE_RATE_P2P','DISTURBANCE_RATE_P2S',
              'DISTURBANCE_RATE_POTENTIAL','DISTURBANCE_RATE_S2S','DISTURBANCE_RATE_TREEFALL'),
    ylab  = 'Fraction of ground area disturbed [0-1]',  
    sum_vars = NULL,
    subtract_vars = NULL
  ),
  p9 = list(
    vvars = c('ED_NCOHORTS','ED_NPATCHES'),
    ylab  = 'FATES diagnostics [#]',
    sum_vars = NULL,
    subtract_vars = NULL
  )
)

plotlist_phys <- list(
  p0 = list(
    vvars = c('GPP','AR'),
    ylab  = expression('C Flux [gC '*m^-2*' timestep'^-1*']'),
    sum_vars = NULL,
    subtract_vars = NULL
  ),
  p0a = list(
    vvars = c('QVEGT','QVEGE'),
    ylab  = expression('H2O Flux [kg '*H[2]*O*' '*m^-2*' timestep'^-1*']')
  ),
  p1 = list(
    vvars = c('BTRAN'),
    ylab  = expression('Plant water status [0-1]; WUE [kg.'*g^-1*']'),
    sum_vars = c('QVEGT'),
    subtract_vars = NULL,
    div_vars = c('GPP')
  ),
  p1a = list(
    vvars = c('C_STOMATA'),
    ylab  = expression(g[s]*' ['*mu*mol*' '*m^-2*' s'^-1*']')
  ),
  p1b = list(
    vvars = c('C_LBLAYER'),
    ylab  = expression(g[b]*' ['*mu*mol*' '*m^-2*' s'^-1*']')
  ),
  p2 = list(
    vvars = c('TLAI','TRIMMING'),
    ylab  = expression('LAI ['*m^2*m^-2*'], LAI trimming [0-1]'),
    sum_vars = NULL,
    subtract_vars = NULL
  ),
  p3 = list(
    vvars = c('RAIN','QVEGT','QVEGE'),
    ylab  = expression('H2O Flux [kg '*H[2]*O*' '*m^-2*' timestep'^-1*']'),
    sum_vars = c('QRUNOFF','QDRAI'),
    subtract_vars = NULL
  ),
  p4 = list(
    vvars   = c('H2OSOI'),
    v4d     = T,
    dim_sub = 1:10,
    ylab    = 'Soil Water Content [%]'
  ),
  p5 = list(
    vvars = c('SOILPSI'),
    v4d     = T,
    dim_sub = 1:10,
    ylab    = expression('Soil Water Potential, '*Psi*' [MPa]')
  ),
  p6 = list(
    vvars = c('SOILPSI'),
    v4d     = T,
    dim_sub = 2:10,
    ylab    = expression('Soil Water Potential, '*Psi*' [MPa]')
  )
)


# functions
############################3

plot_3dim <- function(a3d=algtime, vvars=c('GPP','NPP','AR'), xdim='time', 
                      vcol=NULL, leg_cols=4, print2screen=T, ... ) {
  lv <- length(vvars)
  lx <- dim(a3d)[xdim]
  if(is.null(vcol)) vcol <- viridis(lv)
  if(lv<leg_cols)   leg_cols <- lv
  
  p1 <- 
    xyplot(a3d[,,vvars] ~ rep(1:lx,lv), groups=rep(vvars,each=lx),
           type='l', 
           scales=list(tck=c(-0.5,-0.5)),
           par.settings=simpleTheme(col=vcol,lwd=2),# lwd=2, lty = 1:3 ), 
           auto.key=list(lines=T, points=F, corner=c(0,1), x=0, y=1, 
                         columns=leg_cols, border=T, cex=0.75, background='white' ),
           ... )
  
  if(print2screen) print(p1)
  p1
}


plot_3dim_combvars <- function(a3d=algtime, vvars=c('GPP'), sum_vars=c('NPP','AR'), 
                               product_vars=NULL, 
                               subtract_vars=NULL, 
                               div_vars=NULL,
                               xdim='time', vcol=NULL, leg_cols=4, print2screen=T, ... ) {
  lv <- length(vvars) + !is.null(sum_vars) 
  lx <- dim(a3d)[xdim]
  
  # averaging / summing
  asum    <- apply(a3d[,,sum_vars,drop=F], 2:1, sum )
  nv_name <- paste0('sum(',paste(sum_vars,collapse=','),')')
  if(!is.null(subtract_vars)) {
    asub <- apply(a3d[,,subtract_vars,drop=F], 2:1, sum )
    asum <- asum[drop=F] - asub[drop=F]
    nv_name <- paste0(nv_name,' - sum(',paste(subtract_vars,collapse=','),')')
  } else if(!is.null(div_vars)) {
    adiv <- apply(a3d[,,div_vars,drop=F], 2:1, sum )
    asum <- asum[drop=F] / adiv[drop=F]
    nv_name <- paste0(nv_name,' / sum(',paste(div_vars,collapse=','),')')
  }
  
  
  groups <- c(vvars, nv_name  )
  if(is.null(vcol))           vcol <- viridis(length(groups))
  if(length(groups)<leg_cols) leg_cols <- length(groups)
  
  a3d[,,'TRIMMING'] <- asum[,]
  dimnames(a3d)[[3]][which(dimnames(a3d)[[3]]=='TRIMMING')] <- nv_name
  
  p1 <- 
    xyplot(a3d[,,groups] ~ rep(1:lx,lv), groups=rep(groups,each=lx),
           type='l', scales=list(tck=c(-0.5,-0.5)),
           par.settings=simpleTheme(col=vcol,lwd=2),# lty = 1:3 ), 
           auto.key=list(lines=T, points=F, corner=c(0,1), x=0, y=1,
                         columns=leg_cols, border=T, cex=0.75, background='white' ),
           ... )
  
  if(print2screen) print(p1)
  p1
}


plot_4dim <- function(a4d=alglgtime, vvars=c('H2OSOI'), xdim='time', zdim='levgrnd', dim_sub=NULL,
                      vcol=NULL, leg_cols=4, print2screen=T, ... ) {
  lv <- length(vvars)
  lx <- dim(a4d)[xdim]
  lz <- if(is.null(dim_sub)) dim(a4d)[zdim] else length(dim_sub)
  if(is.null(dim_sub)) dim_sub <- 1:lz
  if(is.null(vcol)) vcol <- viridis(lz)
  if(lz<leg_cols)   leg_cols <- lz
  
  p1 <- 
    xyplot(a4d[,dim_sub,,vvars] ~ rep(1:lx,each=lz), groups=rep(paste(vvars,letters[dim_sub]),lx),
           type='l', 
           scales=list(tck=c(-0.5,-0.5)),
           par.settings=simpleTheme(col=vcol,lwd=2),# lwd=2, lty = 1:3 ), 
           auto.key=list(lines=T, points=F, corner=c(0,1), x=0, y=1, 
                         columns=leg_cols, border=T, cex=0.75, background='white' ),
           ... )
  
  if(print2screen) print(p1)
  p1
}


make_figures <- function(a3d, a4d, plotlist, xlab='Spin-up', timestep='years', print2screen=F ) {
  
  lapply(plotlist, function(l) {
    print('')
    print('Making figure:')
    print(unlist(l))
    
    if(is.null(l$v4d)) {
      if(is.null(l$sum_vars)) {
        plot_3dim(a3d, vvars=l$vvars, ylab=l$ylab, xlab=paste(xlab,timestep), print2screen=print2screen )
      } else {
        plot_3dim_combvars(a3d, sum_vars=l$sum_vars, subtract_vars=l$subtract_vars, div_vars=l$div_vars,
                           vvars=l$vvars, ylab=l$ylab, xlab=paste(xlab,timestep), 
                           print2screen=print2screen )
      }
    } else {
      if(!is.null(a4d)) {
        plot_4dim(a4d, vvars=l$vvars, dim_sub=l$dim_sub, ylab=l$ylab, xlab=paste(xlab,timestep), print2screen=print2screen )
      } else {
        warning('a4d argument not given, no 4d plots.')
      }
    }
  })
}


plot_figures <- function(plots, plotname='plots.pdf', nper_page=3 ) {
  print('', quote=F )
  print(paste('Saving figures to:',plotname), quote=F )
  
  pdf(plotname, width=8.5, height=11 )
  lapply(1:length(plots), function(p) {
    print(plots[p], 
          split=c(1,p%%nper_page+if(p%%nper_page==0) nper_page else 0,1,nper_page), 
          more=p%%nper_page )
    numeric(0)
  })
  dev.off()
}



#############################################
# open RDS lists of arrays

setwd(wd_out)
if(is.null(wd_out_plots)) {
  wd_out_plots <- wd_out
} else if(!file.exists(wd_out_plots)) dir.create(wd_out_plots)


c <- 2
fname <- paste(caseidprefix,sites,cases[c],sep='_')
l1    <- readRDS(paste0(fname,'.RDS')) 
l2    <- readRDS(paste0(fname,'_annual.RDS')) 

# extract data
algtime   <- l1$data_arrays$`lndgrid,time`
alglgtime <- l1$data_arrays$`lndgrid,levgrnd,time`
format(object.size(algtime),units='Mb')
format(object.size(alglgtime),units='Mb')
dimnames(algtime)[[3]]
dimnames(alglgtime)


# extract annual data
algtime_annual   <- l2$data_arrays$`lndgrid,time`
alglgtime_annual <- l2$data_arrays$`lndgrid,levgrnd,time`
format(object.size(algtime_annual),units='Mb')
format(object.size(alglgtime_annual),units='Mb')
dimnames(algtime_annual)[[3]]
dimnames(alglgtime_annual)

names(l1)
names(l2)

lapply(l1$variables, function(l) l$units )[c('NPP_STEM','GPP')]
lapply(l2$variables, function(l) l$units )[c('NPP_STEM','GPP')]



# plot annual data
plots <- make_figures(algtime_annual, plotlist=plotlist, xlab=case_xlabs[cases[c]], timestep='years' )
plot_figures(plots, paste0(fname,'_annual','.pdf') )

plots <- make_figures(algtime, alglgtime, plotlist=plotlist_phys, xlab=case_xlabs[cases[c]], timestep='days' )
plot_figures(plots, paste0(fname,'_physiology','.pdf') )

# plots <- make_figures(algtime, alglgtime, plotlist=c(plotlist_phys,plotlist['p1']), xlab=case_xlabs[cases[c]], timestep='days' )
# plot_figures(plots, paste0(fname,'_physiology','.pdf') )


# spin
c <- 1
fname <- paste(caseidprefix,sites,cases[c],sep='_')
l1    <- readRDS(paste0(fname,'.RDS')) 

# extract data
algtime   <- l1$data_arrays$`lndgrid,time`
alglgtime <- l1$data_arrays$`lndgrid,levgrnd,time`
format(object.size(algtime),units='Mb')
format(object.size(alglgtime),units='Mb')
dimnames(alglgtime)
l1$variables['QBOT']
l1$variables['RH2M']
l1$variables['RH2M_R']

grep( 'humidity', lapply(l1$variables, function(l) l$longname) , value=T )
grep( 'kg/kg', lapply(l1$variables, function(l) l$units) , value=T )


# plot annual data
plots <- make_figures(algtime, plotlist=plotlist, xlab=case_xlabs[cases[c]], timestep='years' )
plot_figures(plots, paste0(fname,'.pdf') )

plots <- make_figures(algtime, alglgtime, plotlist=plotlist_phys, xlab=case_xlabs[cases[c]], timestep='years' )
plot_figures(plots, paste0(fname,'_physiology','.pdf') )


grep('resistance', sapply(l1$variables, function(l) l$longname ), value=T )
grep('conductance', sapply(l1$variables, function(l) l$longname ), value=T )
l1$variables['C_LBLAYER']
l1$variables['C_STOMATA']



### END ###