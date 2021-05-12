##############################
#
# Open ELM/CLM lists of R arrays and plot 
#
# AWalker
# April 2021
#
##############################

# rm(list=ls())

library(lattice)
library(grid)
library(viridis)


# variables not passsed from processing script
wd_out_plots  <- NULL
case_xlabs    <- c(spins='Spin-up', trans='Transient', aCO2='Ambient CO2', eCO2='Elevated CO2' )
case_timestep <- c(spins='year', trans='day', aCO2='day', eCO2='day' )


print('',quote=F);print('',quote=F);print('',quote=F)

if(exists('call_plot')) {
  
  print('Plotting function called from processing script.',quote=F)

} else {

  print('Plotting function called as stand alone.',quote=F)
  
  # paths
  wd_out       <- '/Volumes/disk2/Research_Projects/FATES/runs/tests/FACEconly_exptest/raw_output/FACEconly_exptest_processed'
  wd_out       <- '/Volumes/disk2/Research_Projects/FATES/runs/tests/FACEconly_exptest/raw_output/FACEconly_exptest_vcmax70_processed'
  # directory where to save output, if NULL same as wd_out
  
  # filename variables
  caseidprefix <- 'FACEconly_exptest'
  caseidprefix <- 'FACEconly_exptest_vcmax70'
  sites        <- 'US-DUK'
  case_labs    <- c('spins', 'trans' )
  
  
  
  ##################################
  # parse command line arguments
  
  print('',quote=F); print('',quote=F)
  print('Read command line arguments:',quote=F)
  print('',quote=F)
  print(commandArgs(T))
  if(length(commandArgs(T))>=1) {
    for( ca in 1:length(commandArgs(T)) ) {
      eval(parse(text=commandArgs(T)[ca]))
    }
  }
}



#######################################
# Initialise


# Functions
source('functions_plotting.R')


# plotting lists
# - each element in these plotlist lists result describe a separate figure when passed to the make_figures function 
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
    ylab  = expression('C Flux [gC '*m^-2*' timestep'^-1*']')
  ),
  p1 = list(
    vvars = c('NPP_CROOT','NPP_FROOT','NPP_LEAF','NPP_SEED','NPP_STEM','NPP_STOR'),
    ylab  = expression('C Flux [gC '*m^-2*' timestep'^-1*']')
  ),
  p2 = list(
    vvars = c('ED_balive','ED_bdead','ED_bfineroot','ED_biomass','ED_bleaf','ED_bsapwood','ED_bstore'),
    ylab  = expression('C Pool [gC '*m^-2*']')
  ),
  p3 = list(
    vvars = c('RAIN','QRUNOFF','QDRAI','QVEGT','QVEGE','QSOIL'),
    ylab  = expression('H2O Flux [kg '*H[2]*O*' '*m^-2*' timestep'^-1*']')
  ),
  p4 = list(
    vvars = c('SOILWATER_10CM'),
    ylab  = 'Soil Water Content [%]'
  ),
  p5 = list(
    vvars = c('BTRAN'),
    ylab  = 'Plant water status [0-1]'
  ),
  p6 = list(
    vvars = c('TLAI'),
    ylab  = expression('LAI ['*m^2*m^-2*']')
  ),
  p7 = list(
    vvars = c('DEMOTION_CARBONFLUX','MORTALITY_CARBONFLUX_CANOPY','MORTALITY_CARBONFLUX_UNDERSTORY'),
    ylab  = expression('C Flux [gC '*m^-2*' timestep'^-1*']')
  ),
  p8 = list(
    vvars = c('DISTURBANCE_RATE_FIRE','DISTURBANCE_RATE_LOGGING','DISTURBANCE_RATE_P2P','DISTURBANCE_RATE_P2S',
              'DISTURBANCE_RATE_POTENTIAL','DISTURBANCE_RATE_S2S','DISTURBANCE_RATE_TREEFALL'),
    ylab  = 'Fraction of ground area disturbed [0-1]'  
  ),
  p9 = list(
    vvars = c('ED_NCOHORTS','ED_NPATCHES'),
    ylab  = 'FATES diagnostics [#]'
  )
)

plotlist_phys <- list(
  p0 = list(
    vvars = c('GPP','AR'),
    ylab  = expression('C Flux [gC '*m^-2*' timestep'^-1*']')
  ),
  p0a = list(
    vvars = c('QVEGT','QVEGE'),
    ylab  = expression('H2O Flux [kg '*H[2]*O*' '*m^-2*' timestep'^-1*']')
  ),
  p2 = list(
    vvars = c('TLAI','TRIMMING'),
    sum_vars='TLAI',
    div_sumvars='TRIMMING',
    ylab  = expression('LAI ['*m^2*m^-2*'], LAI trimming [0-1]')
  ),
  p1c = list(
    vvars = NULL,
    ylab  = expression('veg WUE [g.'*kg^-1*']'),
    sum_vars = c('GPP'),
    div_sumvars = c('QVEGT')
  ),
  p1d = list(
    vvars = NULL,
    ylab  = expression('ecosystem WUE [g.'*kg^-1*']'),
    sum_vars = c('GPP'),
    div_sumvars = c('QVEGT','QVEGE','QSOIL')
  ),
  p1.1 = list(
    vvars = NULL,
    ylab  = expression('iWUE GPP/'*g[c]*' ['*mu*mol*' C '*mol^-1*' '*H[2]*O*']'),
    sum_vars = list(
      sum_vars=c('GPP'),
      scale=1e12/(86400*12) # convert g C umol-1 H2O to umol C mol-1 H2O
      ),
    div_prodvars = c('C_STOMATA','TLAI')
  ),
  p1a = list(
    vvars = c('C_STOMATA'),
    sum_vars='GPP',
    product_vars=c('C_STOMATA','TLAI'),
    ylab  = expression(g['s,c']*' ['*mu*mol*' '*H[2]*O*' '*m^-2*' s'^-1*']')
  ),
  p1b = list(
    vvars = c('C_LBLAYER'),
    ylab  = expression(g['s,c']*' ['*mu*mol*' '*H[2]*O*' '*m^-2*' s'^-1*']')
  ),
  p1 = list(
    vvars = c('BTRAN'),
    ylab  = expression('Plant water status [0-1]')
  ),
  p3 = list(
    vvars = c('RAIN','QVEGT','QVEGE','QSOIL'),
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



#############################################
# open RDS lists of arrays

setwd(wd_out)
if(is.null(wd_out_plots)) {
  wd_out_plots <- wd_out
} else if(!file.exists(wd_out_plots)) dir.create(wd_out_plots)


c <- 2
for(c in 1:length(case_labs)) {
  fname <- paste(caseidprefix,sites,case_labs[c],sep='_')
  l1    <- readRDS(paste0(fname,'.RDS')) 
  
  # extract data
  algtime   <- l1$data_arrays$`lndgrid,time`
  alglgtime <- l1$data_arrays$`lndgrid,levgrnd,time`
  
  # plot required figures
  plots <- make_figures(algtime, plotlist=plotlist, xlab=case_xlabs[case_labs[c]], timestep=case_timestep[case_labs[c]] )
  plot_figures(plots, paste0(fname,'.pdf') )
  
  plots <- make_figures(algtime, alglgtime, plotlist=plotlist_phys, xlab=case_xlabs[case_labs[c]], timestep=case_timestep[case_labs[c]] )
  plot_figures(plots, paste0(fname,'_physiology','.pdf') )
  
  
  # extract & plot annual data
  fname_a <- paste0(fname,'_annual.RDS')
  if(file.exists(fname_a)) {
    l2 <- readRDS(fname_a) 
    algtime_annual   <- l2$data_arrays$`lndgrid,time`
    alglgtime_annual <- l2$data_arrays$`lndgrid,levgrnd,time`
    
    plots <- make_figures(algtime_annual, plotlist=plotlist, xlab=case_xlabs[case_labs[c]], timestep='year' )
    plot_figures(plots, paste0(fname,'_annual','.pdf') )
  }
}



### END ###
