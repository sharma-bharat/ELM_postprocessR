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
wd_out_plots   <- NULL
case_xlabs     <- c(spins='Spin-up', trans='Transient', aCO2='Ambient CO2', eCO2='Elevated CO2' )
case_timestep  <- c(spins='year', trans='day', aCO2='day', eCO2='day' )
plot_nutrients <- F

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
source('info_plotting.R')



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

  if(grepl('CNP',caseidprefix) | plot_nutrients) {
    print('PLOTTING NUTRIENTS')
    plots <- make_figures(algtime, plotlist=plotlist_nutrients, xlab=case_xlabs[case_labs[c]], timestep=case_timestep[case_labs[c]] )
    plot_figures(plots, paste0(fname,'_nutrients','.pdf') )
  }  
  
  # extract & plot annual data
  fname_a <- paste0(fname,'_annual.RDS')
  if(file.exists(fname_a)) {
    l2 <- readRDS(fname_a) 
    algtime_annual   <- l2$data_arrays$`lndgrid,time`
    alglgtime_annual <- l2$data_arrays$`lndgrid,levgrnd,time`
    
    plots <- make_figures(algtime_annual, plotlist=plotlist, xlab=case_xlabs[case_labs[c]], timestep='year' )
    plot_figures(plots, paste0(fname,'_annual','.pdf') )
  
    if(grepl('CNP',caseidprefix) | plot_nutrients) {
      plots <- make_figures(algtime_annual, plotlist=plotlist_nutrients, xlab=case_xlabs[case_labs[c]], timestep='year' )
      plot_figures(plots, paste0(fname,'_annual_nutrients','.pdf') )
    }  
  }
}



### END ###
