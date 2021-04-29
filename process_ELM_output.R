##############################
#
# Open ELM/CLM netcdf files, join, & put into lists of R arrays 
#
# AWalker
# April 2021
#
##############################

rm(list=ls())

library(parallel)
library(ncdf4)



### Initialise
#######################################

# paths
wd_mod_out <- '/Volumes/disk2/Research_Projects/FATES/runs/tests/FACEconly_exptest/raw_output'

# filename variables
caseidprefix    <- 'FACEconly_exptest'
sites           <- 'US-DUK'
cases           <- c('I1850CLM45ED_ad_spinup', 'I1850CLM45ED', 'I20TRCLM45ED' )
mod             <- 'clm2'
fstart_datetime <- '-01-01-00000'

# time variables 
# - syear, years, & tsteps vectors are the same extent as cases and elements correspond
# APW: syear & years may vary by site
syear     <- c(1, 1, 1850 )
years     <- c(60, 60, 145 )
tsteps    <- c(1, 1, 365 ) 
hist      <- 0
days_in_m <- c(31,28,31,30,31,30,31,31,30,31,30,31)
time_vars <- c('mcdate', 'mcsec', 'mdcur', 'mscur', 'nstep', 
               'time_bounds', 'date_written', 'time_written' )

# switches
sep_spin  <- T
zip       <- F
# vsub      <- 209:210
vsub      <- NULL 
yrzero    <- F
varconv   <- T



### Functions
#######################################

conv_sh_to_rh <- function(q, t, p=101325 ) {
  # convert specific humidity to relative
  # use Buck 1981 for sat vp (as does SDGVM)
  
  # q - water:dry air mass ratio, g/g
  # t - temperature,              K
  # p - pressure,                 Pa
  
  paw  <- p / (0.62198/q + 1)
  paws <- 6.1121 * exp(17.502*(t-273.15) / (240.97 + t - 273.15)) * 100
  #print(c(paw,paws))
  paw/paws * 1000      # units for SDGVM 0.1 % or parts per mil
}


# conversion functions 
conv_K_to_oC   <- function(t,...) (t - 273.15)                              # converts K to oC
conv_persecond <- function(v,tsteps_in_yr,...) v * 3600*24*365/tsteps_in_yr # per second units to the timestep of the data     
conv_perday    <- function(v,tsteps_in_yr,...) v * 365/tsteps_in_yr         # per second units to the timestep of the data     
conv_kgperyear <- function(v,tsteps_in_yr,...) v * 1e-3/tsteps_in_yr        # kg per year units to g per timestep of the data     
conv_sw        <- function(swr,...) swr / (d_in_m*24*3600)  # W/m2

# all variables per timestep - could separate
# water units in kg (mm) m-2
# C units in g m-2
var_conv <- list(
  K            = conv_K_to_oC,
  `gC/m^2/s`   = conv_persecond,
  `gC/m2/s`    = conv_persecond,
  `mm/s`       = conv_persecond,
  `kgC/m2/yr`  = conv_kgperyear,
  `m2 m-2 d-1` = conv_perday
)


breakout_cases <- function(v, spinss ) {
  list(
    spins=v[spinss],
    trans=v[-spinss]
  )
} 



### Start processing
#######################################

# separate spins etc 
if(sep_spin) {
  spinss <- which(grepl('1850', cases ))
  cases  <- breakout_cases(cases, spinss ) 
  syear  <- breakout_cases(syear, spinss ) 
  years  <- breakout_cases(years, spinss ) 
  tsteps <- breakout_cases(tsteps, spinss ) 
}


# cases loop
c <- 1
for(c in 1:length(cases) ) {
  setwd(wd_mod_out)
  
  cases_current   <- cases[[c]] 
  syear_current   <- syear[[c]] 
  years_current   <- years[[c]] 
  tsteps_current  <- tsteps[[c]] 
  ntsteps_current <- sum(years_current * tsteps_current)
  
  # combine sim variables to get all simulations
  sims <- apply(as.matrix(expand.grid(caseidprefix,sites,cases_current)), 1, paste, collapse='_' )
  
  s <- 1
  for(s in 1:length(sims)) {
    setwd(paste0(sims[s],'/run'))
    
    if(sims[s]==sims[1]) {
      fdate      <- paste0(formatC(syear_current[1], width=4, format="d", flag="0"), fstart_datetime )
      ifile      <- paste(sims[s],mod,paste0('h',hist),fdate,'nc',sep='.')
      ncdf1      <- nc_open(ifile)
      vdims_list <- lapply(ncdf1$var, function(l) sapply(l$dim, function(l) l$name ) )
      vdims_len  <- lapply(ncdf1$var, function(l) sapply(l$dim, function(l) l$len ) )
      vars_units <- lapply(ncdf1$var, function(l) l$units )
      
      # redefine existing dimensions 
      newvars <-
        lapply(ncdf1$var, 
               function(var) {
                 if(any(vdims_list[[var[['name']]]]=='time'))
                   var$dim[[which(vdims_list[[var[['name']]]]=='time')]]$len <- ntsteps_current
                 var
               })
      nc_close(ncdf1)
      
      # subset variables
      if(!is.null(vsub)) newvars <- newvars[vsub]
      vnames <- names(newvars)
      
      # create new nc file
      setwd(wd_mod_out)
      new_fname <- paste(caseidprefix,sites,names(cases)[c],sep='_')
      newnc     <- nc_create(paste0(new_fname,'.nc'), newvars )
      tend_prev <- tcaug <- 0
      setwd(paste0(sims[s],'/run'))
      
      # list of variables & key info in new file 
      vars_list <- lapply(newnc$var, 
                          function(l) list(
                            name     = l$name,
                            longname = l$longname,
                            dims     = sapply(l$dim, function(l) l$name ),
                            len      = sapply(l$dim, function(l) l$len ),
                            units    = l$units
                          ))
    }
    
    
    # join files along redefined dimensions
    if(names(years)[c]=='spins') {
      year_range <- syear_current[s]:years_current[s] + 1
      timecount_augment <- tcaug
    } else {
      year_range <- syear_current[s]:(syear_current[s] + years_current[s] - 1)
      timecount_augment <- 0
    }
    
    # can multicore this loop but requires OOP or other method
    for(y in year_range) {
      fdate <- paste0(formatC(y, width=4, format="d", flag="0"), fstart_datetime )
      ifile <- paste(sims[s], mod, paste0('h',hist), fdate, 'nc', sep='.' )
      ncdf2 <- nc_open(ifile)

      # add dim value
      # there are also a bunch of time related variables
      if(names(years)[c]=='spins') {
        tstart <- y-1
      } else {
        tstart <- (y-year_range[1]) * tsteps_current[s] + 1
      }
      tstart   <- tstart + tend_prev
      timevals <- ncvar_get(ncdf2, 'time' )
      ncvar_put(newnc, 'time', timevals + timecount_augment, tstart, tsteps_current[s] )

      # add var values
      for(v in vnames) {
        if(any(vdims_list[[v]]=='time')) {
          start <- rep(1, length(vdims_list[[v]]) )
          start[which(vdims_list[[v]]=='time')] <- tstart
          count <- vdims_len[[v]]
          count[which(vdims_list[[v]]=='time')] <- tsteps_current[s]
          ncvar_put(newnc, v, ncvar_get(ncdf2, v ), start, count )
        }
      }
    }

    
    # sim loop
    tend_prev <- tend_prev + years_current[s]*tsteps_current[s]
    tcaug     <- timevals[length(timevals)]
    setwd(wd_mod_out)
  }
  
  
  # take data in newnc and create labeled R arrays
  dim_combos <- sapply(unique(vdims_list), function(v) paste(v, collapse=',' ) )
  dc_ss      <- sapply(vdims_list, function(v) which(dim_combos==paste(v, collapse=',' ))  )
  dc_nvars   <- data.frame(dim_combos, nvars=tabulate(dc_ss) )
  dlen       <- sapply(newnc$dim, function(l) l$len )
  
  # for each unique combination of dimensions create an array 
  al <- list()
  for(dc in 1:length(dim_combos)) {
    # find variables with dim_combos[dc] dimensions 
    avar_names <- names(dc_ss)[which(dc_ss==dc)]
    
    # create and label array
    ndims                    <- length(unique(vdims_list)[[dc]])
    dimnames                 <- c(rep(list(NULL),ndims), list(vars=avar_names))
    names(dimnames)[1:ndims] <- unique(vdims_list)[[dc]]
    # APW: need to add dimnames where appropriate, e.g. for ground levels, time? (could add a POSIX standard)
    adim <- dlen[names(dimnames)[1:ndims]]
    a    <- array(dim=c(adim,vars=length(avar_names)), dimnames=dimnames ) 
    amss <- as.matrix(expand.grid(lapply(adim, function(i) 1:i)))
    
    # populate array with variables & convert units
    for(v in avar_names) {
      vss                <- which(avar_names==v)
      # a[cbind(amss,vss)] <- ncvar_get(newnc, v )
      a[cbind(amss,vss)] <-
        if(vars_units[[v]]%in%names(var_conv) & varconv) {
          var_conv[[vars_units[[v]]]](ncvar_get(newnc, v ), tsteps_current[1] ) # APW: assumes that concatenated simulations have the same timestep
        } else {
          ncvar_get(newnc, v )
        }
    }
    
    # combine arrays into a list
    al <- c(al, list(a) )
    names(al)[length(al)] <- dim_combos[dc]
  }

  # close/write nc file
  nc_close(newnc)
  # zip nc file
  if(zip) system(paste('gzip ', paste0(new_fname,'.nc') ))

    
  # APW: could create annual data from daily here maybe
  # APW: could maybe do the variable conversion here too 
  
  
  # cases loop  
  # create output list(s) & save RDS file(s)
  l1 <- list(dimensions=dlen, dim_combinations=dc_nvars, variables=vars_list, data_arrays=al )
  saveRDS(l1, paste0(new_fname,'.RDS') )
}

names(var_conv)

# rough working

# # list of variables with key info 
# vars_list <- lapply(ncdf1$var, 
#                     function(l) list(
#                       name     = l$name,
#                       longname = l$longname,
#                       dims     = sapply(l$dim, function(l) l$name ),
#                       len      = sapply(l$dim, function(l) l$len ),
#                       units    = l$units
#                     ))
# 
# unique(sapply(vdims_list,function(l) l$dim ))
# 
# # process dimensions
# timevals <- ncvar_get(ncdf1, 'time' )
# ncvar_get(ncdf1, 'lndgrid' )
# ncvar_get(ncdf1, 'fates_levagepft' )
# ncvar_get(ncdf1, 'fates_levelem' )
# ncvar_get(ncdf1, 'fates_levheight' )




### END ###