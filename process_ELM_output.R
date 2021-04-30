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
# library(optparse) - could be used for python-like option parsing



### Initialise
#######################################

##################################
# command line options and defaults

# any one of the below variables to line 77 or so can be specified as a command line argument by following the call to this script with a character string
# each argument string (separated by a space) is parsed and interpreted individually as R code.

#       Rscript run_MAAT.R "object1<-value1" "object2<-value2"
#  e.g. Rscript run_MAAT.R "wd_mod_out<-'/home/alp/'" "zip<-T"
#  OR from a calling script can be a single argument variable 
#  e.g. ARGS="wd_mod_out<-'/home/alp/' zip<-T"
#       Rscript run_MAAT.R $ARGS
#       Rscript run_MAAT.R "$ARGS varconv<-F"


# paths
# directory where model run directories live
wd_mod_out <- '/Volumes/disk2/Research_Projects/FATES/runs/tests/FACEconly_exptest/raw_output'
# directory where to save output
wd_out     <- NULL

# filename etc variables
caseidprefix    <- 'FACEconly_exptest'
sites           <- 'US-DUK'
cases           <- c('I1850CLM45ED_ad_spinup', 'I1850CLM45ED', 'I20TRCLM45ED' )
# model name in output files
mod             <- 'clm2'
# start date and time of output files
# - currently only this one is supported, anything other then output files covering integer years requires development 
fstart_datetime <- '-01-01-00000'
# netcdf dimensions that contain character variables, can be a vector
char_dims       <- 'string_length'


# time variables 
# - syear, years, & tsteps vectors are the same extent as cases and elements correspond
# APW: syear & years may vary by site but currently not supported
# start year of each case
syear     <- c(1, 1, 1850 )
# number of years of each case
years     <- c(60, 60, 145 )
# output timesteps per year of each case
tsteps    <- c(1, 1, 365 ) 
# number for history file
hist      <- 0
days_in_m <- c(31,28,31,30,31,30,31,31,30,31,30,31)
time_vars <- c('mcdate', 'mcsec', 'mdcur', 'mscur', 'nstep', 
               'time_bounds', 'date_written', 'time_written' )

# switches
sep_spin  <- T    # separate spin cases from transient ones, done automatically if tsteps are different
zip       <- F    # zip concatenated netcdf
vsub      <- NULL # subscripts (or a character vector) to put only those variables into new netcdf and RDS files
yrzero    <- F    # ignore first year for spinups, currently automatic needs dev 
to_annual <- T    # where tsteps > 1 also produce an annual RDS file  
varconv   <- T    # covert variables using functions in var_conv list
timeconv  <- T    # covert variables using functions in time_conv list, only implemented if varconv also TRUE (necessary?) 



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



#######################################
# Functions

conv_sh_to_rh <- function(q, t, p=101325 ) {
  # convert specific humidity to relative
  # Buck 1981 used for sat vp (as does SDGVM)
  
  # q - water:dry air mass ratio, g/g
  # t - temperature,              K
  # p - pressure,                 Pa
  
  paw  <- p / (0.62198/q + 1)
  paws <- 6.1121 * exp(17.502*(t-273.15) / (240.97 + t - 273.15)) * 100
  #print(c(paw,paws))
  paw/paws * 1000      # units for SDGVM 0.1 % or parts per mil
}


# conversion functions 
conv_K_to_oC          <- function(v,...) (v - 273.15)                              # converts K to oC
conv_kgpyear_to_gpsec <- function(v,...)  v * 1e3 / (3600*24*365)                  # kg yr-1 to g s-1   
conv_psecond_to_pts   <- function(v,tsteps_in_yr,...) v * 3600*24*365/tsteps_in_yr # per second units to the timestep of the data     
conv_pday_to_pts      <- function(v,tsteps_in_yr,...) v * 365/tsteps_in_yr         # per second units to the timestep of the data     


# unit conversion functions and new units
# - elements of list named with units variable in the netcdf file
# - water units in kg (mm) m-2
# - C units in g m-2
var_conv <- list(
  K = list(
    func = conv_K_to_oC,
    newunits = 'oC'
  ),
  `kgC/m2/yr` = list(
    func = conv_kgpyear_to_gpsec,
    newunits = 'gC/m2/s'
  )
)


# time unit conversion functions and new units
# - elements of list named with units variable in the netcdf file
time_conv <- list(
  `gC/m^2/s` = list(
    func = conv_psecond_to_pts,
    newunits = 'gC/m2/timestep'
  ), 
  `gC/m2/s` = list(
    func = conv_psecond_to_pts,
    newunits = 'gC/m2/timestep'
  ), 
  `mm/s` = list(
    func = conv_psecond_to_pts,
    newunits = 'mm/timestep'
  ), 
  `m2 m-2 d-1` = list(
    func = conv_pday_to_pts,
    newunits = 'm2 m-2 timestep-1'
  )
)


breakout_cases <- function(v, spinss ) {
  list(
    spins=v[spinss],
    trans=v[-spinss]
  )
} 


# this summarises an array and preserves names/labelling
# - default takes a single time dimension and summarises by a certain number of timesteps
summarize_array <- function(a1, summarise_dim='time', extent=365, scale=NULL, func=mean ) {
  
  # determine scale vector
  if(is.null(scale)) {
    scale <- rep(1,extent)
  } else if(length(scale)!=extent) {
    stop('sumarise_array: scale argument must be of length extent.')
  }

  # summarize array
  keep_dims <- (1:length(dim(a1)))[-which(names(dim(a1))==summarise_dim)] 
  a2 <- apply(a1, keep_dims, function(v) apply(matrix(v,extent)*scale,2,func) )
  
  # update names(dim) and dimnames
  dn <- dimnames(a2)
  names(dim(a2)) <- names(dimnames(a2))
  dimnames(a2)   <- dn
  
  # permute to original dim order
  perm_dims <- match(names(dim(a1)), names(dim(a2)) )
  a2 <- aperm(a2, perm_dims )
  
  a2
}


# change units in an array
# - requires that the last dimension of the array is the variables to convert, labeled and named vars
var_conv_array <- function(a1, var_conv, vars_units, tstep ) {
  
  # convert to matrix - assumes that vars is the last dimension
  adim      <- dim(a1)
  ndim      <- length(adim)
  adimnames <- dimnames(a1)
  if(which(names(adimnames)=='vars')!=ndim) stop('last array dimension not the var dimension')
  m         <- apply(a1, ndim, function(v) v )
  
  # apply conversion functions over second dimension of array
  for(var in adimnames$vars) {
    vars_units[[var]]
    if(!is.null(var_conv[[vars_units[[var]]]])) {
      print(paste('convert var:',var,'from',vars_units[[var]],'to',var_conv[[vars_units[[var]]]]$newunits), quote=F )
      m[,var] <- var_conv[[vars_units[[var]]]]$func(m[,var], tstep ) 
    }
  }
  
  # convert matrix back to array
  array(m, adim, adimnames )
}



### Start processing
#######################################
wd_src <- getwd()

print('',quote=F)
print('Processing cases in model output directory:',quote=F)
print(wd_mod_out,quote=F)

if(is.null(wd_out)) {
  wd_out <- paste0(wd_mod_out,'/',caseidprefix,'_processed')
  setwd(wd_mod_out)
}
if(!file.exists(wd_out)) dir.create(wd_out)

print('',quote=F)
print('output directory:',quote=F)
print(wd_out,quote=F)
print('',quote=F)


# separate spins from transient
if(sep_spin | length(unique(tsteps))!=1) {
  spinss <- which(grepl('1850', cases ))
  cases  <- breakout_cases(cases, spinss ) 
  syear  <- breakout_cases(syear, spinss ) 
  years  <- breakout_cases(years, spinss ) 
  tsteps <- breakout_cases(tsteps, spinss ) 
}


# cases loop
c <- 1
for(c in 1:length(cases)) {
# for(c in 2 ) {
  setwd(wd_mod_out)
  
  cases_current   <- cases[[c]] 
  syear_current   <- syear[[c]] 
  years_current   <- years[[c]] 
  tsteps_current  <- tsteps[[c]] 
  ntsteps_current <- sum(years_current * tsteps_current)
  
  # combine sim variables to get all simulations
  sims <- apply(as.matrix(expand.grid(caseidprefix,sites,cases_current)), 1, paste, collapse='_' )
  print('',quote=F);print('',quote=F);print('',quote=F)
  print('Processing (& concatenating) case(s):',quote=F)
  print(sims,quote=F)
  
  s <- 1
  for(s in 1:length(sims)) {
    setwd(paste(wd_mod_out,sims[s],'run',sep='/'))

    print('',quote=F)
    print('Processing case:',quote=F)
    print(sims[s],quote=F)
    
    if(sims[s]==sims[1]) {
      print('',quote=F)
      print('Setting up new netcdf file ... ',quote=F)

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
      setwd(wd_out)
      new_fname <- paste(caseidprefix,sites,names(cases)[c],sep='_')
      newnc     <- nc_create(paste0(new_fname,'.nc'), newvars )
      tend_prev <- tcaug <- 0
      setwd(paste(wd_mod_out,sims[s],'run',sep='/'))
      
      # list of variables & key info in new file 
      vars_list <- lapply(newnc$var, 
                          function(l) list(
                            name     = l$name,
                            longname = l$longname,
                            dims     = sapply(l$dim, function(l) l$name ),
                            len      = sapply(l$dim, function(l) l$len ),
                            units    = l$units
                          ))
      print('done.',quote=F)
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
    print('',quote=F)
    print('Reading data and adding to new netcdf file ... ',quote=F)
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
    print('done.',quote=F)

    
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
  print('',quote=F)
  print('Creating R arrays ... ',quote=F)
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
      a[cbind(amss,vss)] <- ncvar_get(newnc, v )
    }
    
    # combine arrays into a list
    al <- c(al, list(a) )
    names(al)[length(al)] <- dim_combos[dc]
  }
  print('done.',quote=F)
  
  # close/write nc file
  nc_close(newnc)
  # zip nc file
  if(zip) system(paste('gzip ', paste0(new_fname,'.nc') ))

    
  # Create annual data from daily data if requested
  ald <- NULL
  if(to_annual & tsteps_current[1]>1) {
    print('',quote=F)
    print('Aggregating sub-annual data ... ',quote=F)
    
    ald <- lapply(al, function(a) {
      adim <- dim(a)
      if('time'%in%names(adim) & !any(names(adim)%in%char_dims)) {
        a <- summarize_array(a, summarise_dim='time', extent=tsteps_current[1] )
      }
      a
    }) 
    
    print('done.',quote=F)
  }


  # Variable conversion if requested
  if(varconv) {
    print('',quote=F)
    print('Variable conversions for R arrays ... ',quote=F)
    
    al  <- lapply(al, var_conv_array, var_conv=var_conv, vars_units=vars_units, tstep=tsteps_current[1] ) 
    ald <- if(!is.null(ald)) 
           lapply(ald, var_conv_array, var_conv=var_conv, vars_units=vars_units, tstep=tsteps_current[1] )
    
    vars_units <- lapply(vars_units, function(c) 
      if(is.null(var_conv[[c]])) c else var_conv[[c]]$newunits )
    print('done.',quote=F)
    
    if(timeconv) {
      print('Time unit conversions for R arrays ... ',quote=F)
      
      al  <- lapply(al, var_conv_array, var_conv=time_conv, vars_units=vars_units, tstep=tsteps_current[1] ) 
      ald <- if(!is.null(ald)) 
        lapply(ald, var_conv_array, var_conv=time_conv, vars_units=vars_units, tstep=1 )
      
      vars_units <- lapply(vars_units, function(c) 
        if(is.null(time_conv[[c]])) c else time_conv[[c]]$newunits )
      print('done.',quote=F)
    }
    
    # update vars_list
    vars_list <- lapply(vars_list, function(l) {
      l$units <- vars_units[[l$name]]
      l
    })
  }

  
  # create output list(s) & save RDS file(s)
  print('',quote=F)
  print('Writing RDS file(s) ... ',quote=F)
  
  setwd(wd_out)
  l1 <- list(dimensions=dlen, dim_combinations=dc_nvars, variables=vars_list, data_arrays=al )
  saveRDS(l1, paste0(new_fname,'.RDS') )
  if(!is.null(ald)) {
    l1 <- list(dimensions=dlen, dim_combinations=dc_nvars, variables=vars_list, data_arrays=ald )
    saveRDS(l1, paste0(new_fname,'_annual.RDS') )
  }
  print('done.',quote=F)
  
  # cases loop  
}



### END ###