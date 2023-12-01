##############################
#
# Plotting lists - each list describes a separate plotting pdf
#
# AWalker
# April 2021
#
##############################

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
    vvars = c('FATES_GPP','FATES_NPP','FATES_AUTORESP'),
    ylab  = expression('C Flux [gC '*m^-2*' timestep'^-1*']')
  ),
  p1 = list(
    vvars = c('FATES_CROOT_ALLOC','FATES_FROOT_ALLOC','FATES_LEAF_ALLOC','FATES_SEED_ALLOC','FATES_STEM_ALLOC','FATES_STORE_ALLOC'),
    ylab  = expression('C Flux [gC '*m^-2*' timestep'^-1*']')
  ),
  p1.1 = list(
    stackplot = T,
    norm  = F, 
    vvars = c('FATES_CROOT_ALLOC','FATES_FROOT_ALLOC','FATES_LEAF_ALLOC','FATES_SEED_ALLOC','FATES_STEM_ALLOC','FATES_STORE_ALLOC'),
    ylab  = expression('C Flux [gC '*m^-2*' timestep'^-1*']')
  ),
  p1.2 = list(
    stackplot = T,
    norm  = T, 
    vvars = c('FATES_CROOT_ALLOC','FATES_FROOT_ALLOC','FATES_LEAF_ALLOC','FATES_SEED_ALLOC','FATES_STEM_ALLOC','FATES_STORE_ALLOC'),
    ylab  = expression('C Flux [gC '*m^-2*' timestep'^-1*']')
  ),
  p2 = list(
    vvars = c('FATES_STRUCTC','FATES_NONSTRUCTC','FATES_FROOTC','FATES_VEGC','FATES_VEGC_ABOVEGROUND','FATES_LEAFC','FATES_SAPWOODC','FATES_STOREC','FATES_REPROC'),
    ylab  = expression('Veg C Pool [gC '*m^-2*']')
  ),
  p2.1 = list(
    vvars = c('SOILC'),
    ylab  = expression('Soil C Pool [gC '*m^-2*']')
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
    vvars = c('FATES_PROMOTION_CARBONFLUX','FATES_DEMOTION_CARBONFLUX','FATES_MORTALITY_CFLUX_CANOPY','FATES_MORTALITY_CFLUX_USTORY'),
    ylab  = expression('C Flux [gC '*m^-2*' timestep'^-1*']')
  ),
  p8 = list(
    vvars = c('FATES_DISTURBANCE_RATE_FIRE','FATES_DISTURBANCE_RATE_LOGGING','FATES_DISTURBANCE_RATE_P2P','FATES_DISTURBANCE_RATE_P2S',
              'FATES_DISTURBANCE_RATE_POTENTIAL','FATES_DISTURBANCE_RATE_S2S','FATES_DISTURBANCE_RATE_TREEFALL'),
    ylab  = 'Fraction of ground area disturbed [0-1]'  
  ),
  p9 = list(
    vvars = c('FATES_NCOHORTS','FATES_NPATCHES'),
    ylab  = 'FATES diagnostics [#]'
  )
)


plotlist_nutrients <- list(
  p1 = list(
    vvars = c('FATES_NONSTRUCTC','FATES_VEGC'),
    ylab  = expression('C Pool [gC '*m^-2*']')
  ),
  p2 = list(
    vvars = c('FATES_VEGN','FATES_SAPWOODN','FATES_LEAFN','FATES_FROOTN'),#'SOILN'),
    ylab  = expression('N Pool [gN '*m^-2*']')
  ),
  p3 = list(
    vvars = c('FATES_VEGP','FATES_SAPWOODP','FATES_LEAFP','FATES_FROOTP'),#'SOILP'),
    ylab  = expression('P Pool [gP '*m^-2*']')
  ),
  p4 = list(
    #vvars = c('NUPTAKE','NNEED','NEFFLUX'),
    vvars = c('FATES_NO3UPTAKE','FATES_NH4UPTAKE','FATES_NDEMAND','FATES_NFIX_SYM','FATES_NEFFLUX'),
    ylab  = expression('N Flux [gN '*m^-2*' timestep'^-1*']')
  ),
  p5 = list(
    vvars = c('FATES_PUPTAKE','FATES_PDEMAND','FATES_PEFFLUX'),
    ylab  = expression('P Flux [gP '*m^-2*' timestep'^-1*']')
  ),
  p6 = list(
    vvars = c('GROSS_NMIN','NET_NMIN'), 
    ylab  = expression('N Flux [gN '*m^-2*' timestep'^-1*']')
  ),
  p7 = list(
    vvars = c('BIOCHEM_PMIN','BIOCHEM_PMIN_TO_PLANT','GROSS_PMIN','NET_PMIN'), 
    ylab  = expression('P Flux [gP '*m^-2*' timestep'^-1*']')
  ),
  px = list(
    vvars = c('FATES_STOREC','FATES_STOREN','FATES_STOREP'),
    ylab  = expression('C/N/P Store [g '*m^-2*']')
  ),
  px.1 = list(
    vvars = c('FATES_STOREN','FATES_STOREP'),
    ylab  = expression('N/P Store [g '*m^-2*']')
  )
)


plotlist_phys <- list(
  p0 = list(
    vvars = c('FATES_GPP','FATES_AUTORESP'),
    ylab  = expression('C Flux [gC '*m^-2*' timestep'^-1*']')
  ),
  p0a = list(
    vvars = c('QVEGT','QVEGE'),
    ylab  = expression('H2O Flux [kg '*H[2]*O*' '*m^-2*' timestep'^-1*']')
  ),
  p2 = list(
    vvars = c('TLAI','FATES_TRIMMING'),
    sum_vars='TLAI',
    div_sumvars='FATES_TRIMMING',
    ylab  = expression('LAI ['*m^2*m^-2*'], LAI trimming [0-1]')
  ),
  p1c = list(
    vvars = NULL,
    ylab  = expression('veg WUE [g.'*kg^-1*']'),
    sum_vars = c('FATES_GPP'),
    div_sumvars = c('QVEGT')
  ),
  p1d = list(
    vvars = NULL,
    ylab  = expression('ecosystem WUE [g.'*kg^-1*']'),
    sum_vars = c('FATES_GPP'),
    div_sumvars = c('QVEGT','QVEGE','QSOIL')
  ),
  p1.1 = list(
    vvars = NULL,
    ylab  = expression('iWUE GPP/'*g[c]*' ['*mu*mol*' C '*mol^-1*' '*H[2]*O*']'),
    sum_vars = list(
      sum_vars=c('FATES_GPP'),
      scale=1e12/(86400*12) # convert g C umol-1 H2O to umol C mol-1 H2O
      ),
    div_prodvars = c('FATES_STOMATAL_COND','TLAI')
  ),
  p1a = list(
    vvars = c('FATES_STOMATAL_COND'),
    sum_vars='FATES_GPP',
    product_vars=c('FATES_STOMATAL_COND','TLAI'),
    ylab  = expression(g['s,c']*' ['*mu*mol*' '*H[2]*O*' '*m^-2*' s'^-1*']')
  ),
  p1b = list(
    vvars = c('FATES_LBLAYER_COND'),
    ylab  = expression(g['s,c']*' ['*mu*mol*' '*H[2]*O*' '*m^-2*' s'^-1*']')
  ),
  p1 = list(
    vvars = c('BTRAN'),
    ylab  = expression('Plant water status [0-1]')
  ),
  p3 = list(
    vvars = c('RAIN','QVEGT','QVEGE','QSOIL'),
    ylab  = expression('H2O Flux [kg '*H[2]*O*' '*m^-2*' timestep'^-1*']'),
    sum_vars = c('QRUNOFF','QDRAI')
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
    dim_sub = 3:10,
    ylab    = expression('Soil Water Potential, '*Psi*' [MPa]')
  )
)



### END ###
