##############################
#
# Functions for plotting processed ELM/CLM netcdf files 
#
# AWalker
# April 2021
#
##############################

library(ggplot2)
library(png)



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


plot_stack <- function(a3d=algtime, vvars=c('FATES_CROOT_ALLOC','FATES_FROOT_ALLOC','FATES_LEAF_ALLOC','FATES_SEED_ALLOC','FATES_STEM_ALLOC','FATES_STORE_ALLOC'),
                       norm=F, xdim='time', ylabi, xlabi,
                       vcol=NULL, leg_cols=4, print2screen=T, ... ) {
  lv <- length(vvars)
  lx <- dim(a3d)[xdim]
  if(is.null(vcol)) vcol <- viridis(lv)
 
  as   <- a3d[,,vvars]
  tas  <- as.data.frame(as)
  stas <- stack(tas, vvars )
 
  if(norm) {
    sumas <- apply(as, 1, sum )
    stas$values <- stas$values / sumas
  }

  p1 <- 
    ggplot(stas, aes(x=rep(1:lx,lv), y=values, fill=ind)) +
    geom_area() +
    ylab(ylabi) + 
    xlab(xlabi) + 
    labs(fill='')
  
  if(print2screen) print(p1)
  p1
}


plot_3dim_combvars <- function(a3d=algtime, vvars=c('GPP'), sum_vars=c('NPP','AR'), 
                               product_vars=NULL, 
                               subtract_vars=NULL, 
                               div_sumvars=NULL,
                               div_prodvars=NULL,
                               div_zero=1e-2,
                               xdim='time', vcol=NULL, leg_cols=4, print2screen=T,
                               days=1, ... ) {
  
  lv <- length(vvars) + !is.null(sum_vars) 
  lx <- dim(a3d)[xdim]
  
  # averaging / summing
  sum_vars.scale <- 1
  if(is.list(sum_vars)) {sum_vars.scale <- sum_vars$scale*1/days; sum_vars <- sum_vars$sum_vars }

  asum    <- apply(a3d[,,sum_vars,drop=F]*sum_vars.scale, 2:1, sum )
  nv_name <- paste0('sum(',paste(sum_vars,collapse=','),')')
  if(!is.null(product_vars)) {
    asum <- apply(a3d[,,product_vars,drop=F], 2:1, prod )
    nv_name <- paste0('prod(',paste(product_vars,collapse=','),')')
  }  
  if(!is.null(subtract_vars)) {
    asub <- apply(a3d[,,subtract_vars,drop=F], 2:1, sum )
    asum <- asum[drop=F] - asub[drop=F]
    nv_name <- paste0(nv_name,' - sum(',paste(subtract_vars,collapse=','),')')
  } else if(!is.null(div_sumvars)) {
    adiv <- apply(a3d[,,div_sumvars,drop=F], 2:1, sum )
    adiv[adiv<div_zero] <- NA
    asum <- asum[drop=F] / adiv[drop=F]
    nv_name <- paste0(nv_name,' / sum(',paste(div_sumvars,collapse=','),')')
  } else if(!is.null(div_prodvars)) {
    adiv <- apply(a3d[,,div_prodvars,drop=F], 2:1, prod )
    adiv[adiv<div_zero] <- NA
    asum <- asum[drop=F] / adiv[drop=F]
    nv_name <- paste0(nv_name,' / prod(',paste(div_prodvars,collapse=','),')')
  }
  
  
  groups <- c(vvars, nv_name  )
  if(is.null(vcol))           vcol <- viridis(length(groups))
  if(length(groups)<leg_cols) leg_cols <- length(groups)

  dummy_var <- if(!('FATES_TRIMMING'%in%vvars)) 'FATES_TRIMMING' else 'FATES_CROOT_ALLOC'  
  a3d[,,dummy_var] <- asum[,]
  dimnames(a3d)[[3]][which(dimnames(a3d)[[3]]==dummy_var)] <- nv_name
  
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


make_figures <- function(a3d, a4d, plotlist, xlab='Spin-up', timestep='year', print2screen=F ) {
  
  lapply(plotlist, function(l) {
    
    print('',quote=F)
    print('Making figure:',quote=F)
    print(paste('vars:',paste(l$vvars,collapse=',')),quote=F)
    combvars <- F
    if(!is.null(l$sum_vars))      {print(paste('sum vars:',paste(l$sum_vars,collapse='+')),quote=F); combvars <- T }
    if(!is.null(l$subtract_vars)) {print(paste('subtract vars:',paste(l$subtract_vars,collapse='+')),quote=F); combvars <- T }
    if(!is.null(l$div_sumvars))   {print(paste('divide vars:',paste(l$div_sumvars,collapse='+')),quote=F); combvars <- T }
    if(!is.null(l$div_prodvars))  {print(paste('divide vars:',paste(l$div_prodvars,collapse='*')),quote=F); combvars <- T }
    
    days <- 1
    if(timestep=='year') {
      days <- 365
    }
   
    if(!is.null(l$stackplot)) {
      plot_stack(a3d, vvars=l$vvars, norm=l$norm, ylabi=l$ylab, xlabi=paste(xlab,timestep), print2screen=print2screen, days=days )

    } else if(is.null(l$v4d)) {
      if(combvars) {
        plot_3dim_combvars(a3d, sum_vars=l$sum_vars, product_vars=l$product_vars, 
                           subtract_vars=l$subtract_vars, 
                           div_sumvars=l$div_sumvars, div_prodvars=l$div_prodvars,
                           vvars=l$vvars, ylab=l$ylab, xlab=paste(xlab,timestep), 
                           print2screen=print2screen, days=days )
      } else {
        plot_3dim(a3d, vvars=l$vvars, ylab=l$ylab, xlab=paste(xlab,timestep), print2screen=print2screen, days=days )
      }

    } else {
      if(!is.null(a4d)) {
        plot_4dim(a4d, vvars=l$vvars, dim_sub=l$dim_sub, ylab=l$ylab, xlab=paste(xlab,timestep), 
                  print2screen=print2screen, days=days )
      } else {
        warning('a4d argument not given, no 4d plots.')
      }
    }
  })
}


plot_figures <- function(plots, plotname='plots.pdf', nper_page=3, png=F ) {
  print('', quote=F )
  if(png) plotname <- paste0(strsplit(plotname,'.')[[1]], '_png.pdf')
  print(paste('Saving figures to:',plotname), quote=F )
 
  if(png) {
    plotlist_png(plots, plotname )
 
  } else { 
    pdf(plotname, width=8.5, height=11 )
    lapply(1:length(plots), function(p) {
      print(plots[p], 
            split=c(1,p%%nper_page+if(p%%nper_page==0) nper_page else 0,1,nper_page), 
            more=p%%nper_page )
      numeric(0)
    })
    dev.off()
 
  }
}


plotPNG <- function(png) {
  plot(c(0,1),c(0,1),type="n")
  rasterImage(png,0,0,1,1)
}


plotlist_png <- function(plotlist, fname='plotlist_png.pdf', ... ) {

  if(!file.exists('plots')) dir.create('plots')
  setwd('plots')

  # plot as png's
  png('list-test%03d.png')
  eval(plotlist)
  dev.off()

  # read png's to list
  plotlist_png <- lapply(1:length(plotlist), function(i) readPNG(paste0("list-test",formatC(i, width=3, format="d", flag="0"),".png")) )
  
  # write png's to single pdf
  setwd('..')
  pdf(fname)
  par(mai=c(0,0,0,0))
  dummy <- lapply(plotlist_png, plotPNG )
  dev.off()
}





### END ###
