mplus.plot.bayesian.distributionBG <- function(file,
                                               paramNum = 1,
                                               bins=100, 
                                               cfill = color_scheme_get("blue")[1], 
                                               cline = "gray",
                                               main = NULL,
                                               qualPal = "Accent",
                                               ci = .95,
                                               intType = c("hdi", "qi"),
                                               plot = TRUE) {
  if (missing(file)) {
    stop("- name of the GH5 file is required")
  }
  if (!(file.exists(file))) {
    cstr <- paste("- file does not exist:",file,"\n")
    stop(cstr)
  }
  library(ggplot2)
  library(bayesplot)
  library(ggthemes)
  library(RColorBrewer)
  library(tidybayes)
  gh5 <- h5dump(file, load=TRUE)
  
  # check if bayesian data exists
  if ( !("bayesian_data" %in% names(gh5)) ) {
    stop("- requires bayesian data\n\nUse TYPE=PLOT2 setting in Mplus with a Bayesian analysis.")
  }
  paramstr <- trimws(gh5$bayesian_data$parameters_autocorr$statements[paramNum])
  if(is.null(main)) main <- paramstr
  if (missing(paramstr)) {
    stop("- requires the parameter label or index.\n\nUse mplus.list.bayesian.parameters to get the list of parameters.")
  }
  
  # the number of bins should be greater than 0
  if (bins <= 0) {
    stop("The number of bins should be greater than 0.")
  }
  
  # get the dimensions of parameters array
  # first dimension is the number of parameters
  # second dimension is the number of iterations
  # third dimension is the number of chains
  dims <- attr(gh5$bayesian_data$parameters_autocorr$parameters,"dim")
  #print(dims)
  
  statements <- mplus.get.group.dataset(file, 'bayesian_data/parameters_autocorr', 'statements')
  statements <- gsub("(^\\s+|\\s+$)", "", statements, perl=TRUE)
  
  if (is.character(paramstr)) {
    lcstatements <- tolower(statements)
    paramstr <- tolower(paramstr)
    paramidx <- pmatch(paramstr, lcstatements, nomatch=0)
    
    if (paramidx == 0) {
      cstr <- paste(c("- unknown parameter:"),paramstr,"\n\nUse mplus.list.bayesian.parameters to see the list of parameters.\n")
      stop(cstr)
    }
  } else {
    paramidx <- paramstr
    if (paramidx < 1 || paramidx > dims[1]) {
      cstr <- paste(" - parameter index is out of range: ",paramidx,"\n\nUse mplus.list.bayesian.parameters to see the list of parameters.\n")
      stop(cstr)
    }
  }
  label <- statements[paramidx]
  
  ndist <- mplus.get.dataset.attribute(file, 'bayesian_data/parameters_autocorr/parameters', 'ndistribution')
  
  yy <- array(0, c(dims[2],dims[3]))
  if (ndist == dims[2]) {
    xx <- array(0, c(dims[2]*dims[3]))
  } else {
    xx <- array(0, c((dims[2]-ndist)*dims[3]))
  }
  
  for (i in c(1:dims[3])) {
    yy[,i] <- mplus.get.bayesian.parameter.data(file, paramidx, i)
  }
  start <- 0
  for (i in c(1:dims[3])) {
    if (ndist == dims[2]) {
      for (j in c(1:dims[2])) {
        start <- start + 1
        #cstr <- paste(start, j, i)
        #print(cstr)
        #print(xxc[j,i])
        xx[start] <- yy[j,i]
      }
    } else {
      for (j in c((ndist+1):dims[2])) {
        start <- start + 1
        #cstr <- paste(start, j, i)
        #print(cstr)
        #print(xxc[j,i])
        xx[start] <- yy[j,i]
      }
    }
  }
  
  xxx <- data.frame(values = xx)
  cstr <- paste(c("Distribution of:"),label)
  h <- hist(xx,breaks=seq(min(xx),max(xx),length=bins+1), plot = FALSE)
  
  xxmode <- h$mids[h$counts == max(h$counts)]
  xxmean <- mean(xx)
  xxsd <- sd(xx)
  xxmedian <- median(xx)
  intType <- intType[1]
  if(intType == "hdi") {
    left <- tidybayes::hdi(xx, .width = ci)[,1]
    right <- tidybayes::hdi(xx, .width = ci)[,2]
  }
  if(intType == "sym") {
    left <- tidybayes::qi(xx, .width = ci)[,1]
    right <- tidybayes::qi(xx, .width = ci)[,2]
  }
  
  modestr <- sprintf("Mode = %0.3f", xxmode)
  meanstr <- sprintf("Mean = %0.3f (SD = %0.3f)", xxmean, xxsd)
  medianstr <- sprintf("Median = %0.3f", xxmedian)
  intLab <- ifelse(intType == "hdi", "HDI", "QI")
  lowci <- sprintf(paste0("95%% Lower ", intLab, " = %0.3f"), left)
  uppci <- sprintf(paste0("95%% Upper ", intLab, " = %0.3f"), right)
  ldesc <- c(meanstr, medianstr, modestr, lowci, uppci)
  
  
  p <- ggplot(xxx, aes(x = values)) + geom_histogram(bins = bins, fill = cfill, colour = cline) + 
    theme_cowplot() + xlab("Estimate") + ylab("Count") + ggtitle(main) + geom_vline(aes(xintercept = xxmode, colour = modestr)) +
    geom_vline(aes(xintercept = xxmean, colour = meanstr)) + geom_vline(aes(xintercept = xxmedian, colour = medianstr)) +
    geom_vline(aes(xintercept = left, colour = lowci)) + 
    geom_vline(aes(xintercept = right, colour = uppci)) +
    scale_colour_manual(name = "Posterior Distribution Properties", values = c(brewer.pal(4, qualPal)[1], brewer.pal(4, qualPal))) + 
    theme(legend.direction = "vertical", 
          legend.position = "bottom",
          legend.box = "horizontal",
          legend.title.align=0.5) + guides(colour=guide_legend(ncol=2,byrow=TRUE))
  
  if(plot) plot(p)
  results <- data.frame(pLT0 = sum(xx < 0)/length(xx),
                        pGT0 = sum(xx > 0)/length(xx))
  out <- list(plot = p,
              data = xx,
              summary = results)
  #out[[1]] <- p
  #out[[2]] <- xx
  return(out)
}
