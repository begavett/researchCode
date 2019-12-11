# source("https://raw.githubusercontent.com/begavett/researchCode/master/R/plotMplusLongInteraction.R")

# Usage: To plot 3-way interactions derived from Mplus latent growth curve model results (linear only)
# This function reads in the Mplus .out file, so the working directory should be set accordingly.
# Factor 1 levels are shown as separate lines with user-specified levels
# Factor 2 levels are shown as separate facets with user-specified levels

# This returns a ggplot2 object, which can then be further modified as desired for custom plotting.

# outfile (string length 1): name of Mplus .out file, with .out extension included
# dvlabel (string length 1): name of dependent variable (to be plotted as y-axis label)
# factor1 (string length 1): how Mplus represents the name of factor 1
# f1label (string length 1): How you want the plot to show the label for factor 1 (in legend)
# f1levels (numeric vector length > 1): Numeric representation of factor 1 levels to be plotted
# f1labels (string length > 1): Character representation of factor 1 level labels
# factor2 (string length 1): how Mplus represents the name of factor 2
# f2levels (numeric vector length > 1): Numeric representation of factor 2 levels to be plotted
# f2labels (string length > 1): Character representation of factor 2 level labels
# intXvar (string length 1): how Mplus represents the name of the interaction variable
# icptvar (string length 1): how Mplus represents the name of the latent intercept (usually "I")
# slpvar (string length 1): how Mplus represents the name of the latent slope (usually "S")
# timespan (numeric vector length > 1): Numeric representation of time units to be plotted on x-axis
# timeunits (string length 1): Character representation of units of time (used to label x-axis)
# std (string length 1): Which type of parameter estimates should be plotted? Defaults to unstandardized.
# linewidth (numeric vector length 1): size of lines drawn

# Example (not run):

# plotMplusLongInteraction(outfile = "decomposition and lgm categorical moderator NEW wTECH3.out",
#                          dvlabel = "ADNI-EF Score (M=0, SD=1)",
#                          factor1 = "MEMR",
#                          f1label = "Cognitive Reserve",
#                          f1levels = -1:1,
#                          f1labels = c("Low (-1 SD)", "Average (0 SD)", "High (+1 SD)"),
#                          factor2 = "PATHPOS",
#                          f2levels = 0:1,
#                          f2labels = c("AD Biomarker Negative", "AD Biomarker Positive"),
#                          intXvar = "INTCAT",
#                          icptvar = "I",
#                          slpvar = "S",
#                          timespan = 0:5,
#                          timeunits = "years",
#                          std = "US",
#                          linewidth = 1)
                         

plotMplusLongInteraction <- function(outfile = "decomposition and lgm categorical moderator NEW wTECH3.out", 
                                     dvlabel = "ADNI-EF Score (M=0, SD=1)",
                                     factor1 = "MEMR",
                                     f1label = "Cognitive Reserve",
                                     f1levels = -1:1,
                                     f1labels = c("Low (-1 SD)", "Average (0 SD)", "High (+1 SD)"),
                                     factor2 = "PATHPOS",
                                     f2levels = 0:1,
                                     f2labels = c("AD Biomarker Negative", "AD Biomarker Positive"),
                                     intXvar = "INTCAT",
                                     icptvar = "I",
                                     slpvar = "S",
                                     timespan = 0:5,
                                     timeunits = "years",
                                     std = c("US", "STDY", "STDYX"),
                                     linewidth = 1){
  require(dplyr)
  require(ggplot2)
  require(cowplot)
  require(MplusAutomation)
  
  results <- MplusAutomation::readModels(target = outfile)
  if(length(std) > 1) std <- "US"
  if(std == "US") PEs <- results$parameters$unstandardized
  if(std == "STDY") PEs <- results$parameters$stdy.standardized
  if(std == "STDYX") PEs <- results$parameters$stdyx.standardized
  dat <- expand.grid(f1levels, f2levels)
  dat$Var1xVar2 <- dat$Var1*dat$Var2
  dat <- dplyr::bind_rows(replicate(length(timespan), expr = {list(dat)}))
  dat$Time <- rep(timespan, each = nrow(dat)/length(timespan))
  dat$b0_I <- as.numeric(subset(PEs, paramHeader == "Intercepts" & param == icptvar, select = est))
  dat$b1_I <- as.numeric(subset(PEs, paramHeader == paste0(icptvar, ".ON") & param == factor1, select = est))
  dat$b2_I <- as.numeric(subset(PEs, paramHeader == paste0(icptvar, ".ON") & param == factor2, select = est))
  dat$b3_I <- as.numeric(subset(PEs, paramHeader == paste0(icptvar, ".ON") & param == intXvar, select = est))
  dat$b0_S <- as.numeric(subset(PEs, paramHeader == "Intercepts" & param == slpvar, select = est))
  dat$b1_S <- as.numeric(subset(PEs, paramHeader == paste0(slpvar, ".ON") & param == factor1, select = est))
  dat$b2_S <- as.numeric(subset(PEs, paramHeader == paste0(slpvar, ".ON") & param == factor2, select = est))
  dat$b3_S <- as.numeric(subset(PEs, paramHeader == paste0(slpvar, ".ON") & param == intXvar, select = est))
  dat$Intercept <- with(dat, b0_I + b1_I*Var1 + b2_I*Var2 + b3_I*Var1xVar2)
  dat$Slope <- with(dat, b0_S + b1_S*Var1 + b2_S*Var2 + b3_S*Var1xVar2)
  dat$Yhat <- with(dat, Intercept + Slope*Time)
  
  if(length(f1labels) != length(f1levels)) f1labels <- factor(as.character(f1levels))
  if(length(f2labels) != length(f2levels)) f2labels <- factor(as.character(f2levels))
  
  dat$Var1F <- factor(dat$Var1, levels = f1levels, labels = f1labels)
  dat$Var2F <- factor(dat$Var2, levels = f2levels, labels = f2labels)
  
  p <- ggplot(dat, aes(x = Time, y = Yhat, colour = Var1F, lty = Var1F)) + geom_line(size = linewidth) +
    facet_wrap(~Var2F) + ylab(dvlabel) + xlab(paste0("Time (", timeunits,")")) +
    scale_colour_discrete(name = f1label) + 
    scale_linetype_discrete(name = f1label) +
    cowplot::theme_cowplot()
  plot(p)
  return(p)
}
