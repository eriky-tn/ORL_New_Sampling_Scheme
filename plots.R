################################################################################
#
# This code is related to the plots of the article:
# Gomes, E.S. Cruz, F.R.B. (2026) Novel Sampling Strategy for Classical Inference 
# in M/M/1 Queues, Operations Research Letters 67:107461.
#
# (c) 2026, Gomes & Cruz.
# v.2026.03.13
#
################################################################################

rm(list = ls())

library(this.path) # use relative path
library(ggplot2) # for plotting
library(dplyr) # for data analysis
library(reshape2) # for melt


################################################################################
# Auxiliary functions
################################################################################

# organize directory for plotting
setwd(this.path::here())
output_path <- file.path(getwd(), 'figures')
if(!dir.exists(output_path)) dir.create(output_path)


################################################################################
# basic plotting layout
################################################################################

# some manual customization variables ------------------------------------------

{
#cpManual<-c('black','blue','red2','green4','orange3','purple2','navyblue','cyan3',
#            'salmon4','gold','violet','limegreen','springgreen','slateblue4')
cpManual<-c('red', 'black', 'blue','green4',
            'red', 'black', 'blue','green4',
            'orange2')
#ltManual<-c('dotdash','longdash','dashed','dotted','solid')
ltManual<-c('solid', 'solid','solid', 'solid',
            'dashed', 'dashed','dashed','dashed',
            'dotted')
shapeManual<-c(seq(1,9))

# uncomment these lines to reproduce MLE comparison plots
  #cpManual<-c('red', 'red', 'black','purple2') # mles
  #ltManual<-c('solid', 'dashed','longdash', 'dotted') # mle
  

# basic plot
ggBase <- ggplot() + theme_minimal() + labs(color='',linetype='',shape='') +
  theme(
    panel.background = element_rect(fill='white',color='black'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #panel.grid.major = element_line(linewidth = 0.5,linetype='solid',color='gray90'),
    #panel.grid.minor = element_line(linewidth = 0.5, linetype='solid',color='gray90'),
    axis.ticks = element_line(),
    axis.ticks.length = unit(c(-0,1),'mm'),
    #legend.box.spacing = unit(0,'pt'),
    legend.margin = margin(t=1,r=1,b=1,l=1,unit='mm')
  ) +
  scale_linetype_manual(values=ltManual) +
  scale_color_manual(values=cpManual) +
  scale_shape_manual(values=shapeManual)
}

# plot function for estimators -------------------------------------------------
  
plotEst <- function(myData, var_x, var_y, label_x, label_y, file_name,
                    file_ext = '.pdf', x_lim = c(0, 1), y_lim = c(0, 1),
                    leg_position = c(0.97, 0.05), leg_justification = c(0, 1),
                    lbl_order = lbl_plot){
  
  if(!is.null(lbl_order)) myData$name <- factor(myData$name, levels = lbl_order)
  
  myPlot <- ggBase +
    xlab(label_x) + ylab(label_y) +
    xlim(x_lim) + ylim(y_lim) +
    #scale_x_continuous(breaks=seq(0,1,0.2)) +
    #scale_y_continuous(breaks=seq(-1,1,0.2)) +
    geom_line(aes_string(x = var_x, y = var_y, color = 'name', linetype = 'name'),
              myData, lwd = 0.3) +
    geom_point(aes_string(x = var_x, y = var_y, color = 'name', shape = 'name'),
               size = 1.5, myData) +
    # just sets a manual legend
    # annotate('text',
    #          x = 0.03 * (x_lim[2] - x_lim[1]) + x_lim[1],
    #          y = 0.99 * (y_lim[2] - y_lim[1]) + y_lim[1],
    #          label = paste0('r = ', r)) +
    theme(
      legend.position = 'inside',
      legend.position.inside = leg_position,
      legend.justification = leg_justification
    ) +
    guides(color = guide_legend(nrow = 4), shape = guide_legend(nrow = 4),
           linetype = guide_legend(nrow = 4))
  
  # for bias only
  if(var_y == 'bias'){
    myPlot <- myPlot +
      geom_abline(intercept = 0, slope = 0, color = 'black', lwd = 0.2)
  }
  
  # saving to file
    ggsave(
      file = paste0(file_name, file_ext),
      plot = myPlot,
      width = 5,
      heigh = 3
    )
  
  rm(myPlot)
}


################################################################################
# load the simulated data
################################################################################

load('results/simulation_mle_13-03-2026.rdata')
load('results/simulation_ads_13-03-2026.rdata')
load('results/simulation_sdep_13-03-2026.rdata')
load('results/simulation_beta1_13-03-2026.rdata')
load('results/simulation_beta2_13-03-2026.rdata')
load('results/simulation_beta3_13-03-2026.rdata')

setwd(output_path)

################################################################################
# organizing data for plotting
################################################################################

{
# labels for plotting
lbl_plot <- c(
  "MLE",
  "B(1, 1)",
  "B(1.5, 2.5)",
  "B(2.5, 1.5)",
  "MLE (conditional)",
  "B(1, 1) (conditional)",
  "B(1.5, 2.5) (conditional)",
  "B(2.5, 1.5) (conditional)"
)

sims_p  <- list(sim_mle_p,sim_beta1_p, sim_beta2_p, sim_beta3_p,
                sim_mle_p_cond, sim_beta1_p_cond, sim_beta2_p_cond, sim_beta3_p_cond)

sims_lq <- list(sim_mle_lq, sim_beta1_lq, sim_beta2_lq, sim_beta3_lq,
                sim_mle_lq_cond, sim_beta1_lq_cond, sim_beta2_lq_cond, sim_beta3_lq_cond)

sims_ls <- list(sim_mle_ls, sim_beta1_ls, sim_beta2_ls, sim_beta3_ls,
                sim_mle_ls_cond, sim_beta1_ls_cond, sim_beta2_ls_cond, sim_beta3_ls_cond)


make_df <- function(name, sim) cbind(name = name, sim)

df_p  <- do.call(rbind, Map(make_df, lbl_plot, sims_p)) %>% as.data.frame()
df_lq <- do.call(rbind, Map(make_df, lbl_plot, sims_lq))  %>% as.data.frame()
df_ls <- do.call(rbind, Map(make_df, lbl_plot, sims_ls))  %>% as.data.frame()
}

################################################################################
# organizing alternative models for plotting
################################################################################

{
# labels for plotting
lbl2_plot <- c(
  "MLE",
  'MLE (conditional)',
  "ADS",
  "SDEP"
)

sims2_p  <- list(sim_mle_p, sim_mle_p_cond, sim_ads_p, sim_sdep_p)
sims2_lq <- list(sim_mle_lq, sim_mle_lq_cond, sim_ads_lq, sim_sdep_lq)
sims2_ls <- list(sim_mle_ls, sim_mle_ls_cond, sim_ads_ls, sim_sdep_ls)

make_df <- function(name, sim) cbind(name = name, sim)

df2_p  <- do.call(rbind, Map(make_df, lbl2_plot, sims2_p)) %>% as.data.frame()
df2_lq <- do.call(rbind, Map(make_df, lbl2_plot, sims2_lq))  %>% as.data.frame()
df2_ls <- do.call(rbind, Map(make_df, lbl2_plot, sims2_ls))  %>% as.data.frame()
}

################################################################################
# plotting vs parameters
################################################################################

col_numeric <- 2:5
df_plot_p <- df_p # or df2_p for mle comparison
df_plot_lq <- df_lq # or df2_lq for mle comparison
df_plot_ls <- df_ls # or df2_ls for mle comparison
lbl_order = lbl_plot # or lbl2_plot for mle comparison

# grouping by parameter
{
myData_p <-
  df_plot_p %>%
  mutate_at(col_numeric, as.numeric) %>%
  mutate(bias = mean - p) %>%
  mutate(var = sd^2) %>%
  mutate(rmse = sqrt(bias^2 + sd^2)) %>%
  filter(size != 500) %>%
  group_by(name, p) %>%
  summarise_at(vars(mean, bias, sd, var, rmse), mean)

myData_lq <-
  df_plot_lq %>%
  filter(p != 0.01 & p!= 0.99) %>%
  mutate_at(col_numeric, as.numeric) %>%
  mutate(lq = round(p^2 / (1 - p), 1)) %>%
  mutate(bias = mean - lq) %>%
  mutate(var = sd^2) %>%
  mutate(rmse = sqrt(bias^2 + sd^2)) %>%
  filter(size != 500) %>%
  group_by(name, lq) %>%
  summarise_at(vars(mean, bias, sd, var, rmse), mean)

myData_ls <-
  df_plot_ls %>%
  filter(p != 0.01 & p!= 0.99) %>%
  mutate_at(col_numeric, as.numeric) %>%
  mutate(ls = round(p / (1 - p), 1)) %>%
  mutate(bias = mean - ls) %>%
  mutate(var = sd^2) %>%
  mutate(rmse = sqrt(bias^2 + sd^2)) %>%
  filter(size != 500) %>%
  group_by(name, ls) %>%
  summarise_at(vars(mean, bias, sd, var, rmse), mean)
}

# plotting vs parameter (files on output path)
{
variable_plotting <- 'rmse' # mean, bias, sd, var, rmse
label_plotting <- 'RMSE'
leg_position <- c(0, 1.1)
leg_justification <- c(0, 1)

# p
plotEst(myData_p, var_x = 'p', var_y = variable_plotting, label_x = expression(rho),
        label_y = label_plotting, file_name =  paste0(variable_plotting,'_rho'), 
        x_lim = c(0, 1), y_lim = c(0, 0.5), 
        leg_position = leg_position, leg_justification = leg_justification, 
        file_ext = '.pdf', lbl_order = lbl_order)

# lq
plotEst(myData_lq, var_x = 'lq', var_y = variable_plotting, label_x = 'Lq',
        label_y = label_plotting, file_name = paste0(variable_plotting,'_lq'), 
        x_lim = c(0, 5), y_lim = c(0,5),
        leg_position = leg_position, leg_justification = leg_justification,
        file_ext = '.pdf', lbl_order = lbl_order)
# ls
plotEst(myData_ls, var_x = 'ls', var_y = variable_plotting, label_x = 'Ls',
        label_y = label_plotting, file_name = paste0(variable_plotting,'_ls'), 
        x_lim = c(0, 5), y_lim = c(0,5),
        leg_position = leg_position, leg_justification = leg_justification, 
        file_ext = '.pdf', lbl_order = lbl_order)
}

################################################################################
# plotting vs size
################################################################################

{
# grouping by size
myData_p <-
  df_plot_p %>%
  mutate_at(col_numeric, as.numeric) %>%
  mutate(bias = mean - p) %>%
  mutate(rmse = sqrt(bias^2 + sd^2)) %>%
  filter(size != 500) %>%
  group_by(name, size) %>%
  summarise_at(vars(mean, bias, sd, rmse), mean)

myData_lq <-
  df_plot_lq %>%
  mutate_at(col_numeric, as.numeric) %>%
  mutate(lq = round(p^2 / (1 - p), 1)) %>%
  mutate(bias = mean - lq) %>%
  mutate(rmse = sqrt(bias^2 + sd^2)) %>%
  filter(size != 500) %>%
  group_by(name, size) %>%
  summarise_at(vars(mean, bias, sd, rmse), mean)

myData_ls <-
  df_plot_ls %>%
  mutate_at(col_numeric, as.numeric) %>%
  mutate(ls = round(p / (1 - p), 1)) %>%
  mutate(bias = mean - ls) %>%
  mutate(rmse = sqrt(bias^2 + sd^2)) %>%
  filter(size != 500) %>%
  group_by(name, size) %>%
  summarise_at(vars(mean, bias, sd, rmse), mean)
}

# plotting vs size (files on output path)
{
variable_plotting <- 'rmse' # mean, bias, sd, var, rmse
label_plotting <- 'RMSE'
leg_position <- c(0, 1.1)
leg_justification <- c(0, 1)

# p
plotEst(myData_p, var_x = 'size', var_y = variable_plotting, label_x = 'n',
        label_y = label_plotting, file_name =  paste0(variable_plotting,'_rho_size'), 
        x_lim = c(0, 200), y_lim = c(0, 0.5), 
        leg_position = leg_position, leg_justification = leg_justification, 
        file_ext = '.pdf', lbl_order = lbl_order)
# lq
plotEst(myData_ls, var_x = 'size', var_y = variable_plotting, label_x = 'n',
        label_y = label_plotting, file_name = paste0(variable_plotting,'_lq_size'), 
        x_lim = c(0, 200), y_lim = c(0, 5),
        leg_position = leg_position, leg_justification = leg_justification, 
        file_ext = '.pdf', lbl_order = lbl_order)
# ls
plotEst(myData_ls, var_x = 'size', var_y = variable_plotting, label_x = 'n',
        label_y = label_plotting, file_name = paste0(variable_plotting,'_ls_size'), 
        x_lim = c(0, 200), y_lim = c(0, 5), #xlim = c(0, max(n))
        leg_position = leg_position, leg_justification = leg_justification,
        file_ext = '.pdf', lbl_order = lbl_order)
}

################################################################################
# plotting prior distributions
################################################################################

# some customization for prior plotting
{
  #cpManual<-c('black','blue','red2','green4','orange3','purple2','navyblue','cyan3',
  #            'salmon4','gold','violet','limegreen','springgreen','slateblue4')
  cpManual<-c('black', 'blue','orange2','green4','blue', 'red2')
  ltManual<-c('solid','twodash','dashed','dotted','solid')
  shapeManual<-c(15,16,17,18,4,8)

  # basic plot
  ggBase <- ggplot() + theme_minimal() + labs(color='',linetype='',shape='') +
    theme(
      panel.background = element_rect(fill='white',color='black'),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      #panel.grid.major = element_line(linewidth = 0.5,linetype='solid',color='gray90'),
      #panel.grid.minor = element_line(linewidth = 0.5, linetype='solid',color='gray90'),
      axis.ticks = element_line(),
      axis.ticks.length = unit(c(-0,1),'mm'),
      #legend.box.spacing = unit(0,'pt'),
      legend.margin = margin(t=1,r=1,b=1,l=1,unit='mm')
    ) +
    scale_linetype_manual(values=ltManual) +
    scale_color_manual(values=cpManual) +
    scale_shape_manual(values=shapeManual)
}

PlotDensityRho <- function(tabPlot, namePlot,
                           legend_pos = c(0, 0), legend_justif = c(0, 0),
                           nrowLegend = 2, y_lim = c(0, 0)){
  myPlot <- ggBase +
    xlab(expression(rho)) +
    ylab('density') +
    expand_limits(x = c(0, 1)) +
    #scale_x_continuous(breaks=seq(0, 1.1,0.2)) +
    #scale_y_continuous(breaks=seq(-1,1,0.2)) +
    ylim(ifelse(y_lim == c(0, 0), c(min(tabPlot$value), max(tabPlot$value)), y_lim)) +
    geom_line(data = tabPlot,
              aes(x = p, y = value,
                  color = variable, linetype = variable), lwd = 0.3) +
    geom_point(data = filter(tabPlot, row_number() %% 5 == 1), # gap between points
               aes(x = p, y = value,
                   color = variable, shape = variable), size = 1.5) +
    #geom_vline(xintercept = 0.95, lwd = 0.2) +
    theme(legend.position = legend_pos,
          legend.justification = legend_justif,
          legend.text = element_text(size = 10),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    guides(linetype = guide_legend(nrow = nrowLegend))


  #myPlot
  ggsave(
    file = paste0(namePlot),
    plot = myPlot,
    width = 6,
    height = 4,
    device = cairo_pdf
  )
}


# priors
beta_pri <- function(p, a, b){
  return(p^(a-1) * (1-p)^(b-1) / beta(a, b))
}

# plotting priors (files on output path)
{
p <- seq(1e-3, 1, 1e-2)
a <- c(1, 1.5, 2, 2.5)
b <- c(1, 2.5, 2, 1.5)

bp1 <- beta_pri(p, a[1], b[1])
bp2 <- beta_pri(p, a[2], b[2])
bp3 <- beta_pri(p, a[3], b[3])
bp4 <- beta_pri(p, a[4], b[4])

bp <- cbind(p, bp1, bp2, bp3, bp4)


names_bp <- c(paste0('B(', a, ', ', b,')'))
colnames(bp) <- c('p', names_bp)

bp <- as.data.frame(bp)
bp <- melt(bp, id.vars = 'p')

PlotDensityRho(bp, 'beta_prior.pdf', y_lim = c(0, 2.5),
               legend_pos = c(0, 1.05), legend_justif = c(0, 1), nrowLegend = 1)
}

################################################################################
# ploting autocorrelation function for queue-size samples
################################################################################

#  simulates a random g-g-c queue
rggc <- function(size_max, nserv, farr, fdep, narr_sim){
  # todo: implement finite queues
  
  # initialize variables
  arr <- cumsum(sapply(1:narr_sim, farr))
  dep <- sapply(1:narr_sim, fdep)
  serv_free <- rep(0, nserv)
  arr_start_serv <- rep(0, narr_sim)
  arr_end_serv <- rep(0, narr_sim)
  
  # simulate end of service times
  for(i in 1:narr_sim){
    k_next <- which.min(serv_free)
    arr_start_serv[i] <- max(arr[i], serv_free[k_next])
    arr_end_serv[i] <- arr_start_serv[i] + dep[i]
    serv_free[k_next] <- arr_end_serv[i]
  }
  
  # update queue
  queue_tab <- data.frame(
    time = c(arr, arr_end_serv),
    type = c(rep('a', length(arr)), rep('d', length(arr_end_serv)))
  )
  queue_tab <- queue_tab[order(queue_tab$time), ]
  queue_tab$size <- cumsum(ifelse(queue_tab$type == 'a', 1, -1))
  
  return(queue_tab)
}

nserv <- 1
rhos <- c(0.5, 0.8, 0.9)
alpha <- 0.10

acf_list <- lapply(rhos, function(rho){
  
  sim_queue <- rggc(
    size_max = 1000,
    nserv = nserv,
    farr = function(x) rexp(n=1, rate=1),
    fdep = function(x) rexp(n=1, rate=1/(nserv*rho)),
    narr_sim = 20e3
  )
  
  queue_size_series <- sim_queue$size  
  acf_obj <- acf(queue_size_series, lag.max=100, plot=FALSE)
  
  data.frame(
    lag = acf_obj$lag[,1,1],
    acf = acf_obj$acf[,1,1],
    rho = rho,
    limit = qnorm(1 - alpha/2) / sqrt(length(queue_size_series))
  )
})

df_all <- do.call(rbind, acf_list)

# plotting layout
acfPlot <- ggplot(df_all, aes(x=lag, y=acf)) +
  geom_segment(aes(xend=lag, yend=0), linewidth=0.6, color="black") +
  geom_hline(yintercept=0, linewidth=0.4) +
  geom_hline(aes(yintercept=limit), 
             linetype="dashed", linewidth=0.5, color="red") +
  geom_hline(aes(yintercept=-limit), 
             linetype="dashed", linewidth=0.5, color="red") +
  facet_wrap(~ rho, ncol=1) +
  geom_text(
    data = unique(df_all[c("rho")]),
    aes(x = Inf, y = Inf, 
        label = paste("rho==", rho)),
    parse = TRUE,
    hjust = 1.2, vjust = 1.5,
    inherit.aes = FALSE,
    size = 5
  ) +
  labs(
    x = "Lag",
    y = "Autocorrelation"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill="white", color="black"),
    panel.grid = element_blank(),
    axis.ticks = element_line(),
    axis.ticks.length = unit(c(0,1),"mm"),
    strip.text = element_blank(),
    strip.background = element_blank()
)

# plotting autocorrelation function (files on output path)
ggsave(
  file = paste0('fig_acf_queue.pdf'),
  plot = acfPlot,
  width = 6,
  height = 6,
  device = cairo_pdf
)

