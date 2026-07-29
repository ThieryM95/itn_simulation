#####################################################################
# Visualise ITN parameters (intital value and decay) values         #
#                                                                   #
#                                                                   #
# Author: thiery.masserey@swisstph.ch                               #
#####################################################################


##################
# House keeping  #
##################

# Clear global environment
rm(list = ls())

# Define working directory
setwd("/scicore/home/penny/masthi00/itn_simulation")

# Source package and functions
source("pacman.R")
source("run.R")
source("extract.R")


# Set plot theme 
#################

# Constant
constant = 2

# Theme
plot_theme = theme(
  axis.text = element_text(
    face = "bold",
    size = 20 / constant,
    angle = 0
  ),
  axis.title = element_text(
    face = "bold",
    size = 20 / constant,
    angle = 0
  ),
  strip.background = element_rect(
    color = "black",
    fill = "aliceblue",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    face = "bold",
    size = 15 / constant,
    angle = 0
  ),
  strip.text.y = element_text(
    face = "bold",
    size = 15 / constant,
    angle = 90   # rotate to vertical
  ),
  legend.title = element_text(size = 20 / constant, face = "bold"),
  legend.text = element_text(size = 20 / constant),
  legend.key = element_blank(),
  legend.key.size = unit(0.5, 'cm'),
  legend.spacing.y = unit(0.5, 'cm'),
  legend.spacing.x = unit(0.5, 'cm'),
  legend.position = "bottom",
  panel.spacing.x = unit(0.2, "lines"),
  panel.spacing.y = unit(0.4, "lines"),
)+
  theme(
    strip.text.x = element_text(size = 20 / constant, face = "bold"),      # horizontal strips
    strip.text.y = element_text(size = 20 / constant, face = "bold"),
    strip.text.x.top = element_text(size = 20 / constant, face = "bold"),  # top-level facet
    strip.text.x.bottom = element_text(size = 20 / constant, face = "bold")
  )

my_theme <- function() {
  theme_pubclean() +
    plot_theme
}


##############################
# Generate the data to plot  #
##############################


# Weiblu function
weibul<-function(t,L, K){
  
  results<-exp(-(t/L)^K * log(2) )
  
  return(results)
}


# Parametrisation 1
###################

TIME<-seq(0,6,by=0.01)

# Deterrency PYR-ITN
parameterisation <- "Parameterisation 1"
ITN <- "PYR-ITN"
parameter <- "Deterrency"
initial <- 0.37  # Values come from parameters.R (scaffolds folder)
half_life <-2.3  # Values come from parameters.R (scaffolds folder)
slope <-1.6      # Values come from parameters.R (scaffolds folder)
TIME
Data <- weibul(t=TIME, L=half_life, K=slope)*initial
Data_1.1 <- data.frame(parameterisation, ITN, parameter, initial, half_life, slope, TIME, Data)

# Deterrency CFP-PYR-ITN
parameterisation <- "Parameterisation 1"
ITN <- "CFP-PYR-ITN"
parameter <- "Deterrency"
initial <- 0.19 # Values come from parameters.R (scaffolds folder)
half_life <-2.4 # Values come from parameters.R (scaffolds folder)
slope <-2.4     # Values come from parameters.R (scaffolds folder)
TIME
Data <- weibul(t=TIME, L=half_life, K=slope)*initial
Data_1.2 <- data.frame(parameterisation, ITN, parameter, initial, half_life, slope, TIME, Data)

# Pre-prandial killing Effect PYR-ITN
parameterisation <- "Parameterisation 1"
ITN <- "PYR-ITN"
parameter <- "Pre-prandial killing Effect"
initial <- 0.08 # Values come from parameters.R (scaffolds folder)
half_life <-2.3 # Values come from parameters.R (scaffolds folder)
slope <-1.6     # Values come from parameters.R (scaffolds folder)
TIME
Data <- weibul(t=TIME, L=half_life, K=slope)*initial
Data_1.3 <- data.frame(parameterisation, ITN, parameter, initial, half_life, slope, TIME, Data)

# Pre-prandial killing Effect CFP-PYR-ITN
parameterisation <- "Parameterisation 1"
ITN <- "CFP-PYR-ITN"
parameter <- "Pre-prandial killing Effect"
initial <- 0.41  # Values come from parameters.R (scaffolds folder)
half_life <-1.3  # Values come from parameters.R (scaffolds folder)
slope <-1.8      # Values come from parameters.R (scaffolds folder)
TIME 
Data <- weibul(t=TIME, L=half_life, K=slope)*initial
Data_1.4 <- data.frame(parameterisation, ITN, parameter, initial, half_life, slope, TIME, Data)

# Post-prandial killing Effect PYR-ITN
parameterisation <- "Parameterisation 1"
ITN <- "PYR-ITN"
parameter <- "Post-prandial killing Effect"
initial <- 0.05  # Values come from parameters.R (scaffolds folder)
half_life <-2.3  # Values come from parameters.R (scaffolds folder)
slope <-1.6      # Values come from parameters.R (scaffolds folder)
TIME
Data <- weibul(t=TIME, L=half_life, K=slope)*initial
Data_1.5 <- data.frame(parameterisation, ITN, parameter, initial, half_life, slope, TIME, Data)


# Post-prandial killing Effect CFP-PYR-ITN
parameterisation <- "Parameterisation 1"
ITN <- "CFP-PYR-ITN"
parameter <- "Post-prandial killing Effect"
initial <- 0.42   # Values come from parameters.R (scaffolds folder)
half_life <-1.8   # Values come from parameters.R (scaffolds folder)
slope <-1.9       # Values come from parameters.R (scaffolds folder)
TIME
Data <- weibul(t=TIME, L=half_life, K=slope)*initial
Data_1.6 <- data.frame(parameterisation, ITN, parameter, initial, half_life, slope, TIME, Data)

# Parameterisation 2A
######################

# Deterrency PYR-ITN
parameterisation <- "Parameterisation 2A"
ITN <- "PYR-ITN"
parameter <- "Deterrency"
initial <- 0.67   # Values come from parameters.R (scaffolds folder)
half_life <-2.3   # Values come from parameters.R (scaffolds folder)
slope <-0.9       # Values come from parameters.R (scaffolds folder)
TIME
Data <- weibul(t=TIME, L=half_life, K=slope)*initial
Data_2A.1 <- data.frame(parameterisation, ITN, parameter, initial, half_life, slope, TIME, Data)

# Deterrency CFP-PYR-ITN
parameterisation <- "Parameterisation 2A"
ITN <- "CFP-PYR-ITN"
parameter <- "Deterrency"
initial <- 0.5    # Values come from parameters.R (scaffolds folder)
half_life <-2.5   # Values come from parameters.R (scaffolds folder)
slope <-1.0       # Values come from parameters.R (scaffolds folder)
TIME 
Data <- weibul(t=TIME, L=half_life, K=slope)*initial
Data_2A.2 <- data.frame(parameterisation, ITN, parameter, initial, half_life, slope, TIME, Data)

# Pre-prandial killing Effect PYR-ITN
parameterisation <- "Parameterisation 2A"
ITN <- "PYR-ITN"
parameter <- "Pre-prandial killing Effect"
initial <- 0.17   # Values come from parameters.R (scaffolds folder)
half_life <-1.6   # Values come from parameters.R (scaffolds folder)
slope <-1.2       # Values come from parameters.R (scaffolds folder)
TIME
Data <- weibul(t=TIME, L=half_life, K=slope)*initial
Data_2A.3 <- data.frame(parameterisation, ITN, parameter, initial, half_life, slope, TIME, Data)

# Pre-prandial killing Effect CFP-PYR-ITN
parameterisation <- "Parameterisation 2A"
ITN <- "CFP-PYR-ITN"
parameter <- "Pre-prandial killing Effect"
initial <- 0.54   # Values come from parameters.R (scaffolds folder)
half_life <-2.5   # Values come from parameters.R (scaffolds folder)
slope <-1         # Values come from parameters.R (scaffolds folder)
TIME
Data <- weibul(t=TIME, L=half_life, K=slope)*initial
Data_2A.4 <- data.frame(parameterisation, ITN, parameter, initial, half_life, slope, TIME, Data)

# Post-prandial killing Effect PYR-ITN
parameterisation <- "Parameterisation 2A"
ITN <- "PYR-ITN"
parameter <- "Post-prandial killing Effect"
initial <- 0.06 # Values come from parameters.R (scaffolds folder)
half_life <-3.0 # Values come from parameters.R (scaffolds folder)
slope <-0.7     # Values come from parameters.R (scaffolds folder)
TIME
Data <- weibul(t=TIME, L=half_life, K=slope)*initial
Data_2A.5 <- data.frame(parameterisation, ITN, parameter, initial, half_life, slope, TIME, Data)

# Post-prandial killing Effect CFP-PYR-ITN
parameterisation <- "Parameterisation 2A"
ITN <- "CFP-PYR-ITN"
parameter <- "Post-prandial killing Effect"
initial <- 0.47 # Values come from parameters.R (scaffolds folder)
half_life <-3.0 # Values come from parameters.R (scaffolds folder)
slope <-0.93    # Values come from parameters.R (scaffolds folder)
TIME 
Data <- weibul(t=TIME, L=half_life, K=slope)*initial
Data_2A.6 <- data.frame(parameterisation, ITN, parameter, initial, half_life, slope, TIME, Data)


# Parameterisation 2B
######################

# Deterrency PYR-ITN
parameterisation <- "Parameterisation 2B"
ITN <- "PYR-ITN"
parameter <- "Deterrency"
initial <- 0.67  # Values come from parameters.R (scaffolds folder)
half_life <-2.2  # Values come from parameters.R (scaffolds folder)
slope <-2.8      # Values come from parameters.R (scaffolds folder)
TIME 
Data <- weibul(t=TIME, L=half_life, K=slope)*initial
Data_2B.1 <- data.frame(parameterisation, ITN, parameter, initial, half_life, slope, TIME, Data)

# Deterrency CFP-PYR-ITN
parameterisation <- "Parameterisation 2B"
ITN <- "CFP-PYR-ITN"
parameter <- "Deterrency"
initial <- 0.5   # Values come from parameters.R (scaffolds folder)
half_life <-2.0  # Values come from parameters.R (scaffolds folder)
slope <-1.9      # Values come from parameters.R (scaffolds folder)
TIME
Data <- weibul(t=TIME, L=half_life, K=slope)*initial
Data_2B.2 <- data.frame(parameterisation, ITN, parameter, initial, half_life, slope, TIME, Data)

# Pre-prandial killing Effect PYR-ITN
parameterisation <- "Parameterisation 2B"
ITN <- "PYR-ITN"
parameter <- "Pre-prandial killing Effect"
initial <- 0.17  # Values come from parameters.R (scaffolds folder)
half_life <-1.9  # Values come from parameters.R (scaffolds folder)
slope <-2.2      # Values come from parameters.R (scaffolds folder)
TIME
Data <- weibul(t=TIME, L=half_life, K=slope)*initial
Data_2B.3 <- data.frame(parameterisation, ITN, parameter, initial, half_life, slope, TIME, Data)

# Pre-prandial killing Effect CFP-PYR-ITN
parameterisation <- "Parameterisation 2B"
ITN <- "CFP-PYR-ITN"
parameter <- "Pre-prandial killing Effect"
initial <- 0.54  # Values come from parameters.R (scaffolds folder)
half_life <- 1.9 # Values come from parameters.R (scaffolds folder)
slope <- 1.9     # Values come from parameters.R (scaffolds folder)
TIME
Data <- weibul(t=TIME, L=half_life, K=slope)*initial
Data_2B.4 <- data.frame(parameterisation, ITN, parameter, initial, half_life, slope, TIME, Data)

# Post-prandial killing Effect PYR-ITN
parameterisation <- "Parameterisation 2B"
ITN <- "PYR-ITN"
parameter <- "Post-prandial killing Effect"
initial <- 0.06  # Values come from parameters.R (scaffolds folder)
half_life <-2.4  # Values come from parameters.R (scaffolds folder)
slope <-3.3      # Values come from parameters.R (scaffolds folder)
TIME
Data <- weibul(t=TIME, L=half_life, K=slope)*initial
Data_2B.5 <- data.frame(parameterisation, ITN, parameter, initial, half_life, slope, TIME, Data)

# Post-prandial killing Effect CFP-PYR-ITN
parameterisation <- "Parameterisation 2B"
ITN <- "CFP-PYR-ITN"
parameter <- "Post-prandial killing Effect"
initial <- 0.47  # Values come from parameters.R (scaffolds folder)
half_life <-2.1  # Values come from parameters.R (scaffolds folder)
slope <-2.05     # Values come from parameters.R (scaffolds folder)
TIME
Data <- weibul(t=TIME, L=half_life, K=slope)*initial
Data_2B.6 <- data.frame(parameterisation, ITN, parameter, initial, half_life, slope, TIME, Data)

# Merge all the data
####################

Data_all <-rbind(Data_1.1,
                 Data_1.2,
                 Data_1.3,
                 Data_1.4,
                 Data_1.5,
                 Data_1.6,
                 Data_2A.1,
                 Data_2A.2,
                 Data_2A.3,
                 Data_2A.4,
                 Data_2A.5,
                 Data_2A.6,
                 Data_2B.1,
                 Data_2B.2,
                 Data_2B.3,
                 Data_2B.4,
                 Data_2B.5,
                 Data_2B.6)


############
# Figure 1 #
############

# Make factor
Data_all$parameter<-as.factor(Data_all$parameter)
Data_all$parameter <- factor(
  Data_all$parameter,
  levels = c("Deterrency", "Pre-prandial killing Effect", "Post-prandial killing Effect"))

# plot
PLOT2<-ggplot(Data_all,
       aes(x = TIME, y = Data, color=ITN)) +
  geom_line(size=2) +
  my_theme() +
  facet_nested(parameter~parameterisation)+
  xlab("Time (years)") +
  ylab("Effect") +
  scale_color_manual(values = c("CFP-PYR-ITN" = "#0072B2",
                               "PYR-ITN" = "#D55E00"))+
  guides(color = guide_legend(title = "ITN:"))+
  geom_vline(xintercept = c(3), color = "grey", linetype = "solid")+
  geom_vline(xintercept = c(4), color = "grey", linetype = "longdash") +
  geom_vline(xintercept = c(6), color = "grey", linetype = "dashed") +
  geom_line(size=2) 

outdir_experiment = paste0("/scicore/home/penny/masthi00/OUT_itn_simulation/",
                           "Visualise_results/")

# Save
summary_file <- "Figure_1.pdf"
summary_path <- paste0(outdir_experiment, summary_file)

ggsave(
  summary_path,
  plot = PLOT2,
  width = 18,
  height = 18,
  device = "pdf",
  units = "cm",
  dpi = 300
)

