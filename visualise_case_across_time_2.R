############################################################################
# Incidence of clinical malaria across ITN deployment strategy             #
# across different ITN parameterisation and transmission setting           #
#                                                                          #
# Author: thiery.masserey@swisstph.ch and swapnoleena.sen@swisstph.ch      #
############################################################################
  

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
library(cowplot)

# Set plot theme 
################

# constant
constant = 2

# theme
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

# Define label 
e.labs <- ~ paste0("EIR= ", as.numeric(.x))
c.labs <- ~ paste0("Coverage = ", as.numeric(.x) * 100, "%")
A.labs <- ~ paste0("Access = ", as.numeric(.x) * 100, "%")

eht.labs <- c("Parameterisation 1", "Parameterisation 2A", "Parameterisation 2B")
names(eht.labs) <- c("Martin", "Nguessan", "Nguessan_2")

# Define colour scheme (colour-blind friendly) ####
okabe_ito_palette <- c(
  "CFP-PYR-ITN with a reduction\nof deployment frequency of 25 %" = "#1B9E77",  # dark green
  "CFP-PYR-ITN with a reduction\nof deployment frequency of 50 %" = "#A1D99B",  # light green
  
  "CFP-PYR-ITN with a reduction\nof coverage of 25 %" = "#5E3C99",              # dark purple
  "CFP-PYR-ITN with a reduction\nof coverage of 50 %" = "#B3B3D9",              # light purple
  
  "CFP-PYR-ITN" = "black",
  "Standard - PYR-ITN" = "grey50"
)

#################
# Visualisation #
#################


# Load the data
data_seed_1<-read.table("/scicore/home/penny/masthi00/OUT_itn_simulation/output_PYR_12years_EPI/Output_postprocessed.txt", sep = ";", header = T)
data_seed_2<-read.table("/scicore/home/penny/masthi00/OUT_itn_simulation/output_IG2_12years_frequency_EPI/Output_postprocessed.txt", sep = ";", header = T)
data_seed_3<-read.table("/scicore/home/penny/masthi00/OUT_itn_simulation/output_IG2_12years_coverage_EPI/Output_postprocessed.txt", sep = ";", header = T)

# Merge
data_seed<-rbind(data_seed_1,data_seed_2,data_seed_3)

# Label the different strategy
data_seed$strategy<-"Standard - PYR-ITN"
data_seed$strategy[data_seed$deployment_frequency==4] <- "CFP-PYR-ITN with a reduction\nof deployment frequency of 25 %"
data_seed$strategy[data_seed$deployment_frequency==6] <- "CFP-PYR-ITN with a reduction\nof deployment frequency of 50 %"
data_seed$strategy[data_seed$reduction_coverage==0.25] <-"CFP-PYR-ITN with a reduction\nof coverage of 25 %"
data_seed$strategy[data_seed$reduction_coverage==0.5] <- "CFP-PYR-ITN with a reduction\nof coverage of 50 %"
data_seed$strategy[data_seed$reduction_coverage==0 & data_seed$deployment_frequency==3 & data_seed$net=="IG2"] <- "CFP-PYR-ITN"

data_seed$strategy_2<-"Standard - PYR-ITN"
data_seed$strategy_2[data_seed$deployment_frequency==4] <- "CFP-PYR-ITN with a reduction\nof deployment frequency"
data_seed$strategy_2[data_seed$deployment_frequency==6] <- "CFP-PYR-ITN with a reduction\nof deployment frequency"
data_seed$strategy_2[data_seed$reduction_coverage==0.25] <-"CFP-PYR-ITN with a reduction\nof coverage"
data_seed$strategy_2[data_seed$reduction_coverage==0.5] <- "CFP-PYR-ITN with a reduction\nof coverage"
data_seed$strategy_2[data_seed$reduction_coverage==0 & data_seed$deployment_frequency==3 & data_seed$net=="IG2"] <- "CFP-PYR-ITN"

# Select the data
scenario_2 <- data_seed %>%
  filter(seasonality %in% c("Perennial"),
         human_blood_index %in% c("High"),
         coverage %in% c(0.8),
         access %in% c(0.2),
         eir %in% c(10,40,100)) 



# Estimate the moving average of case across time (make plot more smooth)
smoothed <- scenario_2 %>%
  mutate(survey_group = ceiling(survey / 6)) %>%
  group_by(survey_group, ageGroup, measure, net, access, eir, coverage, EHT, deployment_frequency, reduction_coverage, strategy, strategy_2) %>%
  summarise(normalised_value_avg = mean(normalised_value), .groups = "drop")

# make variable as factor
smoothed$strategy <- factor(
  smoothed$strategy,
  levels = c(
    "Standard - PYR-ITN",
    "CFP-PYR-ITN",
    "CFP-PYR-ITN with a reduction\nof coverage of 25 %",
    "CFP-PYR-ITN with a reduction\nof coverage of 50 %",
    "CFP-PYR-ITN with a reduction\nof deployment frequency of 25 %",
    "CFP-PYR-ITN with a reduction\nof deployment frequency of 50 %"
  )
)

smoothed$strategy_2 <- factor(
  smoothed$strategy_2,
  levels = c(
    "Standard - PYR-ITN",
    "CFP-PYR-ITN",
    "CFP-PYR-ITN with a reduction\nof coverage",
    "CFP-PYR-ITN with a reduction\nof deployment frequency"
  )
)

# plot Figure S2
fig10 <- ggplot(smoothed %>% filter(measure == "nUncomp")) +
  # geom_line(aes(x = survey/12, y = normalised_value_M, color = group), size = 1.2) +
  geom_line(aes(x = survey_group/12, y = normalised_value_avg*365/5, color=strategy), size = 1.2) +
  # geom_ribbon(aes(x = survey/12, ymin = normalised_value_L, ymax = normalised_value_U,
  #                 fill = group),
  #             alpha = 0.25) +
  facet_nested(eir + EHT ~  strategy_2,
               labeller = labeller(eir = e.labs, EHT = eht.labs, access = A.labs)) +
  my_theme() +
  scale_color_manual(values = okabe_ito_palette) +
  scale_fill_manual(values = okabe_ito_palette) +
  xlab("Time (year)") +
  ylab("Clinical cases (per person per year)") +
  guides(color = guide_legend(title = "Study arm"),
         fill  = guide_legend(title = "Study arm")) +
  theme(legend.position = "bottom")

fig10


# Save
outdir_experiment = paste0("/scicore/home/penny/masthi00/OUT_itn_simulation/", "Visualise_results/")
summary_file <- "Figure_S2.pdf"
summary_path <- paste0(outdir_experiment, summary_file)

ggsave(summary_path, plot = fig10, width = 32, height = 38, device = "pdf", units = "cm",  dpi = 300)








