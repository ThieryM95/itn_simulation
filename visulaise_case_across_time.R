############################################################################
# Illustration of  the uncomplicated malaria case across time across the   #
# different ITN implementation strategy                                    #
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

# Define label ####
e.labs <- ~ paste0("EIR= ", as.numeric(.x))
c.labs <- ~ paste0("Coverage = ", as.numeric(.x) * 100, "%")
A.labs <- ~ paste0("Access = ", as.numeric(.x) * 100, "%")

eht.labs <- c("Parameterisation 1", "Parameterisation 2A", "Parameterisation 2B")
names(eht.labs) <- c("Martin", "Nguessan", "Nguessan_2")


# Define color scheme (colour-blind friendly) ####
okabe_ito_palette <- c(
  "CFP-PYR-ITN with a reduction\nof deployment frequency of 25 %" = "#1B9E77",  # dark green
  "CFP-PYR-ITN with a reduction\nof deployment frequency of 50 %" = "#A1D99B",  # light green
  
  "CFP-PYR-ITN with a reduction\nof coverage of 25 %" = "#5E3C99",              # dark purple
  "CFP-PYR-ITN with a reduction\nof coverage of 50 %" = "#B3B3D9",              # light purple
  
  "CFP-PYR-ITN" = "black",
  "Standard - PYR-ITN" = "grey50"
)

# Color for plot b
okabe_ito_palette_2 <- c(
  "IG2_ITN" = "#0072B2",
  "PYR_ITN" = "#D55E00",
  "IG2_Placebo" = "#A6D7F2",
  "PYR_Placebo" = "#F4C27A",
  "IG2_Other" = "#7F8FAF",
  "PYR_Other" = "#D19A6B"
  
)

#################
# Visualisation #
#################

# Figure 1A
############

# Load the data
data_seed_1<-read.table("/scicore/home/penny/masthi00/OUT_itn_simulation/output_PYR_12years_EPI/Output_postprocessed.txt", sep = ";", header = T)
data_seed_2<-read.table("/scicore/home/penny/masthi00/OUT_itn_simulation/output_IG2_12years_frequency_EPI/Output_postprocessed.txt", sep = ";", header = T)
data_seed_3<-read.table("/scicore/home/penny/masthi00/OUT_itn_simulation/output_IG2_12years_coverage_EPI/Output_postprocessed.txt", sep = ";", header = T)

# merge the data
data_seed<-rbind(data_seed_1,data_seed_2,data_seed_3)

# named the different strategy
data_seed$strategy<-"Standard - PYR-ITN"
data_seed$strategy[data_seed$deployment_frequency==4] <- "CFP-PYR-ITN with a reduction\nof deployment frequency of 25 %"
data_seed$strategy[data_seed$deployment_frequency==6] <- "CFP-PYR-ITN with a reduction\nof deployment frequency of 50 %"
data_seed$strategy[data_seed$reduction_coverage==0.25] <-"CFP-PYR-ITN with a reduction\nof coverage of 25 %"
data_seed$strategy[data_seed$reduction_coverage==0.5] <- "CFP-PYR-ITN with a reduction\nof coverage of 50 %"
data_seed$strategy[data_seed$reduction_coverage==0 & data_seed$deployment_frequency==3 & data_seed$net=="IG2"] <- "CFP-PYR-ITN"

# named the different strategy
data_seed$strategy_2<-"Standard - PYR-ITN"
data_seed$strategy_2[data_seed$deployment_frequency==4] <- "CFP-PYR-ITN with a reduction\nof deployment frequency"
data_seed$strategy_2[data_seed$deployment_frequency==6] <- "CFP-PYR-ITN with a reduction\nof deployment frequency"
data_seed$strategy_2[data_seed$reduction_coverage==0.25] <-"CFP-PYR-ITN with a reduction\nof coverage"
data_seed$strategy_2[data_seed$reduction_coverage==0.5] <- "CFP-PYR-ITN with a reduction\nof coverage"
data_seed$strategy_2[data_seed$reduction_coverage==0 & data_seed$deployment_frequency==3 & data_seed$net=="IG2"] <- "CFP-PYR-ITN"

# select the data
scenario_2 <- data_seed %>%
  filter(seasonality %in% c("Perennial"),
         human_blood_index %in% c("High"),
         coverage %in% c(0.8),
         access %in% c(0.2),
         eir %in% c(10), 
         EHT== "Martin") 


# Make an moving average (plot will be smoother)
smoothed <- scenario_2 %>%
  mutate(survey_group = ceiling(survey / 6)) %>%
  group_by(survey_group, ageGroup, measure, net, access, eir, coverage, EHT, deployment_frequency, reduction_coverage, strategy, strategy_2) %>%
  summarise(normalised_value_avg = mean(normalised_value), .groups = "drop")

# make variable as factors
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

smoothed$strategy_2<-as.factor(smoothed$strategy_2)
smoothed$strategy_2 <- factor(
  smoothed$strategy_2,
  levels = c(
    "Standard - PYR-ITN",
    "CFP-PYR-ITN",
    "CFP-PYR-ITN with a reduction\nof coverage",
    "CFP-PYR-ITN with a reduction\nof deployment frequency")
)


# plot
fig10 <- ggplot(smoothed %>% filter(measure == "nUncomp")) +
  # geom_line(aes(x = survey/12, y = normalised_value_M, color = group), size = 1.2) +
  geom_line(aes(x = survey_group/12, y = normalised_value_avg*365/5, color= strategy), size = 1.2) +
  # geom_ribbon(aes(x = survey/12, ymin = normalised_value_L, ymax = normalised_value_U,
  #                 fill = group),
  #             alpha = 0.25) +
  facet_nested(. ~  strategy_2,
               labeller = labeller(eir = e.labs, EHT = eht.labs, access = A.labs)) +
  my_theme() +
  scale_color_manual(values = okabe_ito_palette) +
  scale_fill_manual(values = okabe_ito_palette) +
  xlab("Time (year)") +
  ylab("Clinical cases (per person per year)") +
  ylim(0,2) +
  xlim(0,13) +
  guides(color = guide_legend(title = "Strategy:", nrow=2),
         fill  = guide_legend(title = "Strategy:")) +
  theme(legend.position = "bottom")

fig10


# figure 1B
###########

experiment <- "output_Final_All_2"
outdir_experiment = paste0("/scicore/home/penny/masthi00/OUT_itn_simulation/", experiment)
summary_file <-  "/Output_postprocessed.txt"
summary_path <- paste0(outdir_experiment, summary_file)
data_seed_0 <-read.table(summary_path, sep = ";", header = T)


# Sum across age group
data_seed_allage <- data_seed_0 %>% 
  group_by(survey, measure, cohort, net, access, eir, coverage, EHT, seasonality, human_blood_index) %>%
  summarise(value = sum(value), 
            value_nHost = sum(value_nHost))%>% 
  mutate(normalised_value = value / value_nHost) %>%
  ungroup()

# Check
sum(data_seed$value[
  data_seed$survey  == 200 & 
    data_seed$measure == "nUncomp" &
    data_seed$coverage == 0.8 &
    data_seed$access == 0.04 &
    data_seed$eir == 10 &
    data_seed$cohort == "Other"&
    data_seed$net == "PYR" ])


data_seed_allage$value[
  data_seed_allage$survey  == 200 & 
    data_seed_allage$measure == "nUncomp" &
    data_seed_allage$coverage == 0.8 &
    data_seed_allage$access == 0.04 &
    data_seed_allage$eir == 10 &
    data_seed_allage$cohort == "Other"&
    data_seed_allage$net == "PYR"]


# select the sceario
scenario_2 <- data_seed_allage %>%
  filter(coverage %in% c(0.8),
         access %in% c(0.2),
         eir %in% c(10),
         seasonality %in% c("Perennial"),
         human_blood_index %in% c("High")) %>%
  mutate(group = paste0(net, "_", cohort))


# select the time during which ITN is deployed
time_int <-unique(data_seed_allage[data_seed_allage$cohort=="ITN" & data_seed_allage$measure == "nHost" & data_seed_allage$value >= 1,]$survey) 
time_start <- time_int[1]-1 # it should be survey 75
time_end <- time_int[length(time_int)]+1 # it should be 294 (14 + 12*3)

scenario_2$normalised_value[scenario_2$cohort == "Other" & 
                              scenario_2$survey >= time_start & 
                              scenario_2$survey <= time_end] <-- NA

# make variable as factor
scenario_2$group<-as.factor(scenario_2$group)
scenario_2$group <- factor(
  scenario_2$group,
  levels = c("IG2_All", "IG2_ITN", "IG2_Placebo", "IG2_Other","PYR_All", "PYR_ITN", "PYR_Placebo", "PYR_Other"))

# estimate a moving average to makke it more smooth
smoothed <- scenario_2 %>%
  mutate(survey_group = ceiling(survey / 3)) %>%
  group_by(survey_group, cohort, measure, net, access, eir, coverage, group, EHT) %>%
  summarise(normalised_value_avg = mean(normalised_value), .groups = "drop")



fig9 <- ggplot(smoothed %>% filter(measure == "nUncomp" & EHT == "Martin")) +
   geom_line(aes(x = survey_group/24, y = normalised_value_avg*365/5, color = group), size = 1.2) +
  my_theme() +
  ylim(0,2) +
  xlim(0,4) +
  scale_color_manual(values = okabe_ito_palette_2,
                     labels = c("IG2_ITN" = "CFP-PYR-ITN users",
                                "IG2_Placebo" = "CFP-PYR-ITN non-users",
                                "PYR_ITN" = "PYR-ITN users",
                                "PYR_Placebo" = "PYR-ITN non-users",
                                "IG2_Other" = "CFP-PYR-ITN All (pre-ITN deployment)     ",
                                "PYR_Other" = "PYR-ITN All (pre-ITN deployment)     ")) +
  xlab("Time (year)") +
  ylab("Clinical case (per person per year)") +
  guides(color = guide_legend(title = "Arm:")) +
  theme(legend.position = "right")

fig9

# Merge both plot in one figure
###############################

# Merge
PLOT1 <- plot_grid(fig10, fig9, ncol = 1, nrow = 2, scale = c(1,0.9), rel_heights=c(1,0.8) , labels = c("A", "B"), label_size = 18 / constant, label_fontface = 2)

# Save
outdir_experiment = paste0("/scicore/home/penny/masthi00/OUT_itn_simulation/", "Visualise_results/")
summary_file <- "Figure_2.pdf"
summary_path <- paste0(outdir_experiment, summary_file)

ggsave(summary_path, plot = PLOT1, width = 24, height = 20, device = "pdf", units = "cm",  dpi = 300)




