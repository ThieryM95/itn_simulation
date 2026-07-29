############################################################################
# Visualise results of analysis 2: Cumulative case per coverage            #
#                                                                          #
# Quantify changes in malaria burden among individuals who would           #
# potentially lose access to ITNs under the reduced-coverage scenarios     #
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
  legend.key.size = unit(1, 'cm'),
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
# Label name
e.labs <- ~ paste0("EIR= ", as.numeric(.x))
c.labs <- ~ paste0("Coverage = ", as.numeric(.x) * 100, "%")
A.labs <- ~ paste0("Access = ", as.numeric(.x) * 100, "%")
H.labs <- ~ paste0("Human Blood Index = ", as.factor (.x))
S.labs <- ~ paste0("Transmission = ", as.factor(.x))

eht.labs <- c("Parameterisation 1",
              "Parameterisation 2A",
              "Parameterisation 2B")
names(eht.labs) <- c("Martin", "Nguessan", "Nguessan_2")


c.labs <-  c("Whole population CFP-PYR-ITN vs\n Whole population PYR-ITN", "Protected CFP-PYR-ITN vs\n Protected PYR-ITN", "Unprotected CFP-PYR-ITN vs\n Unprotected PYR-ITN","Unprotected CFP-PYR-ITN vs\n Protected PYR-ITN")
names(c.labs) <- c("All IG2 vs\n All PYR", "Protected IG2 vs\n Protected PYR", "Unprotected IG2 vs\n Unprotected PYR","Unprotected IG2 vs\n Protected PYR")


# Define colour scheme (colour-blind friendly) ####
okabe_ito_palette <- c(
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


# Load the data
###############

experiment<-"output_Final_All_2" # Name of the output folder
outdir_experiment = paste0("/scicore/home/penny/masthi00/OUT_itn_simulation/", experiment)
summary_file <- "/Output_summary.txt"
summary_path <- paste0(outdir_experiment, summary_file)
cumulative_data_seed_allage<-read.table(summary_path, sep = ";", header = T)



# Figure 5
###########


# Select the data
EIR<-c(10,40,100)
ACCESS<- 0.2

cumulative_data_seed_allage_select <- cumulative_data_seed_allage%>%
  filter(eir %in% EIR, access ==ACCESS , cohort %in% c("ITN", "Placebo"),  seasonality == "Perennial" , human_blood_index == "High", EHT %in% c("Nguessan_2"))%>%
  mutate(group = paste0(net, "_", cohort))


# Plot
Plot_3<-ggplot(cumulative_data_seed_allage_select,
       aes(x = coverage, y = cumulative_normalised_value, color=group)) +
  geom_point() +
  geom_line() +
  my_theme() +
  facet_nested(EHT~eir, labeller = labeller(eir = e.labs, EHT = eht.labs))+
  scale_color_manual(values = okabe_ito_palette,
                     labels = c("IG2_ITN" = "CFP-PYR-ITN users",
                                "IG2_Placebo" = "CFP-PYR-ITN non-users",
                                "PYR_ITN" = "PYR-ITN users",
                                "PYR_Placebo" = "PYR-ITN non-users")) +
  xlab("ITN Coverage (%)") +
  ylab("3-year cumulative malaria cases (per person)") +
  guides(color = guide_legend(title = "Arm:")) +
  theme(legend.position = "bottom")

# Save
outdir_experiment = paste0("/scicore/home/penny/masthi00/OUT_itn_simulation/", "Visualise_results/")
summary_file <- "Figure_4.pdf"
summary_path <- paste0(outdir_experiment, summary_file)

ggsave(summary_path, plot = Plot_3, width = 22, height = 10, device = "pdf", units = "cm",  dpi = 300)


# Figure S7
############

# Select the data
cumulative_data_seed_allage_select <- cumulative_data_seed_allage%>%
  filter(eir %in% EIR, access ==ACCESS , cohort %in% c("ITN", "Placebo"), EHT %in% c("Martin"))%>%
  mutate(group = paste0(net, "_", cohort))

Plot_3<-ggplot(cumulative_data_seed_allage_select,
               aes(x = coverage, y = cumulative_normalised_value, color=group)) +
  geom_point() +
  geom_line() +
  my_theme() +
  facet_nested(EHT+seasonality+human_blood_index~eir, labeller = labeller(eir = e.labs, EHT = eht.labs, seasonality=S.labs, human_blood_index=H.labs))+
  scale_color_manual(values = okabe_ito_palette,
                     labels = c("IG2_ITN" = "CFP-PYR-ITN users",
                                "IG2_Placebo" = "CFP-PYR-ITN non-users",
                                "PYR_ITN" = "PYR-ITN users",
                                "PYR_Placebo" = "PYR-ITN non-users")) +
  xlab("ITN Coverage (%)") +
  ylab("3-year cumulative malaria cases (per person)") +
  guides(color = guide_legend(title = "Arm:")) +
  theme(legend.position = "bottom")


# Save
outdir_experiment = paste0("/scicore/home/penny/masthi00/OUT_itn_simulation/", "Visualise_results/")
summary_file <- "Figure_S7.pdf"
summary_path <- paste0(outdir_experiment, summary_file)

ggsave(summary_path, plot = Plot_3, width = 22, height = 24, device = "pdf", units = "cm",  dpi = 300)


# Figure S8
############

# Select the data
cumulative_data_seed_allage_select <- cumulative_data_seed_allage%>%
  filter(eir %in% EIR, access ==ACCESS , cohort %in% c("ITN", "Placebo"), EHT %in% c("Nguessan"))%>%
  mutate(group = paste0(net, "_", cohort))

Plot_3<-ggplot(cumulative_data_seed_allage_select,
               aes(x = coverage, y = cumulative_normalised_value, color=group)) +
  geom_point() +
  geom_line() +
  my_theme() +
  facet_nested(EHT+seasonality+human_blood_index~eir, labeller = labeller(eir = e.labs, EHT = eht.labs, seasonality=S.labs, human_blood_index=H.labs))+
  scale_color_manual(values = okabe_ito_palette,
                     labels = c("IG2_ITN" = "CFP-PYR-ITN users",
                                "IG2_Placebo" = "CFP-PYR-ITN non-users",
                                "PYR_ITN" = "PYR-ITN users",
                                "PYR_Placebo" = "PYR-ITN non-users")) +
  xlab("ITN Coverage (%)") +
  ylab("3-year cumulative malaria cases (per person)") +
  guides(color = guide_legend(title = "Arm:")) +
  theme(legend.position = "bottom")


# Save
outdir_experiment = paste0("/scicore/home/penny/masthi00/OUT_itn_simulation/", "Visualise_results/")
summary_file <- "Figure_S8.pdf"
summary_path <- paste0(outdir_experiment, summary_file)

ggsave(summary_path, plot = Plot_3, width = 22, height = 24, device = "pdf", units = "cm",  dpi = 300)


# Figure S9
############

# Select the data
cumulative_data_seed_allage_select <- cumulative_data_seed_allage%>%
  filter(eir %in% EIR, access ==ACCESS , cohort %in% c("ITN", "Placebo"), EHT %in% c("Nguessan_2"))%>%
  mutate(group = paste0(net, "_", cohort))

Plot_3<-ggplot(cumulative_data_seed_allage_select,
               aes(x = coverage, y = cumulative_normalised_value, color=group)) +
  geom_point() +
  geom_line() +
  my_theme() +
  facet_nested(EHT+seasonality+human_blood_index~eir, labeller = labeller(eir = e.labs, EHT = eht.labs, seasonality=S.labs, human_blood_index=H.labs))+
  scale_color_manual(values = okabe_ito_palette,
                     labels = c("IG2_ITN" = "CFP-PYR-ITN users",
                                "IG2_Placebo" = "CFP-PYR-ITN non-users",
                                "PYR_ITN" = "PYR-ITN users",
                                "PYR_Placebo" = "PYR-ITN non-users")) +
  xlab("ITN Coverage (%)") +
  ylab("3-year cumulative malaria cases (per person)") +
  guides(color = guide_legend(title = "Arm:")) +
  theme(legend.position = "bottom")


# Save
outdir_experiment = paste0("/scicore/home/penny/masthi00/OUT_itn_simulation/", "Visualise_results/")
summary_file <- "Figure_S9.pdf"
summary_path <- paste0(outdir_experiment, summary_file)

ggsave(summary_path, plot = Plot_3, width = 22, height = 24, device = "pdf", units = "cm",  dpi = 300)

