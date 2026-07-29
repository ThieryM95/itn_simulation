############################################################################
# Visualise results of analysis 2: heatMap                                 #
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
    size = 15 / constant,
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
    size = 15 / constant,
    angle = 0
  ),
  strip.text.y = element_text(
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


c.labs <-  c("Whole population CFP-PYR-ITN\nvs Whole population PYR-ITN", "CFP-PYR-ITN users\nvs PYR-ITN users", "CFP-PYR-ITN non-users\nvs PYR-ITN non-users","CFP-PYR-ITN non-users\nvs PYR-ITN users")
names(c.labs) <- c("All IG2 vs\n All PYR", "Protected IG2 vs\n Protected PYR", "Unprotected IG2 vs\n Unprotected PYR","Unprotected IG2 vs\n Protected PYR")


#################
# Visualisation #
#################


# Load the data
###############


# Define the output directory
experiment<-"output_Final_All_2" # Name of the output folder
outdir_experiment = paste0("/scicore/home/penny/masthi00/OUT_itn_simulation/", experiment)

# Define EIR and level of Access to treatment
EIR<-c(10, 40, 100)
ACCESS <- 0.2

# Load each data
summary_file_1a <- paste0("/Output_summary_heatmap",EIR[1],ACCESS, ".txt")
summary_file_1b <- paste0("/Output_summary_heatmap",EIR[2],ACCESS, ".txt")
summary_file_1c <- paste0("/Output_summary_heatmap",EIR[3],ACCESS, ".txt")

summary_path_1a <- paste0(outdir_experiment, summary_file_1a)
summary_path_1b <- paste0(outdir_experiment, summary_file_1b)
summary_path_1c <- paste0(outdir_experiment, summary_file_1c)

heat_map_combined_1a<-read.table(summary_path_1a, sep = ";", header = T)
heat_map_combined_1b<-read.table(summary_path_1b, sep = ";", header = T)
heat_map_combined_1c<-read.table(summary_path_1c, sep = ";", header = T)

# Merge
heat_map_combined<-rbind(heat_map_combined_1a,
                         heat_map_combined_1b,
                         heat_map_combined_1c)


# Make all variable factorial
heat_map_combined$coverage_PYR <- as.factor(heat_map_combined$coverage_PYR )
heat_map_combined$coverage_IG2 <- as.factor (heat_map_combined$coverage_IG2)
heat_map_combined$comparision <- as.factor (heat_map_combined$comparision)
heat_map_combined$comparision <- factor(
  heat_map_combined$comparision,
  levels = c("All IG2 vs\n All PYR", "Protected IG2 vs\n Protected PYR", "Unprotected IG2 vs\n Unprotected PYR", "Unprotected IG2 vs\n Protected PYR")
)


# Figure 4
###########

Plot_HM<-ggplot(heat_map_combined %>% filter(seasonality == "Seasonal" & human_blood_index == "High"  & EHT == "Nguessan_2")) +
  aes(
    x = coverage_PYR,
    y = coverage_IG2,
    fill = relative_reduction
  ) +
  facet_nested(EHT+eir~comparision, labeller = labeller(eir = e.labs, EHT = eht.labs, comparision=c.labs))+
  geom_tile() +
  scale_fill_gradient2(
    name = "Relative reduction in malaria cases",
    low = "#D84314FF",
    mid = "white",
    high = "#7E57C1FF",
    midpoint = 0,
    na.value = "black"
  ) +
  my_theme()+
  labs(y="Coverage of CFP-PYR-ITN", x= "Coverage of PYR-ITN")

# Save
outdir_experiment = paste0("/scicore/home/penny/masthi00/OUT_itn_simulation/", "Visualise_results/")
summary_file <- "Figure_5.pdf"
summary_path <- paste0(outdir_experiment, summary_file)

ggsave(summary_path, plot = Plot_HM, width = 26, height = 18, device = "pdf", units = "cm",  dpi = 300)


# Figure S10
###########
Plot_HM<-ggplot(heat_map_combined %>% filter(EHT == "Martin")) +
  aes(
    x = coverage_PYR,
    y = coverage_IG2,
    fill = relative_reduction
  ) +
  facet_nested(EHT+eir+human_blood_index~comparision+seasonality, labeller = labeller(eir = e.labs, EHT = eht.labs, comparision=c.labs, seasonality=S.labs, human_blood_index=H.labs))+
  geom_tile() +
  scale_fill_gradient2(
    name = "Relative reduction in malaria cases",
    low = "#D84314FF",
    mid = "white",
    high = "#7E57C1FF",
    midpoint = 0,
    na.value = "black"
  ) +
  my_theme()+
  labs(y="Coverage of CFP-PYR-ITN", x= "Coverage of PYR-ITN")

# Save
outdir_experiment = paste0("/scicore/home/penny/masthi00/OUT_itn_simulation/", "Visualise_results/")
summary_file <- "Figure_S10.pdf"
summary_path <- paste0(outdir_experiment, summary_file)

ggsave(summary_path, plot = Plot_HM, width = 40, height = 36, device = "pdf", units = "cm",  dpi = 300)



# Figure S11
###########
Plot_HM<-ggplot(heat_map_combined %>% filter(EHT == "Nguessan")) +
  aes(
    x = coverage_PYR,
    y = coverage_IG2,
    fill = relative_reduction
  ) +
  facet_nested(EHT+eir+human_blood_index~comparision+seasonality, labeller = labeller(eir = e.labs, EHT = eht.labs, comparision=c.labs, seasonality=S.labs, human_blood_index=H.labs))+
  geom_tile() +
  scale_fill_gradient2(
    name = "Relative reduction in malaria cases",
    low = "#D84314FF",
    mid = "white",
    high = "#7E57C1FF",
    midpoint = 0,
    na.value = "black"
  ) +
  my_theme()+
  labs(y="Coverage of CFP-PYR-ITN", x= "Coverage of PYR-ITN")


# Save
outdir_experiment = paste0("/scicore/home/penny/masthi00/OUT_itn_simulation/", "Visualise_results/")
summary_file <- "Figure_S11.pdf"
summary_path <- paste0(outdir_experiment, summary_file)

ggsave(summary_path, plot = Plot_HM, width = 40, height = 36, device = "pdf", units = "cm",  dpi = 300)


# Figure S12
############

Plot_HM<-ggplot(heat_map_combined %>% filter(EHT == "Nguessan_2")) +
  aes(
    x = coverage_PYR,
    y = coverage_IG2,
    fill = relative_reduction
  ) +
  facet_nested(EHT+eir+human_blood_index~comparision+seasonality, labeller = labeller(eir = e.labs, EHT = eht.labs, comparision=c.labs, seasonality=S.labs, human_blood_index=H.labs))+
  geom_tile() +
  scale_fill_gradient2(
    name = "Relative reduction in malaria cases",
    low = "#D84314FF",
    mid = "white",
    high = "#7E57C1FF",
    midpoint = 0,
    na.value = "black"
  ) +
  my_theme()+
  labs(y="Coverage of CFP-PYR-ITN", x= "Coverage of PYR-ITN")

# Save
outdir_experiment = paste0("/scicore/home/penny/masthi00/OUT_itn_simulation/", "Visualise_results/")
summary_file <- "Figure_S12.pdf"
summary_path <- paste0(outdir_experiment, summary_file)

ggsave(summary_path, plot = Plot_HM, width = 40, height = 36, device = "pdf", units = "cm",  dpi = 300)

