############################################################################
# Illustration of  the EIR in transmission setting with seasonal           #
# constant transmission                                                    #
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
################

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


c.labs <-  c("Whole population NGM-ITN vs\n Whole population PYR-ITN", "Protected NGM-ITN vs\n Protected PYR-ITN", "Unprotected NGM-ITN vs\n Unprotected PYR-ITN","Unprotected NGM-ITN vs\n Protected PYR-ITN")
names(c.labs) <- c("All IG2 vs\n All PYR", "Protected IG2 vs\n Protected PYR", "Unprotected IG2 vs\n Unprotected PYR","Unprotected IG2 vs\n Protected PYR")

#################
# Visualisation #
#################


# Load the data
data_seed_1<-read.table("/scicore/home/penny/masthi00/OUT_itn_simulation/output_PYR_12years_EPI/Output_postprocessed.txt", sep = ";", header = T)

# Select the data
data_seed<-data_seed_1 %>% filter(access==0.2, eir == 40, coverage==0.8, human_blood_index == "High", EHT=="Martin")

# Define time at which mass ITN distribution happen
time_int <- data_seed$survey[data_seed$measure == "nMassGVI" & data_seed$value >= 1]

# Plot
Plot_S <- ggplot() +
  # Input EIR as a black line
  geom_line(
    data = data_seed %>% filter(measure == "simulatedEIR"),
    aes(x = survey/73, y = value*73),
    colour = "black"
  ) +
  facet_nested(seasonality~ ., 
               labeller = labeller(seasonality = S.labs)) +
  my_theme() +
  labs(
    x = "Time (years)",
    y = "Entomological innoculation rate \n(inoculations per person per year)") +
  xlim(0,12) +
  geom_vline(xintercept = time_int/73, color = "grey", linetype = "dashed")


# Save
outdir_experiment = paste0("/scicore/home/penny/masthi00/OUT_itn_simulation/", "Visualise_results/")
summary_file <- "Figure_S1.pdf"
summary_path <- paste0(outdir_experiment, summary_file)

ggsave(summary_path, plot = Plot_S, width = 16, height = 12, device = "pdf", units = "cm",  dpi = 300)

