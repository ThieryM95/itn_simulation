############################################################################
# Visualise results of analysis 2: Cumulative case per coverage per Age    #
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
AG.labs <- ~ paste0("Age = ", as.factor(.x), " year old")


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
################

# Save the  dataset in the summarized folder
experiment <- "output_Final_All_2"
outdir_experiment = paste0("/scicore/home/penny/masthi00/OUT_itn_simulation/", experiment)
summary_file <-  "/Output_postprocessed.txt"
summary_path <- paste0(outdir_experiment, summary_file)
data_seed <-read.table(summary_path, sep = ";", header = T)


# Make the data ready for visualisation
#######################################

# Define the time of data used for analysis
time_int <-unique(data_seed[data_seed$cohort=="ITN" & data_seed$measure == "nHost" & data_seed$value >= 1,]$survey) 
time_start <- time_int[1] # it should be survey 75
time_end <- time_int[length(time_int)] # it should be 294 (14 + 12*3)

# Select the data
data_seed_2 <- data_seed %>% filter(survey >= time_start &
                                      survey <= time_end) 

# Estimate the sum over time
cumulative_data_seed <- data_seed_2 %>% filter(measure == "nUncomp") %>%
  group_by(ageGroup, cohort, net, access, eir, coverage, EHT,  reduction_coverage, deployment_frequency, seasonality,human_blood_index) %>%
  summarise(cumulative_case = sum(value),
            cumulative_nHost = mean(value_nHost))%>% 
  mutate(cumulative_normalised_value = cumulative_case / cumulative_nHost)

# Double check that is correct
sum(data_seed_2$value[data_seed_2$ageGroup == "0-5" &
                        data_seed_2$measure == "nUncomp"&
                        data_seed_2$coverage == 0.2 &
                        data_seed_2$access == 0.04 &
                        data_seed_2$eir == 10 &
                        data_seed_2$cohort == "ITN"&
                        data_seed_2$net == "PYR"&
                        data_seed_2$seasonality == "Perennial" &
                        data_seed_2$human_blood_index == "High" &
                        data_seed_2$EHT =="Martin"])/
  mean(data_seed_2$value[data_seed_2$ageGroup == "0-5" &
                           data_seed_2$measure == "nHost"&
                           data_seed_2$coverage == 0.2 &
                           data_seed_2$access == 0.04 &
                           data_seed_2$eir == 10 &
                           data_seed_2$cohort == "ITN"&
                           data_seed_2$net == "PYR"&
                           data_seed_2$seasonality == "Perennial" &
                           data_seed_2$human_blood_index == "High" &
                           data_seed_2$EHT =="Martin"])

cumulative_data_seed$cumulative_normalised_value[cumulative_data_seed$ageGroup == "0-5" &
                                                   cumulative_data_seed$coverage == 0.2 &
                                                   cumulative_data_seed$access == 0.04 &
                                                   cumulative_data_seed$eir == 10 &
                                                   cumulative_data_seed$cohort == "ITN"&
                                                   cumulative_data_seed$net == "PYR"&
                                                   cumulative_data_seed$seasonality == "Perennial" &
                                                   cumulative_data_seed$human_blood_index == "High" &
                                                   cumulative_data_seed$EHT =="Martin"]

# Extract the cumulative number of uncomplicated malaria cases in the ITN_1 simulation and put it as a columns
cumulative_data_seed_2 <- cumulative_data_seed %>% 
  left_join(
    cumulative_data_seed %>%
      filter(net == "PYR") %>%
      select(ageGroup, cohort, EHT, access, eir, coverage, reduction_coverage, deployment_frequency, seasonality,human_blood_index, cumulative_normalised_value) %>%
      rename(cumulative_normalised_value_PYR = cumulative_normalised_value),
    by = c("ageGroup","cohort", "EHT", "access", "eir", "coverage", "reduction_coverage", "deployment_frequency", "seasonality", "human_blood_index")
  )

# double check that is correct
cumulative_data_seed$cumulative_normalised_value[
  cumulative_data_seed$cohort == "Placebo" &
    cumulative_data_seed$coverage == 0.2 &
    cumulative_data_seed$access == 0.04 &
    cumulative_data_seed$eir == 10 &
    cumulative_data_seed$net == "PYR"&
    cumulative_data_seed$seasonality == "Perennial" &
    cumulative_data_seed$human_blood_index == "High" &
    cumulative_data_seed$EHT =="Martin"]

cumulative_data_seed_2$cumulative_normalised_value_PYR[
  cumulative_data_seed_2$cohort == "Placebo" &
    cumulative_data_seed_2$coverage == 0.2 &
    cumulative_data_seed_2$access == 0.04 &
    cumulative_data_seed_2$eir == 10 &
    cumulative_data_seed_2$net.x == "IG2"&
    cumulative_data_seed_2$seasonality == "Perennial" &
    cumulative_data_seed_2$human_blood_index == "High" &
    cumulative_data_seed_2$EHT =="Martin"]


# Figure S13
#############

# Select the data
EIR<-c(10,40,100)
ACCESS<- 0.2

cumulative_data_seed_select <- cumulative_data_seed_2%>%
  filter(eir %in% EIR, access ==ACCESS , cohort %in% c("ITN", "Placebo"),  seasonality == "Perennial" , human_blood_index == "High", EHT %in% c("Nguessan_2"))%>%
  mutate(group = paste0(net.x, "_", cohort))


# make factor
cumulative_data_seed_select$ageGroup<-factor(
  cumulative_data_seed_select$ageGroup,
  levels = c("0-5", "5-10", "10-100", "All"))


# Plot
Plot_3<-ggplot(cumulative_data_seed_select,
               aes(x = coverage, y = cumulative_normalised_value, color=group)) +
  geom_point() +
  geom_line() +
  ylim(0,12) +
  my_theme() +
  facet_nested(EHT+ageGroup~eir, labeller = labeller(eir = e.labs, EHT = eht.labs, ageGroup=AG.labs))+
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
summary_file <- "Figure_S13.pdf"
summary_path <- paste0(outdir_experiment, summary_file)

ggsave(summary_path, plot = Plot_3, width = 22, height = 24, device = "pdf", units = "cm",  dpi = 300)

