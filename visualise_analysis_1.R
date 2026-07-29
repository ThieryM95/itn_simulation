#####################################################################
# Visualise results of analysis 1                                   #
#                                                                   #
# Compare the impact of PYR-ITNs vs CFP-PYR-ITNs without compromise     #
# or with reduced (25% or 50%) coverage or deployment frequency     #
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

# Palette color
okabe_ito_palette <- c(
  "CFP-PYR-ITN with a reduction\nof deployment frequency of 25 %" = "#5EBC89",
  # dark green
  "CFP-PYR-ITN with a reduction\nof deployment frequency of 50 %" = "#5EBC89",
  # light green
  
  "CFP-PYR-ITN with a reduction\nof coverage of 25 %" = "#8978B9",
  # dark purple
  "CFP-PYR-ITN with a reduction\nof coverage of 50 %" = "#8978B9",
  # light purple
  
  "CFP-PYR-ITN" = "black"
) 


# Palette shape
okabe_ito_shape <- c(
  "CFP-PYR-ITN with a reduction\nof deployment frequency of 25 %" = 16,
  # dark green
  "CFP-PYR-ITN with a reduction\nof deployment frequency of 50 %" = 17,
  # light green
  
  "CFP-PYR-ITN with a reduction\nof coverage of 25 %" = 16,
  # dark purple
  "CFP-PYR-ITN with a reduction\nof coverage of 50 %" = 17,
  # light purple
  
  "CFP-PYR-ITN" = 16
)


# Palette dote
okabe_ito_line <- c(
  "CFP-PYR-ITN with a reduction\nof deployment frequency of 25 %" = "solid",
  # dark green
  "CFP-PYR-ITN with a reduction\nof deployment frequency of 50 %" = "dashed",
  # light green
  
  "CFP-PYR-ITN with a reduction\nof coverage of 25 %" = "solid",
  # dark purple
  "CFP-PYR-ITN with a reduction\nof coverage of 50 %" = "dashed",
  # light purple
  
  "CFP-PYR-ITN" = "solid"
)

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



# Load the data
###############

# Load the data
cumulative_data_seed_merge <- read.table(
  "/scicore/home/penny/masthi00/OUT_itn_simulation/Visualise_results/Output_summary_Analysis_1_merged.txt",
  sep = ";",
  header = T
)

# Make all variable factorial
cumulative_data_seed_merge$access <- as.factor(cumulative_data_seed_merge$access)
cumulative_data_seed_merge$coverage <- as.factor(cumulative_data_seed_merge$coverage)
cumulative_data_seed_merge$eir <- as.factor(cumulative_data_seed_merge$eir)
cumulative_data_seed_merge$seasonality <- as.factor(cumulative_data_seed_merge$seasonality)
cumulative_data_seed_merge$human_blood_index <- as.factor(cumulative_data_seed_merge$human_blood_index)


# Define a variable strategy for plot labelling
cumulative_data_seed_merge$strategy <- "Standard - PYR-ITN"
cumulative_data_seed_merge$strategy[cumulative_data_seed_merge$deployment_frequency ==
                                      4] <- "CFP-PYR-ITN with a reduction\nof deployment frequency of 25 %"
cumulative_data_seed_merge$strategy[cumulative_data_seed_merge$deployment_frequency ==
                                      6] <- "CFP-PYR-ITN with a reduction\nof deployment frequency of 50 %"
cumulative_data_seed_merge$strategy[cumulative_data_seed_merge$reduction_coverage ==
                                      0.25] <- "CFP-PYR-ITN with a reduction\nof coverage of 25 %"
cumulative_data_seed_merge$strategy[cumulative_data_seed_merge$reduction_coverage ==
                                      0.5] <- "CFP-PYR-ITN with a reduction\nof coverage of 50 %"
cumulative_data_seed_merge$strategy[cumulative_data_seed_merge$reduction_coverage ==
                                      0 &
                                      cumulative_data_seed_merge$deployment_frequency == 3] <- "CFP-PYR-ITN"



############
# Figure 3 #
############

# Select the data
plot_data <- cumulative_data_seed_merge %>% filter(
  eir %in% c("10", "40", "100"),
  access == 0.2,
  seasonality == "Perennial",
  human_blood_index == "High"
)

# Plot the data
PLOT2 <- ggplot(data = plot_data  %>% filter(net == "IG2")) +
  aes(
    x = coverage,
    y = relative_reduction,
    color = strategy,
    linetype = strategy,
    shape = strategy,
    group = strategy
  ) +
  geom_point(size = 3) +
  geom_line() +
  facet_nested(EHT ~ eir , labeller = labeller(eir = e.labs, EHT = eht.labs)) +
  my_theme() +
  xlab("Coverage of PYR-ITN (%)") +
  ylab("Protective effectiveness of CFP-PYR-ITN compared to PYR-ITN (%)") +
  ylim(-50, 100) +
  guides(
    color = guide_legend(title = "Strategy:"),
    linetype = guide_legend(title = "Strategy:"),
    shape = guide_legend(title = "Strategy:")
  ) +
  scale_color_manual(values = okabe_ito_palette) +
  scale_linetype_manual(values = okabe_ito_line) +
  scale_shape_manual(values = okabe_ito_shape)


# Save the results
outdir_experiment = paste0("/scicore/home/penny/masthi00/OUT_itn_simulation/",
                           "Visualise_results/")

summary_file <- "Figure_3.pdf"
summary_path <- paste0(outdir_experiment, summary_file)

ggsave(
  summary_path,
  plot = PLOT2,
  width = 30,
  height = 18,
  device = "pdf",
  units = "cm",
  dpi = 300
)


#############
# Figure S3 #
#############

# Select the data
plot_data <- cumulative_data_seed_merge %>% filter(
  eir %in% c("10", "40", "100"),
  access == 0.04,
  seasonality == "Perennial",
  human_blood_index == "High"
)

# Plot the data
PLOT2 <- ggplot(data = plot_data  %>% filter(net == "IG2")) +
  aes(
    x = coverage,
    y = relative_reduction,
    color = strategy,
    linetype = strategy,
    shape = strategy,
    group = strategy
  ) +
  geom_point(size = 3) +
  geom_line() +
  facet_nested(EHT ~ eir, labeller = labeller(eir = e.labs, EHT = eht.labs)) +
  my_theme() +
  xlab("Coverage of PYR-ITN (%)") +
  ylab("Protective effectiveness of CFP-PYR-ITN compared to PYR-ITN (%)") +
  ylim(-50, 100) +
  guides(
    color = guide_legend(title = "Strategy:"),
    linetype = guide_legend(title = "Strategy:"),
    shape = guide_legend(title = "Strategy:")
  ) +
  scale_color_manual(values = okabe_ito_palette) +
  scale_linetype_manual(values = okabe_ito_line) +
  scale_shape_manual(values = okabe_ito_shape)


# Save the data
outdir_experiment = paste0("/scicore/home/penny/masthi00/OUT_itn_simulation/",
                           "Visualise_results/")

summary_file <- "Figure_S3.pdf"
summary_path <- paste0(outdir_experiment, summary_file)

ggsave(
  summary_path,
  plot = PLOT2,
  width = 30,
  height = 18,
  device = "pdf",
  units = "cm",
  dpi = 300
)




#############
# Figure S4 #
#############

# Select the data
plot_data <- cumulative_data_seed_merge %>% filter(eir %in% c("10"), access == 0.2)

# Plot the data
PLOT2 <- ggplot(data = plot_data  %>% filter(net == "IG2")) +
  aes(
    x = coverage,
    y = relative_reduction,
    color = strategy,
    linetype = strategy,
    shape = strategy,
    group = strategy
  ) +
  geom_point(size = 3) +
  geom_line() +
  facet_nested(
    eir ~ EHT ~  seasonality + human_blood_index,
    labeller = labeller(
      eir = e.labs,
      EHT = eht.labs,
      seasonality = S.labs,
      human_blood_index = H.labs
    )
  ) +
  my_theme() +
  xlab("Coverage of PYR-ITN (%)") +
  ylab("Protective effectiveness of CFP-PYR-ITN compared to PYR-ITN (%)") +
  guides(
    color = guide_legend(title = "Strategy:"),
    linetype = guide_legend(title = "Strategy:"),
    shape = guide_legend(title = "Strategy:")
  ) +
  scale_color_manual(values = okabe_ito_palette) +
  scale_linetype_manual(values = okabe_ito_line) +
  scale_shape_manual(values = okabe_ito_shape)


# Save the data
outdir_experiment = paste0("/scicore/home/penny/masthi00/OUT_itn_simulation/",
                           "Visualise_results/")

summary_file <- "Figure_S4.pdf"
summary_path <- paste0(outdir_experiment, summary_file)

ggsave(
  summary_path,
  plot = PLOT2,
  width = 30,
  height = 18,
  device = "pdf",
  units = "cm",
  dpi = 300
)


#############
# Figure S5 #
#############

# Select the data
plot_data <- cumulative_data_seed_merge %>% filter(eir %in% c("40"), access == 0.2)

# Plot the data
PLOT2 <- ggplot(data = plot_data  %>% filter(net == "IG2")) +
  aes(
    x = coverage,
    y = relative_reduction,
    color = strategy,
    linetype = strategy,
    shape = strategy,
    group = strategy
  ) +
  geom_point(size = 3) +
  geom_line() +
  facet_nested(
    eir ~ EHT ~  seasonality + human_blood_index,
    labeller = labeller(
      eir = e.labs,
      EHT = eht.labs,
      seasonality = S.labs,
      human_blood_index = H.labs
    )
  ) +
  my_theme() +
  xlab("Coverage of PYR-ITN (%)") +
  ylab("Protective effectiveness of CFP-PYR-ITN compared to PYR-ITN (%)") +
  guides(
    color = guide_legend(title = "Strategy:"),
    linetype = guide_legend(title = "Strategy:"),
    shape = guide_legend(title = "Strategy:")
  ) +
  scale_color_manual(values = okabe_ito_palette) +
  scale_linetype_manual(values = okabe_ito_line) +
  scale_shape_manual(values = okabe_ito_shape)


# Save the data
outdir_experiment = paste0("/scicore/home/penny/masthi00/OUT_itn_simulation/",
                           "Visualise_results/")

summary_file <- "Figure_S5.pdf"
summary_path <- paste0(outdir_experiment, summary_file)

ggsave(
  summary_path,
  plot = PLOT2,
  width = 30,
  height = 18,
  device = "pdf",
  units = "cm",
  dpi = 300
)

#############
# Figure S6 #
#############

# Select the data
plot_data <- cumulative_data_seed_merge %>% filter(eir %in% c("100"), access == 0.2)

# Plot the data
PLOT2 <- ggplot(data = plot_data  %>% filter(net == "IG2")) +
  aes(
    x = coverage,
    y = relative_reduction,
    color = strategy,
    linetype = strategy,
    shape = strategy,
    group = strategy
  ) +
  geom_point(size = 3) +
  geom_line() +
  facet_nested(
    eir ~ EHT ~  seasonality + human_blood_index,
    labeller = labeller(
      eir = e.labs,
      EHT = eht.labs,
      seasonality = S.labs,
      human_blood_index = H.labs
    )
  ) +
  my_theme() +
  xlab("Coverage of PYR-ITN (%)") +
  ylab("Protective effectiveness of CFP-PYR-ITN compared to PYR-ITN (%)") +
  guides(
    color = guide_legend(title = "Strategy:"),
    linetype = guide_legend(title = "Strategy:"),
    shape = guide_legend(title = "Strategy:")
  ) +
  scale_color_manual(values = okabe_ito_palette) +
  scale_linetype_manual(values = okabe_ito_line) +
  scale_shape_manual(values = okabe_ito_shape)


# Save the data
outdir_experiment = paste0("/scicore/home/penny/masthi00/OUT_itn_simulation/",
                           "Visualise_results/")

summary_file <- "Figure_S6.pdf"
summary_path <- paste0(outdir_experiment, summary_file)

ggsave(
  summary_path,
  plot = PLOT2,
  width = 30,
  height = 18,
  device = "pdf",
  units = "cm",
  dpi = 300
)
