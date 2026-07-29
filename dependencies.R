################################################################################
#                                                                              #
# PUROOSE: all package and supplementary dependencies in one place             #                                                                  #
#                                                                              #
# Author: Aurélien Cavelan (aurelien.cavelan@swisstph.ch)                      #
#                                                                              #
################################################################################


# load local files
source("pacman.R")
source("run.R")
source("extract.R")

# Load all required packages, installing them if required
pacman::p_load(char = c("foreach", "doParallel", "dplyr", "data.table"))


# Get details of R version currently running
version_info = R.Version()

# #### Define packages ####

# Complete list of all R packages required for this project
packages = c(
  "tidyverse",
  "tidytext",
  "data.table",
  "utils",
  "stats",
  "matrixStats",
  "roperators",
  "stringr",
  "readr",
  "xlsx",
  "drc",
  "latex2exp",
  "wrapr",
  "scales",
  "MetBrewer",
  "patchwork",
  "ggh4x",
  "Rmisc",
  "ggsci",
  "wesanderson",
  "cowplot",
  "survival",
  "LogicReg",
  "gridExtra",
  "hetGP",
  "pals",
  "colorspace",
  "ggridges",
  "ggalluvial",
  "qwraps2",
  "svglite",
  "ggpubr",
  "compare",
  "colorblindcheck",
  "DescTools",
  "CGPfunctions",
  "broom"
)


# #### Install and/or load packages with pacman ####

message("* Installing required packages")

# If pacman is not yet installed, install it
if (!require("pacman"))
  install.packages("pacman")

# Load pacman
library(pacman)

# Load all required packages, installing them if required
pacman::p_load(char = packages)

# #### Redefine or unmask particular functions ####

# Unmask certain functions otherwise overwritten
union   = dplyr::union
select  = dplyr::select
filter  = dplyr::filter
rename  = dplyr::rename
mutate = dplyr::mutate
predict = stats::predict
as.data.frame = base::as.data.frame

# #### Additional: define notin ####
`%notin%` <- Negate(`%in%`)

# Disable summaries by which group information
options(dplyr.summarise.inform = FALSE)

# #### Additional: track package version ####

get_package_version <- function(pkg) {
  if (pkg %in% installed.packages()[, "Package"]) {
    return(as.character(packageVersion(pkg)))
  } else {
    return(NA)
  }
}

# Create a list of package versions
package_versions <- sapply(packages, get_package_version)

# Convert the list to a data frame
package_versions_df <- data.frame(
  Package = names(package_versions),
  Version = package_versions,
  stringsAsFactors = FALSE
)

# Print the data frame
print(package_versions_df)

# #### Additional: add back-end option to seamlessly save figures ####
options(bitmapType = 'cairo')


# #### Additional: record color palettes used ##################################

# colorBlindGrey8 inspired

colorBlindGrey8   <- c(
  "#999999",
  "#E69F00",
  "#56B4E9",
  "#009E73",
  "#F0E442",
  "#0072B2",
  "#D55E00",
  "#CC79A7"
)

# Darjeeling inspired

colfunc <- colorRampPalette(c("#5ABCD6", "#F98400"))

colors_dg = colfunc(4)

#get hex codes
#print(colors_dg)

colors_dg = c("#5ABCD6", "#8FA98E", "#C39647", "#F98400")
