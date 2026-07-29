############################################################################
# Analysis 1                                                               #
#                                                                          #          
# Compare the impact of PYR-ITNs vs NGM-ITNs without compromise            #
# or with reduced (25% or 50%) coverage or deployment frequency            #
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

#################################################
# Formats output of each experiment of interest #
#################################################

# Define the output directory of the simulation we want analysis results
experiments <- c("output_IG2_12years_coverage_EPI", "output_IG2_12years_frequency_EPI", "output_PYR_12years_EPI") # can look at multiple output directory in parallel

for (experiment in experiments) {
  
  # Load the data
  ###############
  
  # Define output directory
  outdir_experiment = paste0("/scicore/home/penny/masthi00/OUT_itn_simulation/",
                             experiment)
  
  
  # Load the list of simulation run (input)
  scenarios = fread(paste0(outdir_experiment, "/scenarios.csv"))
  
  # Load the outputs
  d = fread(paste0(outdir_experiment, "/output.csv"))
  
  # remove NA values
  d = d[complete.cases(d), ]
  
  # remove first survey
  d = d[!d$survey == 1, ]
  
  # Merge input and ouptus table using index
  d <- merge(d, scenarios, by = c("index"))
  
  
  # Check variable\columns name
  ##############################
  
  # Add EHT details
  
  table(d$net)
  
  d <- d %>%
    mutate(
      EHT = case_when(
        str_detect(net, "martin") ~ "Martin",
        str_detect(net, "nguessan_V2") ~ "Nguessan_2",
        str_detect(net, "nguessan") ~ "Nguessan",
        TRUE ~ net
      )
    )
  
  table(d$EHT)
  
  # Name the measure
  table(d$measure)
  
  d  <- d  %>% mutate (
    measure = recode (
      measure,
      "0" = "nHost",
      "1" = "nInfect",
      "3" = "nPatent",
      "6" = "totalInfs",
      "14" = "nUncomp",
      "30" = "innoculationsPerAgeGroup",
      "35" = "inputEIR",
      "36" = "simulatedEIR",
      "43" = "nNewInfections",
      "56" = "nMassGVI",
      "58" = "nctGVI"
    )
  )
  
  table(d$measure)
  
  
  # Define the different age group and cohort
  table(d$ageGroup)
  
  d <- d  %>% mutate (ageGroup = recode (ageGroup, "0" = "Population", "1" = "All")) 

  table(d$ageGroup)
                   
  # Rename ITN
  table(d$net)
   
  d <- d %>%
    mutate(net = case_when(
      str_detect(net, "itn1") ~ "PYR",
      str_detect(net, "itn2") ~ "IG2",
      TRUE ~ net   # keep as-is if it doesn't match
    ))
  
  table(d$net)
  
  # Check seasonality
  table(d$seasonality)
  
  # Check HBI
  table(d$human_blood_index)
  
  # Make all measure standardized by the number of host
  ######################################################
  
  # Add for each each age group, survey, and measure the number of hosts at this survey
  d <- d %>% left_join(
    d  %>% filter(measure == "nHost") %>% select(c("index", "survey", "ageGroup", "value")),
    by = c("index", "survey", "ageGroup"),
    suffix = c("", "_nHost")
  )
  
  # check 
  d$value[d$measure == "nHost" &
            d$survey == "100" &
            d$index == "10"]
  
  d$value_nHost[d$measure == "nPatent" &
                  d$survey == "100" &
                  d$index == "10"]
  
  # Divide each value by the number of hosts in each age group at this time step
  d <- d %>%
    mutate(normalised_value = value / value_nHost)
  
  d2 <- d
  

  # Estimate the number of cases 
  ################################
  
  # Average across seed
  data_seed <- d2 %>%
    group_by(
      survey,
      ageGroup,
      measure,
      net,
      access,
      eir,
      coverage,
      reduction_coverage,
      deployment_frequency,
      seasonality,
      human_blood_index,
      EHT
    ) %>%
    summarise(value  = mean(value),
              value_nHost = mean(value_nHost)) %>%
    mutate(normalised_value = value / value_nHost) %>%
    ungroup()
  
  # double check that is correct
  mean(d2$normalised_value[d2$survey == 200 &
                             d2$measure == "nUncomp" &
                             d2$deployment_frequency == "3" &
                             d2$reduction_coverage == 0 &
                             d2$coverage == 0.2 &
                             d2$access == 0.04 &
                             d2$eir == 10 &
                             d2$EHT == "Nguessan_2" &
                             d2$seasonality == "Perennial" &
                             d2$human_blood_index == "High"])
  
  data_seed$normalised_value[data_seed$survey == 200 &
                               data_seed$measure == "nUncomp" &
                               data_seed$deployment_frequency == "3" &
                               data_seed$reduction_coverage == 0 &
                               data_seed$coverage == 0.2 &
                               data_seed$access == 0.04 &
                               data_seed$eir == 10 &
                               data_seed$EHT == "Nguessan_2" &
                               data_seed$seasonality == "Perennial" &
                               data_seed$human_blood_index == "High"]
  
  # Save the  dataset in the summarized folder
  summary_file <- "/Output_postprocessed.txt"
  summary_path <- paste0(outdir_experiment, summary_file)
  write.table(
    data_seed,
    file = summary_path,
    sep = ";",
    col.names = TRUE,
    row.names = FALSE,
    quote = FALSE
  )
  
  # Extract data from the time of interest
  #########################################
  
  # Define the time of data used for analysis
  time_int <- data_seed$survey[data_seed$measure == "nMassGVI" &
                                 data_seed$value >= 1]
  time_start <- time_int[1] # it should be  75
  time_end <- max(data_seed$survey) - 73# it should be 949 (one year before end of simulaiton)
  
  # Select the data
  data_seed_2 <- data_seed %>% filter(survey >= time_start &
                                        survey <= time_end)
  
  # Estimate the sum over time
  ############################
  
  # Estimate the sum over time
  cumulative_data_seed <- data_seed_2 %>% filter(measure == "nUncomp") %>%
    group_by(
      ageGroup,
      net,
      access,
      eir,
      coverage,
      deployment_frequency,
      reduction_coverage,
      EHT,
      human_blood_index,
      seasonality
    ) %>%
    summarise(cumulative_case = sum(value),
              cumulative_nHost = mean(value_nHost)) %>%
    mutate(cumulative_normalised_value = cumulative_case / cumulative_nHost)
  
  # Double check that is correct
  sum(data_seed_2$normalised_value[data_seed_2$measure == "nUncomp" &
                                     data_seed_2$coverage == 0.2 &
                                     data_seed_2$reduction_coverage == 0.25 &
                                     data_seed_2$access == 0.04 &
                                     data_seed_2$eir == 10 &
                                     data_seed_2$deployment_frequency == 3 &
                                     data_seed_2$EHT == "Nguessan_2" &
                                     data_seed_2$human_blood_index == "High" &
                                     data_seed_2$seasonality == "Perennial"])
  
  cumulative_data_seed$cumulative_normalised_value[cumulative_data_seed$coverage == 0.2 &
                                                     cumulative_data_seed$reduction_coverage == 0.25 &
                                                     cumulative_data_seed$access == 0.04 &
                                                     cumulative_data_seed$eir == 10 &
                                                     cumulative_data_seed$deployment_frequency == 3 &
                                                     cumulative_data_seed$EHT == "Nguessan_2" &
                                                     cumulative_data_seed$human_blood_index ==
                                                     "High" &
                                                     cumulative_data_seed$seasonality ==
                                                     "Perennial"]
  
  
  # Save the  dataset in the summarized folder
  summary_file <- "/Output_summary.txt"
  summary_path <- paste0(outdir_experiment, summary_file)
  write.table(
    cumulative_data_seed,
    file = summary_path,
    sep = ";",
    col.names = TRUE,
    row.names = FALSE,
    quote = FALSE
  )
  
}


###################################################
# Estimate relative reduction NGM-ITN vs PYR-ITNs #
###################################################

# Load each scenario we wanted to compare (PYR-ITN vs NGM-ITN)
cumulative_data_seed_1 <- read.table(
  "/scicore/home/penny/masthi00/OUT_itn_simulation/output_PYR_12years_EPI/Output_summary.txt",
  sep = ";",
  header = T
)
cumulative_data_seed_2 <- read.table(
  "/scicore/home/penny/masthi00/OUT_itn_simulation/output_IG2_12years_frequency_EPI/Output_summary.txt",
  sep = ";",
  header = T
)
cumulative_data_seed_3 <- read.table(
  "/scicore/home/penny/masthi00/OUT_itn_simulation/output_IG2_12years_coverage_EPI/Output_summary.txt",
  sep = ";",
  header = T
)

# merge the data
cumulative_data_seed <- rbind(cumulative_data_seed_1,
                              cumulative_data_seed_2,
                              cumulative_data_seed_3)

# For each NGM-ITN secenario add a column with the value for the same scenairo under PYR-ITN
cumulative_data_seed_merge <- cumulative_data_seed %>%
  left_join(
    cumulative_data_seed %>%
      filter(net == "PYR") %>%
      select(
        EHT,
        access,
        eir,
        coverage,
        human_blood_index,
        seasonality,
        cumulative_case,
        cumulative_nHost,
        cumulative_normalised_value
      ) %>%
      rename(
        cumulative_case_PYR = cumulative_case,
        cumulative_nHost_PYR = cumulative_nHost,
        cumulative_normalised_value_PYR = cumulative_normalised_value
      ),
    by = c(
      "EHT",
      "access",
      "eir",
      "coverage",
      "human_blood_index",
      "seasonality"
    )
  )

# Double check that it is correct
cumulative_data_seed$cumulative_normalised_value[cumulative_data_seed$coverage == 0.9 &
                                                   cumulative_data_seed$access == 0.04 &
                                                   cumulative_data_seed$eir == 10 &
                                                   cumulative_data_seed$EHT == "Nguessan_2" &
                                                   cumulative_data_seed$net == "PYR" &
                                                   cumulative_data_seed$human_blood_index == "High" &
                                                   cumulative_data_seed$seasonality == "Perennial"]

cumulative_data_seed_merge[cumulative_data_seed_merge$coverage == 0.9 &
                             cumulative_data_seed_merge$access == 0.04 &
                             cumulative_data_seed_merge$eir == 10 &
                             cumulative_data_seed_merge$EHT == "Nguessan_2" &
                             cumulative_data_seed_merge$net == "IG2" &
                             cumulative_data_seed_merge$human_blood_index == "High" &
                             cumulative_data_seed_merge$seasonality == "Perennial", ]


# Estimate relative reduction of NGM-ITN vs PYR-ITN
cumulative_data_seed_merge <- cumulative_data_seed_merge %>%
  mutate(relative_reduction = (
    1 - (
      cumulative_normalised_value / cumulative_normalised_value_PYR
    )
  ) * 100)


# Save the  dataset in the summarized folder
summary_file <- "/Output_summary_Analysis_1_merged.txt"
outdir_experiment = paste0("/scicore/home/penny/masthi00/OUT_itn_simulation/",
                           "Visualise_results")
summary_path <- paste0(outdir_experiment, summary_file)
write.table(
  cumulative_data_seed_merge,
  file = summary_path,
  sep = ";",
  col.names = TRUE,
  row.names = FALSE)
