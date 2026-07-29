############################################################################
# Analysis 2                                                               #
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

#################################################
# Formats output of each experiment of interest #
#################################################

# Load the data
###############

# Define the output directory
experiment <- "output_Final_All_2" # Name of the output folder
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


# Name EHT
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

# Rename ITN

table(d$net)

d <- d %>%
  mutate(net = case_when(
    str_detect(net, "itn1") ~ "PYR",
    str_detect(net, "itn2") ~ "IG2",
    TRUE ~ net   # keep as-is if it doesn't match
  ))

table(d$net)

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


# Define the different age group and cohort (Protected, Unprotected, All)
#########################################################################

# Identify individual that are part of ITN or Placebo cohort or both (both should be no one)
table(d$ageGroup)

# Individual that start by 3XXX are  part of both ITN and Placebo cohrot, should be zero and removed
d_age3 <- d[d$ageGroup %in% c("3001", "3002", "3003")]
summary(d_age3$value) # should be all Zero

# Remove cohort starting with 3XXX
d <- subset(d, !(ageGroup %in% c("3001", "3002", "3003")))

# Check
table(d$ageGroup)


# Separate the infromation about age group and cohort into two different columns
d <-
  d  %>%
  mutate(
    cohort = case_when(
      ageGroup ==  "0" ~ "All",
      ageGroup ==  "1" ~ "Other",
      ageGroup ==  "2" ~ "Other",
      ageGroup ==  "3" ~ "Other",
      ageGroup ==  "1001" ~ "ITN",
      ageGroup ==  "1002" ~ "ITN",
      ageGroup ==  "1003" ~ "ITN",
      ageGroup ==  "2001" ~ "Placebo",
      ageGroup ==  "2002" ~ "Placebo",
      ageGroup ==  "2003" ~ "Placebo"
    )
  ) %>% mutate (
    ageGroup = recode (
      ageGroup,
      "0" = "All",
      "1" = "0-5",
      # individual that did not received ITN
      "2" = "5-10",
      # individual that did not received ITN
      "3" = "10-100",
      # individual that did not received ITN
      
      "1001" = "0-5",
      "1002" = "5-10",
      "1003" = "10-100",
      
      "2001" = "0-5",
      "2002" = "5-10",
      "2003" = "10-100"
    ) # individual in the cohort that received ITN
  )

# Check
table(d$cohort)
table(d$ageGroup)


# Make all measure standardized by the number of host
######################################################

# Add for each each age group, survey, and measure the number of hosts at this survey
d <- d %>% left_join(
  d  %>% filter(measure == "nHost") %>% select(c(
    "index", "survey", "cohort", "ageGroup", "value"
  )),
  by = c("index", "survey", "cohort", "ageGroup"),
  suffix = c("", "_nHost")
)

# Check correct
d$value[d$measure == "nHost" &
          d$survey == "100" &
          d$ageGroup == "0-5" &
          d$index == "10" &
          d$cohort == "Placebo"]

d$value_nHost[d$measure == "nPatent" &
                d$survey == "100" &
                d$ageGroup == "0-5" &
                d$index == "10" &
                d$cohort == "Placebo"]

# Divide each value by the number of hosts in each age group at this time step
d <- d %>%
  mutate(normalised_value = value / value_nHost)


d2 <- d


################################
# Estimate protective efficacy #
################################

# Average across seed
data_seed <- d2 %>%
  group_by(
    survey,
    ageGroup,
    measure,
    cohort,
    net,
    access,
    eir,
    coverage,
    EHT,
    reduction_coverage,
    deployment_frequency,
    seasonality,
    human_blood_index
  ) %>%
  summarise(value  = mean(value),
            value_nHost = mean(value_nHost)) %>%
  mutate(normalised_value = value / value_nHost) %>%
  ungroup()

# double check that is correct
mean(d2$value_nHost[d2$survey == 200 &
                      d2$ageGroup == "0-5" &
                      d2$measure == "nUncomp" &
                      d2$cohort == "ITN" &
                      d2$coverage == 0.2 &
                      d2$access == 0.04 &
                      d2$eir == 10 &
                      d2$net == "PYR" &
                      d2$seasonality == "Perennial" &
                      d2$human_blood_index == "High" &
                      d2$EHT == "Martin"])

data_seed$value_nHost[data_seed$survey == 200 &
                        data_seed$ageGroup == "0-5" &
                        data_seed$measure == "nUncomp" &
                        data_seed$cohort == "ITN" &
                        data_seed$coverage == 0.2 &
                        data_seed$access == 0.04 &
                        data_seed$eir == 10 &
                        data_seed$net == "PYR" &
                        data_seed$seasonality == "Perennial" &
                        data_seed$human_blood_index == "High" &
                        data_seed$EHT == "Martin"]

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

# Define the time of data used for analysis
time_int <- unique(data_seed[data_seed$cohort == "ITN" &
                               data_seed$measure == "nHost" & data_seed$value >= 1, ]$survey)
time_start <- time_int[1] # it should be survey 75
time_end <- time_int[length(time_int)] # it should be 294 (14 + 12*3)

# Select the data
data_seed_2 <- data_seed %>% filter(survey >= time_start &
                                      survey <= time_end)

# Estimate the sum over time
cumulative_data_seed <- data_seed_2 %>% filter(measure == "nUncomp") %>%
  group_by(
    ageGroup,
    cohort,
    net,
    access,
    eir,
    coverage,
    EHT,
    reduction_coverage,
    deployment_frequency,
    seasonality,
    human_blood_index
  ) %>%
  summarise(cumulative_case = sum(value),
            cumulative_nHost = mean(value_nHost)) %>%
  mutate(cumulative_normalised_value = cumulative_case / cumulative_nHost)

# Double check that is correct
sum(data_seed_2$value[data_seed_2$ageGroup == "0-5" &
                        data_seed_2$measure == "nUncomp" &
                        data_seed_2$coverage == 0.2 &
                        data_seed_2$access == 0.04 &
                        data_seed_2$eir == 10 &
                        data_seed_2$cohort == "ITN" &
                        data_seed_2$net == "PYR" &
                        data_seed_2$seasonality == "Perennial" &
                        data_seed_2$human_blood_index == "High" &
                        data_seed_2$EHT == "Martin"]) /
  mean(data_seed_2$value[data_seed_2$ageGroup == "0-5" &
                           data_seed_2$measure == "nHost" &
                           data_seed_2$coverage == 0.2 &
                           data_seed_2$access == 0.04 &
                           data_seed_2$eir == 10 &
                           data_seed_2$cohort == "ITN" &
                           data_seed_2$net == "PYR" &
                           data_seed_2$seasonality == "Perennial" &
                           data_seed_2$human_blood_index == "High" &
                           data_seed_2$EHT == "Martin"])

cumulative_data_seed$cumulative_normalised_value[cumulative_data_seed$ageGroup == "0-5" &
                                                   cumulative_data_seed$coverage == 0.2 &
                                                   cumulative_data_seed$access == 0.04 &
                                                   cumulative_data_seed$eir == 10 &
                                                   cumulative_data_seed$cohort == "ITN" &
                                                   cumulative_data_seed$net == "PYR" &
                                                   cumulative_data_seed$seasonality == "Perennial" &
                                                   cumulative_data_seed$human_blood_index == "High" &
                                                   cumulative_data_seed$EHT ==
                                                   "Martin"]

# Estimate data over age group
cumulative_data_seed_allage <- cumulative_data_seed %>%
  group_by(
    cohort,
    net,
    access,
    eir,
    coverage,
    EHT,
    reduction_coverage,
    deployment_frequency,
    seasonality,
    human_blood_index
  ) %>%
  summarise(
    cumulative_case = sum(cumulative_case),
    cumulative_nHost = sum(cumulative_nHost)
  ) %>%
  mutate(cumulative_normalised_value = cumulative_case / cumulative_nHost) %>%
  ungroup()

# Check
sum(cumulative_data_seed$cumulative_case[cumulative_data_seed$coverage == 0.8 &
                                           cumulative_data_seed$access == 0.04 &
                                           cumulative_data_seed$eir == 10 &
                                           cumulative_data_seed$cohort == "Other" &
                                           cumulative_data_seed$net == "PYR" &
                                           cumulative_data_seed$seasonality == "Perennial" &
                                           cumulative_data_seed$human_blood_index == "High" &
                                           cumulative_data_seed$EHT == "Martin"])


cumulative_data_seed_allage$cumulative_case[cumulative_data_seed_allage$coverage == 0.8 &
                                              cumulative_data_seed_allage$access == 0.04 &
                                              cumulative_data_seed_allage$eir == 10 &
                                              cumulative_data_seed_allage$cohort == "Other" &
                                              cumulative_data_seed_allage$net == "PYR" &
                                              cumulative_data_seed_allage$seasonality == "Perennial" &
                                              cumulative_data_seed_allage$human_blood_index == "High" &
                                              cumulative_data_seed_allage$EHT == "Martin"]


# Extract the cumulative number of uncomplicated malaria cases in the ITN_1 simulation and put it as a columns
cumulative_data_seed_allage <- cumulative_data_seed_allage %>%
  left_join(
    cumulative_data_seed_allage %>%
      filter(net == "PYR") %>%
      select(
        cohort,
        EHT,
        access,
        eir,
        coverage,
        reduction_coverage,
        deployment_frequency,
        seasonality,
        human_blood_index,
        cumulative_normalised_value
      ) %>%
      rename(cumulative_normalised_value_PYR = cumulative_normalised_value),
    by = c(
      "cohort",
      "EHT",
      "access",
      "eir",
      "coverage",
      "reduction_coverage",
      "deployment_frequency",
      "seasonality",
      "human_blood_index"
    )
  )

# double check that is correct
cumulative_data_seed_allage$cumulative_normalised_value[cumulative_data_seed_allage$cohort == "Placebo" &
                                                          cumulative_data_seed_allage$coverage == 0.2 &
                                                          cumulative_data_seed_allage$access == 0.04 &
                                                          cumulative_data_seed_allage$eir == 10 &
                                                          cumulative_data_seed_allage$net == "PYR" &
                                                          cumulative_data_seed_allage$seasonality == "Perennial" &
                                                          cumulative_data_seed_allage$human_blood_index == "High" &
                                                          cumulative_data_seed_allage$EHT == "Martin"]

cumulative_data_seed_allage$cumulative_normalised_value_PYR[cumulative_data_seed_allage$cohort == "Placebo" &
                                                              cumulative_data_seed_allage$coverage == 0.2 &
                                                              cumulative_data_seed_allage$access == 0.04 &
                                                              cumulative_data_seed_allage$eir == 10 &
                                                              cumulative_data_seed_allage$net == "IG2" &
                                                              cumulative_data_seed_allage$seasonality == "Perennial" &
                                                              cumulative_data_seed_allage$human_blood_index == "High" &
                                                              cumulative_data_seed_allage$EHT == "Martin"]



# Save the  dataset in the summarized folder
summary_file <- "/Output_summary.txt"
summary_path <- paste0(outdir_experiment, summary_file)
write.table(
  cumulative_data_seed_allage,
  file = summary_path,
  sep = ";",
  col.names = TRUE,
  row.names = FALSE,
  quote = FALSE
)



##############################################################################
# Estimate the relative reduction  across each population group and coverage #
##############################################################################


# Load the data
cumulative_data_seed_allage <- read.table(
  "/scicore/home/penny/masthi00/OUT_itn_simulation/output_Final_All_2/Output_summary.txt",
  sep = ";",
  header = T
)


# Loop across each EIR and Access (to make it more computentially efficient, we do it across level of EIR and access to treatement)
for (EIR in  unique(cumulative_data_seed_allage$eir)) {
  for (ACCESS in  unique(cumulative_data_seed_allage$access)) {
    
    # Select all individual Protected by an ITN in the simulation with PYR-ITN
    PYR_ITN <- cumulative_data_seed_allage %>%
      filter(eir == EIR, access == ACCESS, net == "PYR", cohort == "ITN") %>%
      select(
        coverage,
        access,
        eir,
        EHT,
        reduction_coverage,
        deployment_frequency,
        seasonality,
        human_blood_index,
        cumulative_case,
        cumulative_nHost,
        cumulative_normalised_value
      ) %>%
      rename(
        coverage_PYR = coverage,
        cumulative_case_PYR = cumulative_case,
        cumulative_nHost_PYR = cumulative_nHost,
        cumulative_normalised_value_PYR = cumulative_normalised_value
      )
    
    # Select all individual Unprotected by an ITN in the simulation with PYR-ITN
    PYR_Control <- cumulative_data_seed_allage %>%
      filter(eir == EIR, access == ACCESS, net == "PYR", cohort == "Placebo") %>%
      select(
        coverage,
        access,
        eir,
        EHT,
        reduction_coverage,
        deployment_frequency,
        seasonality,
        human_blood_index,
        cumulative_case,
        cumulative_nHost,
        cumulative_normalised_value
      ) %>%
      rename(
        coverage_PYR = coverage,
        cumulative_case_PYR = cumulative_case,
        cumulative_nHost_PYR = cumulative_nHost,
        cumulative_normalised_value_PYR = cumulative_normalised_value
      )
    
    # Select all individual Protected by an ITN in the simulation with NGM-ITN
    IG2_ITN <- cumulative_data_seed_allage %>%
      filter(eir == EIR, access == ACCESS, net == "IG2", cohort == "ITN") %>%
      select(
        coverage,
        access,
        eir,
        EHT,
        reduction_coverage,
        deployment_frequency,
        seasonality,
        human_blood_index,
        cumulative_case,
        cumulative_nHost,
        cumulative_normalised_value
      ) %>%
      rename(
        coverage_IG2 = coverage,
        cumulative_case_IG2 = cumulative_case,
        cumulative_nHost_IG2 = cumulative_nHost,
        cumulative_normalised_value_IG2 = cumulative_normalised_value
      )
    
    # Select all individual Unprotected by an ITN in the simulation with NGM-ITN
    
    IG2_Control <- cumulative_data_seed_allage %>%
      filter(eir == EIR, access == ACCESS, net == "IG2", cohort == "Placebo") %>%
      select(
        coverage,
        access,
        eir,
        EHT,
        reduction_coverage,
        deployment_frequency,
        seasonality,
        human_blood_index,
        cumulative_case,
        cumulative_nHost,
        cumulative_normalised_value
      ) %>%
      rename(
        coverage_IG2 = coverage,
        cumulative_case_IG2 = cumulative_case,
        cumulative_nHost_IG2 = cumulative_nHost,
        cumulative_normalised_value_IG2 = cumulative_normalised_value
      )
    
    
    # Compare Protected NGM vs\n Protected PYR
    ##########################################
    
    # Merge the data
    heat_map_protected <- PYR_ITN %>%
      left_join(
        IG2_ITN,
        by = c(
          "access",
          "eir",
          "EHT",
          "reduction_coverage",
          "deployment_frequency",
          "seasonality",
          "human_blood_index"
        )
      )
    
    heat_map_protected$comparision <- "Protected IG2 vs\n Protected PYR"
    
    #double check
    heat_map_protected$cumulative_normalised_value_IG2[heat_map_protected$coverage_IG2 ==
                                                         0.1 &
                                                         heat_map_protected$EHT == "Martin" &
                                                         heat_map_protected$human_blood_index == "High" &
                                                         heat_map_protected$seasonality == "Perennial"]
    
    cumulative_data_seed_allage$cumulative_normalised_value[cumulative_data_seed_allage$eir == EIR &
                                                              cumulative_data_seed_allage$access ==
                                                              ACCESS &
                                                              cumulative_data_seed_allage$net == "IG2" &
                                                              cumulative_data_seed_allage$cohort == "ITN" &
                                                              cumulative_data_seed_allage$coverage == 0.1 &
                                                              cumulative_data_seed_allage$EHT == "Martin" &
                                                              cumulative_data_seed_allage$human_blood_index == "High" &
                                                              cumulative_data_seed_allage$seasonality == "Perennial"]
    
    heat_map_protected$cumulative_normalised_value_PYR[heat_map_protected$coverage_PYR ==
                                                         0.1 &
                                                         heat_map_protected$EHT == "Martin" &
                                                         heat_map_protected$human_blood_index == "High" &
                                                         heat_map_protected$seasonality == "Perennial"]
    cumulative_data_seed_allage$cumulative_normalised_value[cumulative_data_seed_allage$eir == EIR &
                                                              cumulative_data_seed_allage$access ==
                                                              ACCESS &
                                                              cumulative_data_seed_allage$net == "PYR" &
                                                              cumulative_data_seed_allage$cohort == "ITN" &
                                                              cumulative_data_seed_allage$coverage == 0.1 &
                                                              cumulative_data_seed_allage$EHT == "Martin" &
                                                              cumulative_data_seed_allage$human_blood_index == "High" &
                                                              cumulative_data_seed_allage$seasonality == "Perennial"]
    
    # Estimate the efficacy
    heat_map_protected <- heat_map_protected %>%
      mutate(relative_reduction = (
        1 - (
          cumulative_normalised_value_IG2 / cumulative_normalised_value_PYR
        )
      ) * 100)
    
    # Transfom into a dataframe
    heat_map_protected <- as.data.frame(heat_map_protected)
    
    # Compare NGM Unportected vs PYR Unprotected
    ##############################################3
    
    # Merge the data
    heat_map_unprotected <- PYR_Control %>%
      left_join(
        IG2_Control,
        by = c(
          "access",
          "eir",
          "EHT",
          "reduction_coverage",
          "deployment_frequency",
          "seasonality",
          "human_blood_index"
        )
      )
    
    heat_map_unprotected$comparision <- "Unprotected IG2 vs\n Unprotected PYR"
    
    # double check
    heat_map_unprotected$cumulative_normalised_value_IG2[heat_map_unprotected$coverage_IG2 ==
                                                           0.1 &
                                                           heat_map_unprotected$EHT == "Martin" &
                                                           heat_map_unprotected$human_blood_index == "High" &
                                                           heat_map_unprotected$seasonality == "Perennial"]
    cumulative_data_seed_allage$cumulative_normalised_value[cumulative_data_seed_allage$eir == EIR &
                                                              cumulative_data_seed_allage$access ==
                                                              ACCESS &
                                                              cumulative_data_seed_allage$net == "IG2" &
                                                              cumulative_data_seed_allage$cohort == "Placebo" &
                                                              cumulative_data_seed_allage$coverage == 0.1 &
                                                              cumulative_data_seed_allage$EHT == "Martin" &
                                                              cumulative_data_seed_allage$human_blood_index == "High" &
                                                              cumulative_data_seed_allage$seasonality == "Perennial"]
    heat_map_unprotected$cumulative_normalised_value_PYR[heat_map_unprotected$coverage_PYR ==
                                                           0.1 &
                                                           heat_map_unprotected$EHT == "Martin" &
                                                           heat_map_unprotected$human_blood_index == "High" &
                                                           heat_map_unprotected$seasonality == "Perennial"]
    cumulative_data_seed_allage$cumulative_normalised_value[cumulative_data_seed_allage$eir == EIR &
                                                              cumulative_data_seed_allage$access ==
                                                              ACCESS &
                                                              cumulative_data_seed_allage$net == "PYR" &
                                                              cumulative_data_seed_allage$cohort == "Placebo" &
                                                              cumulative_data_seed_allage$coverage == 0.1 &
                                                              cumulative_data_seed_allage$EHT == "Martin" &
                                                              cumulative_data_seed_allage$human_blood_index == "High" &
                                                              cumulative_data_seed_allage$seasonality == "Perennial"]
    
    # estimate the efficacy
    heat_map_unprotected <- heat_map_unprotected %>%
      mutate(relative_reduction = (
        1 - (
          cumulative_normalised_value_IG2 / cumulative_normalised_value_PYR
        )
      ) * 100)
    
    # trasfrom into a data frame
    heat_map_unprotected <- as.data.frame(heat_map_unprotected)
    
    
    # Compare Unprotected NGM vs\n Protected PYR
    #############################################
    
    # Merge the data
    heat_map_unprotectedvsprotected <- PYR_ITN %>%
      left_join(
        IG2_Control,
        by = c(
          "access",
          "eir",
          "EHT",
          "reduction_coverage",
          "deployment_frequency",
          "seasonality",
          "human_blood_index"
        )
      )
    
    # Define the comparision
    heat_map_unprotectedvsprotected$comparision <- "Unprotected IG2 vs\n Protected PYR"
    
    
    #double check
    heat_map_unprotectedvsprotected$cumulative_normalised_value_IG2[heat_map_unprotectedvsprotected$coverage_IG2 ==
                                                                      0.1 &
                                                                      heat_map_unprotectedvsprotected$EHT == "Martin" &
                                                                      heat_map_unprotectedvsprotected$human_blood_index == "High" &
                                                                      heat_map_unprotectedvsprotected$seasonality == "Perennial"]
    cumulative_data_seed_allage$cumulative_normalised_value[cumulative_data_seed_allage$eir == EIR &
                                                              cumulative_data_seed_allage$access ==
                                                              ACCESS &
                                                              cumulative_data_seed_allage$net == "IG2" &
                                                              cumulative_data_seed_allage$cohort == "Placebo" &
                                                              cumulative_data_seed_allage$coverage == 0.1 &
                                                              cumulative_data_seed_allage$EHT == "Martin" &
                                                              cumulative_data_seed_allage$human_blood_index == "High" &
                                                              cumulative_data_seed_allage$seasonality == "Perennial"]
    heat_map_unprotectedvsprotected$cumulative_normalised_value_PYR[heat_map_unprotectedvsprotected$coverage_PYR ==
                                                                      0.1 &
                                                                      heat_map_unprotectedvsprotected$EHT == "Martin" &
                                                                      heat_map_unprotectedvsprotected$human_blood_index == "High" &
                                                                      heat_map_unprotectedvsprotected$seasonality == "Perennial"]
    cumulative_data_seed_allage$cumulative_normalised_value[cumulative_data_seed_allage$eir == EIR &
                                                              cumulative_data_seed_allage$access ==
                                                              ACCESS &
                                                              cumulative_data_seed_allage$net == "PYR" &
                                                              cumulative_data_seed_allage$cohort == "ITN" &
                                                              cumulative_data_seed_allage$coverage == 0.1 &
                                                              cumulative_data_seed_allage$EHT == "Martin" &
                                                              cumulative_data_seed_allage$human_blood_index == "High" &
                                                              cumulative_data_seed_allage$seasonality == "Perennial"]
    
    
    
    
    # estimate the efficacy
    heat_map_unprotectedvsprotected <- heat_map_unprotectedvsprotected %>%
      mutate(relative_reduction = (
        1 - (
          cumulative_normalised_value_IG2 / cumulative_normalised_value_PYR
        )
      ) * 100)
    
    # Transform into data frame
    heat_map_unprotectedvsprotected <- as.data.frame(heat_map_unprotectedvsprotected)
    
    
    # Compare Whole population NGM vs Whole population PYR
    ########################################################
    
    # Sum across cohort and normalised
    d_all <- cumulative_data_seed_allage %>%
      group_by(
        net,
        access,
        eir,
        coverage,
        EHT,
        reduction_coverage,
        deployment_frequency,
        seasonality,
        human_blood_index
      ) %>%
      summarise(
        cumulative_case  = sum(cumulative_case),
        cumulative_nHost  = sum(cumulative_nHost)
      ) %>%
      ungroup() %>%
      mutate(cumulative_normalised_value = cumulative_case / cumulative_nHost)
    
    # Select all the PYR
    PYR_all <- d_all %>%
      filter(eir == EIR, access == ACCESS, net == "PYR") %>%
      select(
        coverage,
        access,
        eir,
        EHT,
        reduction_coverage,
        deployment_frequency,
        seasonality,
        human_blood_index,
        cumulative_case,
        cumulative_nHost,
        cumulative_normalised_value
      ) %>%
      rename(
        coverage_PYR = coverage,
        cumulative_case_PYR = cumulative_case,
        cumulative_nHost_PYR = cumulative_nHost,
        cumulative_normalised_value_PYR = cumulative_normalised_value
      )
    
    # Select all the IG2
    IG2_all <- d_all %>%
      filter(eir == EIR, access == ACCESS, net == "IG2") %>%
      select(
        coverage,
        access,
        eir,
        EHT,
        reduction_coverage,
        deployment_frequency,
        seasonality,
        human_blood_index,
        cumulative_case,
        cumulative_nHost,
        cumulative_normalised_value
      ) %>%
      rename(
        coverage_IG2 = coverage,
        cumulative_case_IG2 = cumulative_case,
        cumulative_nHost_IG2 = cumulative_nHost,
        cumulative_normalised_value_IG2 = cumulative_normalised_value
      )
    
    # Merge the data
    heat_map_all <- PYR_all %>%
      left_join(
        IG2_all,
        by = c(
          "access",
          "eir",
          "EHT",
          "reduction_coverage",
          "deployment_frequency",
          "seasonality",
          "human_blood_index"
        )
      )
    
    
    # Define the comparision
    heat_map_all$comparision <- "All IG2 vs\n All PYR"
    
    
    #double check
    heat_map_all$cumulative_normalised_value_IG2[heat_map_all$coverage_IG2 ==
                                                   0.1 &
                                                   heat_map_all$EHT == "Martin" &
                                                   heat_map_all$human_blood_index == "High" &
                                                   heat_map_all$seasonality == "Perennial"]
    d_all$cumulative_normalised_value[d_all$eir == EIR &
                                        d_all$access == ACCESS &
                                        d_all$net == "IG2" &
                                        d_all$coverage == 0.1 &
                                        d_all$EHT == "Martin" &
                                        d_all$human_blood_index == "High" &
                                        d_all$seasonality == "Perennial"]
    heat_map_all$cumulative_normalised_value_PYR[heat_map_all$coverage_PYR ==
                                                   0.1 &
                                                   heat_map_all$EHT == "Martin" &
                                                   heat_map_all$human_blood_index == "High" &
                                                   heat_map_all$seasonality == "Perennial"]
    d_all$cumulative_normalised_value[d_all$eir == EIR &
                                        d_all$access == ACCESS &
                                        d_all$net == "PYR" &
                                        d_all$coverage == 0.1 &
                                        d_all$EHT == "Martin" &
                                        d_all$human_blood_index == "High" &
                                        d_all$seasonality == "Perennial"]
    
    
    
    
    # estimate the efficacy
    heat_map_all <- heat_map_all %>%
      mutate(relative_reduction = (
        1 - (
          cumulative_normalised_value_IG2 / cumulative_normalised_value_PYR
        )
      ) * 100)
    
    
    # Transform into data frame
    heat_map_all <- as.data.frame(heat_map_all)
    
    # Save the data
    ###############
    
    # Merge all dataset
    heat_map_combined <- rbind(
      heat_map_all,
      heat_map_protected,
      heat_map_unprotected,
      heat_map_unprotectedvsprotected
    )
    
    # save
    summary_file <- paste0("/Output_summary_heatmap", EIR, ACCESS, ".txt")
    summary_path <- paste0(outdir_experiment, summary_file)
    write.table(
      heat_map_combined,
      file = summary_path,
      sep = ";",
      col.names = TRUE,
      row.names = FALSE,
      quote = TRUE
    )
    
  }
}
