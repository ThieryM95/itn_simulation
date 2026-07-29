#################################################################################
# Workflow to run analysis comparing PYR-ITN vs NGM-ITN in OpenMalaria          #
#                                                                               #
# Two different analysis can be run:                                            #
#                                                                               #
# 1) Compare the impact of PYR-ITNs vs NGM-ITNs without compromise              #
#    or with reduced (25% or 50%) coverage or deployment frequency              #
#                                                                               #
# 2) Quantify changes in malaria burden among individuals who would potentially #
#    lose access to ITNs under the reduced-coverage scenarios                   #
#                                                                               #
# Run on OpenMalaria V48.0                                                      #
#                                                                               #
# Author: thiery Masserey (thiery.masserey@swisstph.ch) adapted from Aurélien   #
#         Aurélien Cavelan (aurelien.cavelan@swisstph.ch)                       #
#################################################################################


# Clear global environment
rm(list = ls())

# Define working directory
setwd("/scicore/home/penny/masthi00/itn_simulation")

# Load packages
source("pacman.R")
source("run.R")
source("extract.R")

# SciCORE parameters:
sciCORE = list(
  use = TRUE,
  account = "penny",
  jobName = "OpenMalaria",
  qos = "30min",
  time = "00:30:00",
  cpus_per_task = 16,
  # number of CPUs per job
  batch_size = 16 # number of OM instances per job
  # number of job in array = N / batch_size
  # if batch_size = cpus_per_task then one OM instance = one CPU = faster
  # if batch_size > cpus_per_task then multiple OM instances per cpus = slower but less Slurm jobs
  # just leave it 16 / 16, 32 / 32, 64 / 64
  # if more than 500k jobs then 64 / 128 or 64 / 256
)

# Version of OpenMalaria
om = list(version = 48, path = "/scicore/home/penny/GROUP/OpenMalaria/OM_schema48")

# run scenarios, extract the data, or both
do = list(run = TRUE,
          extract = TRUE,
          example = TRUE)


# Select the analysis
Analysis_set <- 1  # can be 1: impact of  PYR-ITNs vs NGM-ITNs without compromise or with reduced (25% or 50%) coverage or deployment frequency in the whole population
                   #     or 2: impact of  PYR-ITNs vs NGM-ITNs with reduced coverage on ITN user and non User over 3 years

if (Analysis_set==1){
# For analysis 1, define if simulate PYR-ITNs, NGM-ITNs with reduced coverage, or with reduced deployment frequency
ITN_strategy <- "PYR-ITNs" # can be  "PYR-ITNs" or  "NGM-ITNs coverage reduction" or "NGM-ITNs frequency reduction" 
}
  
############################################################
# Define parameter and scaffold for experience we want run #
############################################################

# Analysis 1: impact of  PYR-ITNs vs NGM-ITNs without compromise or with reduced (25% or 50%) coverage or deployment frequency in the whole population
#######################################################################################################################################################

if (Analysis_set==1 & ITN_strategy=="PYR-ITNs"){

# Varying parameters (for simulations with PYR-ITN for 12 years)
seeds = 10 # number of stochastic realizations
eirs = c(5, 10, 15, 40, 100) # Anual entomological inoculation rate per person
accesses =  c(0.04, 0.20)    # level of acess to treatment%
coverages = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)  # ITN coverage %
reduction_coverages = c(0)  # relative reduction in coverage
deployment_frequencies <- c(3) # frequency of deployment (years)

Human_blood_indexes<-c("High", "Low") # Human blood index
Indoor<-c(0.99,0.5)
Outdoor<-c(0.99,0.1)
HBI<-data_frame(Human_blood_indexes,Indoor,Outdoor)

Seasonalities<-c("Perennial", "Seasonal") # Fourier coefficient for seasonality pattern
a1<-c(0,-1.0342562993367512)
b1<-c(0,0.59712815284729)
a2<-c(0,-0.15999998648961386)
b2<-c(0,0.27712810039520264)
Fourier<-data_frame(Seasonalities,a1,b1,a2,b2)

# Fixed parameters for scaffold
pop_size = 10000 # number of humans
start_year = 2000 # start of the monitoring period
end_year = 2014 # end of the monitoring period (2005 for Coverage, 2016 for frequnecy)
burn_in = start_year - 50 # additional burn in time

# Scaffold of simulations
scaffolds = list(
  "scaffolds/default_12years.xml"
)

# ITN parameters xml
nets = c("itn1_martin_12years","itn1_nguessan_12years","itn1_nguessan_V2_12years") # define ITN simulated (here need do one by one)

# Name of the experiment
experiment = 'output_PYR_12years_EPI' # name of the experiment folder
outdir_experiment = paste0("/scicore/home/penny/masthi00/OUT_itn_simulation/",
                           experiment)

}

if (Analysis_set==1 & ITN_strategy=="NGM-ITNs coverage reduction"){
  
  # Varying parameters (for simulations with reduce NGMT-ITNs coverage for 12 years)
  
  seeds = 10 # number of stochastic realizations
  eirs = c(5, 10, 15, 40, 100) # Anual entomological inoculation rate per person
  accesses =  c(0.04, 0.20)    # level of acess to treatment%
  coverages = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)  # ITN coverage %
  reduction_coverages = c(0, 0.25, 0.5)  # relative reduction in coverage
  deployment_frequencies <- c(3) # frequency of deployment (years)
  
  Human_blood_indexes<-c("High", "Low") # Human blood index
  Indoor<-c(0.99,0.5)
  Outdoor<-c(0.99,0.1)
  HBI<-data_frame(Human_blood_indexes,Indoor,Outdoor)
  
  Seasonalities<-c("Perennial", "Seasonal") # Fourier coefficient for seasonality pattern
  a1<-c(0,-1.0342562993367512)
  b1<-c(0,0.59712815284729)
  a2<-c(0,-0.15999998648961386)
  b2<-c(0,0.27712810039520264)
  Fourier<-data_frame(Seasonalities,a1,b1,a2,b2)
  
  # Fixed parameters for scaffold
  pop_size = 10000 # number of humans
  start_year = 2000 # start of the monitoring period
  end_year = 2014 # end of the monitoring period (2005 for Coverage, 2016 for frequnecy)
  burn_in = start_year - 50 # additional burn in time
  
  # Scaffold of simulations
  scaffolds = list(
    "scaffolds/default_12years.xml"
  )
  # Scaffold with ITN parameters
  nets = c("itn2_martin_12years","itn2_nguessan_12years","itn2_nguessan_V2_12years") # define ITN simulated (here need do one by one)
  
  # Define the name of the experiment
  experiment = 'output_IG2_12years_coverage_EPI' # name of the experiment folder
  outdir_experiment = paste0("/scicore/home/penny/masthi00/OUT_itn_simulation/",
                             experiment)
}

if (Analysis_set==1 & ITN_strategy=="NGM-ITNs frequency reduction"){
  
  # Varying parameters (for simulations with reduced NGM-ITNs frequency for 12 years)
  
  seeds = 10 # number of stochastic realizations
  eirs = c(5, 10, 15, 40, 100) # Anual entomological inoculation rate per person
  accesses =  c(0.04, 0.20)    # level of acess to treatment%
  coverages = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)  # ITN coverage %
  reduction_coverages = c(0)  # relative reduction in coverage
  deployment_frequencies <- c(3, 4, 6) # frequency of deployment (years)
  
  Human_blood_indexes<-c("High", "Low") # Human blood index
  Indoor<-c(0.99,0.5)
  Outdoor<-c(0.99,0.1)
  HBI<-data_frame(Human_blood_indexes,Indoor,Outdoor)
  
  Seasonalities<-c("Perennial", "Seasonal") # Fourier coefficient for seasonality pattern
  a1<-c(0,-1.0342562993367512)
  b1<-c(0,0.59712815284729)
  a2<-c(0,-0.15999998648961386)
  b2<-c(0,0.27712810039520264)
  Fourier<-data_frame(Seasonalities,a1,b1,a2,b2)
  
  # Fixed parameters for scaffold
  pop_size = 10000 # number of humans
  start_year = 2000 # start of the monitoring period
  end_year = 2014 # end of the monitoring period (2005 for Coverage, 2016 for frequnecy)
  burn_in = start_year - 50 # additional burn in time
  
  # Scaffold of simulations
  scaffolds = list(
    "scaffolds/default_12years.xml"
  )

  # Scaffold with ITN parameters
  nets = c("itn2_martin_12years","itn2_nguessan_12years","itn2_nguessan_V2_12years") # define ITN simulated (here need do one by one)
  
  # Define the name of the experiment
  experiment = 'output_IG2_12years_frequency_EPI' # name of the experiment folder
  outdir_experiment = paste0("/scicore/home/penny/masthi00/OUT_itn_simulation/",
                             experiment)
}

# Analysis 2: impact of  PYR-ITNs vs NGM-ITNs with reduced coverage on ITN user and non User over 3 years
##########################################################################################################

if (Analysis_set==2){

# Varying parameters
seeds = 10 # number of stochastic realizations
eirs = c(5, 10, 15, 40, 100) # entomological innoculation rates
accesses = c(0.04, 0.20) # level of acess to treatment (%)
coverages = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9) # coverage (%)
deployment_frequencies <- c(3) # deployment frequnecy (here asusmed 3, but stop simulation after 1 deployemnt)
reduction_coverages = c(0) # here assumed zero just run all coverage in both case

Human_blood_indexes <- c("High", "Low") # human blood index
Indoor <- c(0.99, 0.5)
Outdoor <- c(0.99, 0.1)
HBI <- data_frame(Human_blood_indexes, Indoor, Outdoor)

Seasonalities <- c("Perennial", "Seasonal") # fourier coefficient for seasonality pattern
a1 <- c(0, -1.0342562993367512)
b1 <- c(0, 0.59712815284729)
a2 <- c(0, -0.15999998648961386)
b2 <- c(0, 0.27712810039520264)
Fourier <- data_frame(Seasonalities, a1, b1, a2, b2)

# Fixed parameters for scaffold
pop_size = 10000 # number of humans
start_year = 2000 # start of the monitoring period
end_year = 2005 # end of the monitoring period (2005 for Coverage, 2016 for frequnecy)
burn_in = start_year - 50 # additional burn in time

# Scaffold
scaffolds = list("scaffolds/default_placebo.xml")

# ITN scaffold
nets = c(
  "itn1_martin_placebo",
  "itn2_martin_placebo",
  "itn1_nguessan_placebo",
  "itn2_nguessan_placebo",
  "itn1_nguessan_V2_placebo",
  "itn2_nguessan_V2_placebo"
)

# Define the name of the experiment
experiment = 'output_Final_All_2' # name of the experiment folder
outdir_experiment = paste0("/scicore/home/penny/masthi00/OUT_itn_simulation/",
                           experiment)

}


#######################
# Run the simulations #
#######################

# Function to create scenarios
create_scenarios <- function()
{
  index = 1
  scenarios = list()
  for (scaffold in scaffolds)
  {
    # Replaced fixed parameters
    xml = readLines(paste0(scaffold))
    xml = gsub(pattern = "@version@",
               replace = om$version,
               x = xml)
    xml = gsub(pattern = "@pop_size@",
               replace = pop_size,
               x = xml)
    xml = gsub(pattern = "@burn_in@",
               replace = burn_in,
               x = xml)
    xml = gsub(pattern = "@start_year@",
               replace = start_year,
               x = xml)
    xml = gsub(pattern = "@end_year@",
               replace = end_year,
               x = xml)
    
    # Loop trough each varying parameters (full factorial analysis)
    for (eir in eirs)
    {
      for (access in accesses)
      {
        for (net in nets)
        {
          for (coverage in coverages)
          {
            for (reduction_coverage in reduction_coverages)
            {
              for (deployment_frequency in deployment_frequencies)
              {
                for (human_blood_index in Human_blood_indexes)
                {
                  for (seasonality in Seasonalities)
                  {
                    for (seed in 1:seeds)
                    {
                      scenario = xml
                      scenario = gsub(
                        pattern = "@seed@",
                        replace = seed,
                        x = scenario
                      )
                      scenario = gsub(
                        pattern = "@eir@",
                        replace = eir,
                        x = scenario
                      )
                      scenario = gsub(
                        pattern = "@access@",
                        replace = access,
                        x = scenario
                      )
                      
                      # use the right net snippet
                      net_xml = readLines(paste0("scaffolds/", net, ".xml"))
                      net_xml <- paste(net_xml, collapse = "\n")
                      scenario = gsub(
                        pattern = "@INTERVENTIONS@",
                        replace = net_xml,
                        x = scenario
                      )
                      
                      scenario = gsub(
                        pattern = "@coverage@",
                        replace = coverage * (1 - reduction_coverage),
                        x = scenario
                      )
                      scenario = gsub(
                        pattern = "@deployment_frequency@",
                        replace = deployment_frequency,
                        x = scenario
                      )
                      
                      
                      scenario = gsub(
                        pattern = "@HBI_indoor@",
                        replace = HBI$Indoor[HBI$Human_blood_indexes == human_blood_index],
                        x = scenario
                      )
                      scenario = gsub(
                        pattern = "@HBI_outdoor@",
                        replace = HBI$Outdoor[HBI$Human_blood_indexes == human_blood_index],
                        x = scenario
                      )
                      
                      
                      scenario = gsub(
                        pattern = "@a1@",
                        replace = Fourier$a1[Fourier$Seasonalities == seasonality],
                        x = scenario
                      )
                      scenario = gsub(
                        pattern = "@b1@",
                        replace = Fourier$b1[Fourier$Seasonalities == seasonality],
                        x = scenario
                      )
                      scenario = gsub(
                        pattern = "@a2@",
                        replace = Fourier$a2[Fourier$Seasonalities == seasonality],
                        x = scenario
                      )
                      scenario = gsub(
                        pattern = "@b2@",
                        replace = Fourier$b2[Fourier$Seasonalities == seasonality],
                        x = scenario
                      )
                      
                      
                      # write xml
                      writeLines(scenario,
                                 con = paste0(
                                   outdir_experiment,
                                   "/xml/",
                                   index,
                                   ".xml"
                                 ))
                      
                      # Add the scenario to the list, only the 'index' field is mandatory, see example at the end
                      scenario_metadata = list(
                        scaffoldName = scaffold,
                        net = net,
                        access = access,
                        eir = eir,
                        coverage = coverage,
                        reduction_coverage = reduction_coverage,
                        deployment_frequency = deployment_frequency,
                        seasonality = seasonality,
                        human_blood_index = human_blood_index,
                        seed = seed,
                        index = index
                      )
                      scenarios = append(scenarios, list(scenario_metadata))
                      
                      index = index + 1
                      
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  return(scenarios)
}

# Run the analysis
if (do$run == TRUE)
{
  message("Cleaning Tree...")
  unlink(outdir_experiment, recursive = TRUE)
  
  # creat the output directory
  dir.create(outdir_experiment)
  dir.create(paste0(outdir_experiment, "/xml"))
  dir.create(paste0(outdir_experiment, "/txt"))
  dir.create(paste0(outdir_experiment, "/fig"))
  dir.create(paste0(outdir_experiment, "/log"))
  
  # create the scenarios
  message("Creating scenarios...")
  scenarios = create_scenarios()
  fwrite(rbindlist(scenarios),
         paste0(outdir_experiment, "/scenarios.csv"))
  
  # Run the scenarios
  message("Running scenarios...")
  run_scenarios(scenarios, outdir_experiment, om, sciCORE)
}

# Extract the output and putt it all in one dataset
if (do$extract == TRUE)
{
  message("Extracting results...")
  unlink(paste0(outdir_experiment, "/output.csv"))
  scenarios = fread(paste0(outdir_experiment, "/scenarios.csv"))
  
  start.time <- Sys.time()
  df = to_df(scenarios, outdir_experiment)
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  message("Extract time: ", time.taken)
  
  if (nrow(df) == 0) {
    message("Error: extraction failed, output dataframe is empty")
    message("       output.csv not saved")
  }
  else {
    start.time <- Sys.time()
    fwrite(df, paste0(outdir_experiment, "/output.csv"))
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    message("Write time: ", time.taken)
  }
}

# To analysis the results see:
# 1) Analysis 1: post_processing_analysis_1
# 2) Analysis 2: post_processing_analysis_2
