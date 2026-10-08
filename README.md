# itn_simulation

This repository contains the analysis code accompanying the manuscript:
Trade-offs in deploying chlorfenapyr-pyrethroid insecticide-treated nets to reduce malaria burden: a modelling study
Authors: Thiery Masserey¹ ², Swapnoleena Sen¹ ², Neil Hobbs³, Clara Champagne¹ ², Thomas A. Smith¹ ², Nakul Chitnis¹ ²
¹ Swiss Tropical and Public Health Institute (Swiss TPH), Allschwil, Switzerland
² University of Basel, Basel, Switzerland
³ Liverpool School of Tropical Medicine, Liverpool, United Kingdom
Correspondence: Dr Thiery Masserey (thiery.masserey@swisstph.ch)

---

# Overview
This study used OpenMalaria, an individual-based model of malaria epidemiology and transmission developed by Swiss TPH.
OpenMalaria documentation is available at:
https://github.com/SwissTPH/openmalaria/wiki

The repository contains the code used for two complementary analyses.

Analysis 1
We compared the public health impact of deploying:
* conventional pyrethroid-only insecticide-treated nets (PYR-ITNs), and
* chlorfenapyr-pyrethroid insecticide-treated nets (CFP-PYR-ITNs) under different deployment strategies. Because CFP-PYR-ITNs are more expensive than PYR-ITNs, we evaluated scenarios in which the increased unit cost resulted in:
  * no compromise (assuming a higher budget),
  * a 25% or 50% reduction in coverage relative to PYR-ITNs.
  * a 25% or 50% reduced deployment frequency (every 4 or 6 years) relative to PYR-ITNs (every3 years).

Analysis 2
We quantified the change in malaria burden among individuals who might lose access to insecticide-treated nets under the reduced-coverage CFP-PYR-ITN scenarios.

---

# Structure of repository
This folder contains the scripts used to generate the OpenMalaria simulations.

* launch.R:
  * select which analysis to run (Analysis 1 or Analysis 2),
  * specify the ITN deployment strategy,
  * define the parameter values explored,
  * launch OpenMalaria simulations,
  * extract and organise simulation outputs.
* Scaffold/: contains the XML scenario files used as inputs for OpenMalaria. These files define the intervention scenarios and model parameter values.
* Run.R and Job.sh: Scripts used to submit and manage OpenMalaria simulations on the SciCORE high-performance computing (HPC) cluster.
* post_processing_analysis_1.R: Processes simulation outputs from Analysis 1.
* post_processing_analysis_2.R: Processes simulation outputs from Analysis 2.

---

# Results and visualisation

The processed simulation outputs and the scripts used to generate the figures presented in the manuscript are provided in the corresponding data and visualization repository at : X

---

# Notes

The code was developed using the directory structure and file paths of the original research environment. Users will need to modify the file paths and working directories to match their local computing environment before running the scripts.
