# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c(
    "tidyverse",
    "here",
    "metafor",
    "brms",
    "marginaleffects",
    "tidybayes",
    "patchwork"
  ), # Packages that your targets need for their tasks.
  memory = "transient",
  format = "qs",
  garbage_collection = TRUE,
  storage = "worker",
  retrieval = "worker"
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source("R/functions/.")
# tar_source("other_functions.R") # Source other scripts as needed.

# Replace the target list below with your own:
list(
  
  #### Miscellaneous ----
  
  #### Reading and preparing data ----
  tar_target(
    pilot_data_file,
    here("data", "initial_pilot_data.csv"),
    format = "file"
  ),
  
  tar_target(
    pilot_data,
    read_prep_pilot_data(pilot_data_file)
  )
  
)
