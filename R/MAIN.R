# Main config
cfg <- list(
  run_sims = T,
  run_analysis = F,
  run_process = F,
  run_ppt_figures = F,
  sim_which = "estimation",
  sim_level_set = "estimation_1",
  sim_run_or_update = "run",
  sim_num = 100,
  sim_parallel = F,
  sim_n_cores = 1, # 500
  sim_stop_at_error = F
)

# Secondary config
source("R/config.R", local=T)

# Load SimEngine + functions
{
  library(SimEngine)
  source("R/generate_data.R", local=T)
  source("R/misc_functions.R", local=T)
  source("R/one_simulation.R", local=T)
}

# Set level sets
source("R/levels.R", local=T)

# Run simulation
if (cfg$run_sims) { source("R/run.R", local=T) }

# Run analysis
if (cfg$run_analysis) { source("R/analysis.R", local=T) }

# Tables and figures
if (cfg$run_process) {
  source("R/process_sims.R", local=T)
  source("R/process.R", local=T)
}

# PPT figures
if (cfg$run_ppt_figures) {
  source("R/ppt_figures.R", local=T)
}
