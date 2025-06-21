# # Set global constants
# C <- list(
#   alpha_1 = 0.5,
#   alpha_2 = 0.7,
#   t_0 = 200
# )

# Set simulation levels
if (cfg$run_sims && Sys.getenv("sim_run") %in% c("first", "")) {
  
  level_sets <- list()
  
  # Estimation: three models
  # Figures: fig_999
  level_sets[["estimation_1"]] <- list(
    model = c("HH", "HG", "P-ITS")
  )
  
  level_set <- level_sets[[cfg$sim_level_set]]
  
  # if (cfg$sim_level_set=="asdf") { cfg$keep = c(1:3,7:9,16:18,22:24) }
  
}
