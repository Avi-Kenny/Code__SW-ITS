# Set simulation levels
if (cfg$run_sims && Sys.getenv("sim_run") %in% c("first", "")) {
  
  level_sets <- list()
  
  # Estimation: two models
  # Figures: fig_999
  level_sets[["estimation_1"]] <- list(
    # model = c("NE", "P-ITS"),
    model = c("HH", "NE", "P-ITS"),
    time = "continuous",
    # time = c("continuous", "discrete"),
    n_sequences = 6,
    # n_sequences = c(2:10), # 6
    n_clust_per_seq = 2,
    # n_clust_per_seq = c(1,2,5), # 1
    n_ind_per_cell = c(20,200)
  )
  
  level_set <- level_sets[[cfg$sim_level_set]]
  
}
