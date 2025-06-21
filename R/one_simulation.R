######################.
##### Estimation #####
######################.

if (cfg$sim_which=="estimation") {
  
  #' Run a single simulation
  #'
  #' @return A list with ...
  one_simulation <- function() {
    
    # Generate data
    dat <- generate_dataset(
      data_type = "normal",
      n_sequences = 6,
      n_clust_per_seq = 1,
      n_ind_per_cell = 10,
      delta = 1,
      sigma = 0.5,
      tau = 0.5,
      eta = 0.5,
      re = "cluster"
    )
    
    # # For data checking
    # if (T) {
    #   ggplot(dat, aes(x=j, y=y_ij, color=factor(x_ij))) +
    #     geom_point(alpha=0.5) +
    #     facet_wrap(~factor(i), ncol=3)
    # }
    
    # Analyze data
    if (L$model=="HH") {
      model <- lme4::lmer(
        y_ij ~ factor(j) - 1 + (1|i) + x_ij,
        data = dat
      )
    } else if (L$model=="HG") {
      model <- lme4::lmer(
        y_ij ~ factor(j) - 1 + (1|i) + (1|ij) + x_ij,
        data = dat
      )
    } else if (L$model=="P-ITS") {
      model <- lme4::lmer(
        y_ij ~ j + (j|i) + (1|i) + x_ij,
        data = dat
      )
    }
    tx_est <- summary(model)$coefficients["x_ij", "Estimate"]
    
    # Return results
    return(list(
      tx_est = tx_est
    ))
    
  }
  
}
