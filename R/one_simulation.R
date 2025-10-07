######################.
##### Estimation #####
######################.

if (cfg$sim_which=="estimation") {
  
  #' Run a single simulation
  #'
  #' @return A list with ...
  one_simulation <- function() {
    
    # Generate data
    # L <- list(time="continuous", n_sequences=6, n_clust_per_seq=2, n_ind_per_cell=20)
    dat <- generate_data(
      data_type = "normal",
      time = L$time,
      n_sequences = L$n_sequences,
      n_clust_per_seq = L$n_clust_per_seq,
      n_ind_per_cell = L$n_ind_per_cell,
      delta = 1,
      # sigma = 0.3,
      # tau = 0.1,
      # eta = 0.5,
      sigma = 1,
      tau = 0.25,
      eta = 0.75,
      # eta2 = 0.5,
      re = "cluster"
    )
    
    # For data checking
    if (F) {
      ggplot(dat, aes(x=j, y=y_ij, color=factor(x_ij))) +
        geom_point(alpha=0.5) +
        facet_wrap(~factor(i), ncol=3)
    }
    
    # Calculated columns
    if (L$model %in% c("HH", "NE")) {
      dat$j <- ceiling(dat$j)
      dat$ij <- as.numeric(factor(paste0(dat$i, "-", dat$j)))
    }
    
    # Analyze data
    if (L$model=="HH") {
      model <- lme4::lmer(
        y_ij ~ factor(j) - 1 + (1|i) + x_ij,
        data = dat
      )
    } else if (L$model=="NE") {
      model <- lme4::lmer(
        y_ij ~ factor(j) - 1 + (1|i) + (1|ij) + x_ij,
        data = dat
      )
    } else if (L$model=="P-ITS") {
      dat %<>% dplyr::mutate("time"=j, "time2"=j2, "outcome"=y_ij, "tx"=x_ij)
      model <- steppedwedge::analyze_pits(dat, rte=F)
    }
    
    # Return results
    return(list(
      tx_est = summary(model)$coefficients["x_ij", "Estimate"],
      tx_se = summary(model)$coefficients["x_ij", "Std. Error"]
    ))
    
  }
  
}
