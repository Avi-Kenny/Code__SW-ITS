#' Generate one stepped wedge dataset
#'
#' @param data_type One of c("normal", "binomial"). Type of outcome.
#' @param n_sequences Integer. Number of sequences
#' @param n_clust_per_seq Integer. Number of clusters per sequence
#' @param n_ind_per_cell Integer. Number of individuals per cluster - time
#'     period cell (K)
#' @param delta Numeric; Scalar immediate treatment effect
#' @param sigma Numeric; SD of the outcome (ignored if data_type=="binomial")
#' @param tau Numeric. SD of the cluster random effect
#' @param eta Numeric. SD of the random time slope
#' @param re One of c("cluster", "cluster+time"); whether a cluster intercept
#'     should be included ("cluster") or a cluster intercept plus a
#'     cluster-period intercept ("cluster+time")
#' @return A dataframe representing a stepped wedge dataset
generate_dataset <- function(
    data_type, n_sequences, n_clust_per_seq, n_ind_per_cell, delta, sigma, tau,
    eta, re
) {
  
  # Misc
  passed_args <- as.list(environment())
  I <- round(n_sequences * n_clust_per_seq)
  J <- round(n_sequences + 1)
  K <- n_ind_per_cell
  i_vec <- j_vec <- k_vec <- y_ij_vec <- x_ij_vec <- rep(NA, I*J*K)
  
  # Setup
  clusters <- c(1:I)
  crossover <- rep(c(2:J), each=round(I/(J-1)))
  crossover <- sample(crossover)
  pointer <- 1
  
  # Generate data
  for (i in clusters) {

    # Cluster random intercept
    if (re=="cluster") {
      alpha_i <- rnorm(n=1, mean=0, sd=tau)
    } else if (re=="cluster+time") {
      alpha_i <- rnorm(n=1, mean=0, sd=tau/sqrt(2))
    }
    
    # Random pre/post slopes
    beta <- 0.5 # !!!!!
    beta_i <- rnorm(n=1, mean=0, sd=eta)

    for (j in c(1:J)) {

      x_ij <- In(j>=crossover[i])

      if (re=="cluster") {
        gamma_ij <- 0
      } else if (re=="cluster+time") {
        gamma_ij <- rnorm(n=1, mean=0, sd=tau/sqrt(2))
      }
      
      # Linear predictor
      lin_ij <- alpha_i + (beta+beta_i)*j + gamma_ij + delta*x_ij

      # Sample outcome values
      if (data_type=="normal") {
        y_ij <- rnorm(n=K, mean=lin_ij, sd=sigma)
      } else if (data_type=="binomial") {
        y_ij <- rbinom(n=K, size=1, prob=expit(lin_ij))
      }

      inds <- c(pointer:(pointer+(K-1)))
      i_vec[inds] <- rep(i, K)
      j_vec[inds] <- rep(j, K)
      k_vec[inds] <- c(1:K)
      y_ij_vec[inds] <- y_ij
      x_ij_vec[inds] <- rep(x_ij, K)
      pointer <- pointer + K

    }

  }
  
  dat <- data.frame(
    "i" = i_vec,
    "j" = j_vec,
    "k" = k_vec,
    "y_ij" = y_ij_vec,
    "x_ij" = x_ij_vec
  )
  dat$ij <- as.numeric(factor(paste0(i_vec, "-", j_vec)))
  
  attr(dat, "params") <- passed_args
  
  return (dat)
  
}
