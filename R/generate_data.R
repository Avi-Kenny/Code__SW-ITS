#' Generate one stepped wedge dataset
#'
#' @param data_type One of c("normal", "binomial"). Type of outcome.
#' @param time One of c("discrete", "continuous"). Whether time is rounded.
#' @param n_sequences Integer. Number of sequences
#' @param n_clust_per_seq Integer. Number of clusters per sequence
#' @param n_ind_per_cell Integer. Number of individuals per cluster - time
#'     period cell (K)
#' @param delta Numeric; Scalar immediate treatment effect
#' @param alpha Numeric; average outcome value
#' @param beta Numeric; average linear time effect
#' @param sigma Numeric; SD of the outcome (ignored if data_type=="binomial")
#' @param tau Numeric. SD of the cluster random effect
#' @param eta Numeric. SD of the random time slope
#' @param eta2 Numeric. SD of the random time slope change
#' @param re One of c("cluster", "cluster+time"); whether a cluster intercept
#'     should be included ("cluster") or a cluster intercept plus a
#'     cluster-period intercept ("cluster+time")
#' @return A dataframe representing a stepped wedge dataset
generate_data <- function(
    data_type, time, n_sequences, n_clust_per_seq, n_ind_per_cell, delta,
    alpha=0, beta=0, sigma, tau, eta, eta2=0, re
) {
  
  # Misc
  passed_args <- as.list(environment())
  I <- round(n_sequences * n_clust_per_seq)
  J <- round(n_sequences + 1)
  K <- n_ind_per_cell
  i_vec <- j_vec <- j2_vec <- k_vec <- y_ij_vec <- x_ij_vec <- rep(NA, I*J*K)
  
  # Setup
  clusters <- c(1:I)
  crossover <- rep(c(1:(J-1)), each=round(I/(J-1)))
  pointer <- 1
  
  # Generate data
  for (i in clusters) {

    # Cluster random intercept
    if (re=="cluster") {
      alpha_i <- rnorm(n=1, mean=0, sd=tau)
    } else if (re=="cluster+time") {
      alpha_i <- rnorm(n=1, mean=0, sd=tau/sqrt(2))
    }
    
    # Random pre slopes
    beta_i <- rnorm(n=1, mean=0, sd=eta)
    if (eta2!=0) {
      beta2_i <- rnorm(n=1, mean=0, sd=eta2)
    } else {
      beta2_i <- 0
    }
    
    for (j in c(1:J)) {

      if (re=="cluster") {
        gamma_ij <- 0
      } else if (re=="cluster+time") {
        gamma_ij <- rnorm(n=1, mean=0, sd=tau/sqrt(2))
      }
      
      if (time=="continuous") {
        j_k <- j - runif(K)
      } else if (time=="discrete") {
        j_k <- rep(j,K)
      }
      x_ij <- In(j_k>crossover[i])
      j_k2 <- x_ij*(j_k-crossover[i])
      
      # Linear predictor
      lin_ij <- alpha + alpha_i + (beta+beta_i)*j_k + gamma_ij + delta*x_ij +
        beta2_i*x_ij*(j_k-crossover[i])
      
      # Sample outcome values
      if (data_type=="normal") {
        y_ij <- rnorm(n=K, mean=lin_ij, sd=sigma)
      } else if (data_type=="binomial") {
        y_ij <- rbinom(n=K, size=1, prob=expit(lin_ij))
      }
      
      inds <- c(pointer:(pointer+(K-1)))
      i_vec[inds] <- rep(i, K)
      j_vec[inds] <- j_k
      j2_vec[inds] <- j_k2
      k_vec[inds] <- c(1:K)
      y_ij_vec[inds] <- y_ij
      x_ij_vec[inds] <- x_ij
      pointer <- pointer + K
      
    }

  }
  
  dat <- data.frame(
    "i" = i_vec,
    "j" = j_vec,
    "j2" = j2_vec,
    "k" = k_vec,
    "y_ij" = y_ij_vec,
    "x_ij" = x_ij_vec
  )
  
  attr(dat, "params") <- passed_args
  
  return (dat)
  
}
