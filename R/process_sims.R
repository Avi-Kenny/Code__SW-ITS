#####################.
##### VIZ: Sims #####
#####################.

# Figures produced: fig_123

sim <- readRDS("SimEngine.out/estimation_1_20250621.rds")

summ <- sim %>% SimEngine::summarize(
  list(stat="mean", x="tx_est"),
  list(stat="sd", x="tx_est")
) %>%
  dplyr::mutate(
    n_clust_per_seq = paste0(n_clust_per_seq, " clust/seq")
  )
# print(summ)

ggplot(summ, aes(x=n_sequences, y=sd_tx_est, color=model)) +
  geom_line() +
  geom_point() +
  facet_grid(
    cols = dplyr::vars(n_clust_per_seq)
    # rows = dplyr::vars(time)
  ) +
  labs(
    x = "Number of sequences",
    y = "SD of treatment effect estimator",
    color = "Model"
  )

# Summary stats
sim %>% SimEngine::summarize(
  list(stat="mean", x="tx_est"),
  list(stat="sd", x="tx_est"),
  list(stat="coverage", estimate="tx_est", se="tx_se", truth=1, name="cov")
)

# Univariate estimate plot
if (F) {
  plot <- ggplot(sim$results, aes(x=tx_est, y=0, color=model)) +
    geom_jitter(width=0, height=1, alpha=0.3, size=3) +
    facet_wrap(~model, ncol=1, strip.position="left") +
    labs(y=NULL, x="Treatment effect estimate") +
    ylim(-2,2) +
    theme(axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          strip.text.y.left = element_text(angle=0),
          legend.position="none")
  print(plot)
  ggsave(filename="fig_1.pdf", plot=plot, device="pdf", width=6, height=4)
}
