#####################.
##### VIZ: Sims #####
#####################.

# Figures produced: fig_123

# sim <- readRDS("SimEngine.out/estimation_1_20231112.rds")

summ <- sim %>% SimEngine::summarize(
  list(stat="mean", x="tx_est"),
  list(stat="sd", x="tx_est")
)
print(summ)
#   level_id model n_reps mean_tx_est  sd_tx_est
# 1        1    HH    100   1.0013735 0.85035578
# 2        2    HG    100   1.0642440 0.94297954
# 3        3 P-ITS    100   0.9990988 0.01995598

plot <- ggplot(sim$results, aes(x=tx_est, y=0, color=model)) +
  geom_jitter(width=0, height=1, alpha=0.3, size=3) +
  facet_wrap(~model, ncol=1, strip.position="left") +
  labs(y=NULL) +
  ylim(-2,2) +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        strip.text.y.left = element_text(angle=0),
        legend.position="none")
# print(plot)
# ggsave(filename="fig_1.pdf", plot=plot, device="pdf", width=6, height=4)
