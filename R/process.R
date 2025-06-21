############################.
##### VIZ: Scatterplot #####
############################.

# Figures produced: fig_1, fig_2

# Template code
# sim <- readRDS("SimEngine.out/estimation_1_20231112.rds")
# plot_data <- subset(sim$results, select=c("beta", "est", "se"))
# plot <- ggplot(plot_data, aes(x=beta, y=est)) + geom_point(alpha=0.1)
# ggsave(filename="fig_1.pdf", plot=plot, device="pdf", width=6, height=4)

# Figures produced: fig_123

#################################.
##### Plot 1: two sequences #####
#################################.

n <- 200
x_grid <- runif(n)
y_grid_1 <- x_grid + rnorm(n, sd=0.1)
y_grid_2 <- 0.5 + rnorm(n, sd=0.1)

df_plot <- data.frame(
  x = rep(x_grid, 4),
  y = c(rep(y_grid_1, 2), rep(y_grid_2, 2)),
  tx = c(rep(In(x_grid>0.5), 2), rep(0, 2*n)),
  design = rep(rep(c("SW-CRT", "ITS"), each=n), 2),
  sequence = rep(c("Sequence 1","Sequence 2"), each=2*n)
)

ggplot(df_plot, aes(x=x, y=y, color=factor(tx))) +
  geom_point(alpha=0.3) +
  geom_line(
    data = data.frame(
      x = rep(c(0,0.49,0.51,1,0,0.49,0.51,1), 2),
      y = c(c(0.25,0.25,0.75,0.75,0,0.5,0.5,1), rep(0.5,8)),
      tx = c(0,0,1,1,0,0,1,1, rep(0,8)),
      design = rep(rep(c("SW-CRT", "ITS"), each=4), 2),
      sequence = rep(c("Sequence 1", "Sequence 2"), each=8)
    ),
    linewidth = 1.5
  ) +
  facet_grid(rows=dplyr::vars(sequence), cols=dplyr::vars(design)) +
  scale_color_manual(values=c("orange", "forestgreen")) +
  labs(color="Treatment") +
  theme(legend.position="bottom")



##################################.
##### Plot 2: four sequences #####
##################################.

n <- 200
# set.seed(924262) # Overestimated Tx
set.seed(89152) # Opposite sign
x_grid <- runif(n)
prm <- runif(8)*5
y_grid_1 <- sin(prm[1]*x_grid-prm[2]) + rnorm(n, sd=0.05)
y_grid_2 <- sin(prm[3]*x_grid-prm[4]) + rnorm(n, sd=0.05)
y_grid_3 <- sin(prm[5]*x_grid-prm[6]) + rnorm(n, sd=0.05)
y_grid_4 <- sin(prm[7]*x_grid-prm[8]) + rnorm(n, sd=0.05)

df_plot <- data.frame(
  x = rep(x_grid, 4),
  y = c(y_grid_1, y_grid_2, y_grid_3, y_grid_4),
  tx = c(In(x_grid>0.2), In(x_grid>0.4), In(x_grid>0.6), In(x_grid>0.8)),
  sequence = rep(c("Seq 1","Seq 2","Seq 3","Seq 4"), each=n)
)
tx_eff_true <- 0.4
df_plot %<>% dplyr::mutate(
  y = y + tx*tx_eff_true
)

ggplot(df_plot, aes(x=x, y=y, color=factor(tx))) +
  geom_point(alpha=0.3) +
  scale_color_manual(values=c("orange", "forestgreen")) +
  labs(color="Treatment") +
  facet_wrap(~sequence)

df_analysis <- df_plot
df_analysis %<>% dplyr::mutate(
  time = dplyr::case_when(
    x <= 0.2 ~ 1,
    x <= 0.4 ~ 2,
    x <= 0.6 ~ 3,
    x <= 0.8 ~ 4,
    x <= 1.0 ~ 5
  ),
  rte = 10*In(factor(sequence)) + time,
  x2 = dplyr::case_when(
    sequence=="Seq 1" & tx==1 ~ x - 0.2,
    sequence=="Seq 2" & tx==1 ~ x - 0.4,
    sequence=="Seq 3" & tx==1 ~ x - 0.6,
    sequence=="Seq 4" & tx==1 ~ x - 0.8,
    TRUE ~ 0
  )
)

model <- lme4::lmer(y~factor(time)-1+tx+(1|sequence), data=df_analysis)
tx_est <- summary(model)$coefficients["tx", "Estimate"]
model2 <- lme4::lmer(y~factor(time)-1+tx+(1|sequence)+(1|rte), data=df_analysis)
tx_est2 <- summary(model2)$coefficients["tx", "Estimate"]
print(paste0("Tx effect true: ", tx_eff_true))
print(paste0("Tx effect est (HH): ", round(tx_est, 1)))
print(paste0("Tx effect est (HG): ", round(tx_est2, 1)))


model_its <- lme4::lmer(
  # y ~ (1|sequence) + (x|sequence) + tx + (x2|sequence) - 1,
  y ~ (1|sequence) + (x|sequence) + tx + (x2|sequence) + x + x2,
  data = df_analysis
)
tx_est3 <- summary(model_its)$coefficients["tx", "Estimate"]
print(paste0("Tx effect est (ITS): ", round(tx_est3, 1)))
