######################################.
##### Plots 1,2: basic ITS model #####
######################################.

# Generate data
n <- 100
j <- runif(n) * 10
j_0 <- 5
beta <- c(1,0.1,0.2)
delta <- 1
sigma <- 0.3
lin <- beta[1] + beta[2]*j + delta*In(j>j_0) + beta[3]*(j-j_0)*delta*In(j>j_0)
Y_j <- rnorm(n, mean=lin, sd=sigma)

# Create plot
df_plot <- data.frame(
  time = j,
  outcome = Y_j,
  treatment = In(j>j_0),
  time2 = pmax(j-j_0,0)
)
plot_1 <- ggplot(df_plot, aes(x=time, y=outcome, color=factor(treatment))) +
  geom_point(alpha=0.6) +
  labs(color="Treatment", x="Time", y="Outcome") +
  scale_color_manual(values=c("#E69F00", "#009E73"))
ggsave(filename="../Figures + Tables/ppt_1.png", plot=plot_1, device="png", width=6, height=4)

# Fit model
model <- lm(outcome~time+treatment+time2*treatment, data=df_plot)
df_preds <- data.frame(
  time = j,
  outcome = as.numeric(predict(model)),
  treatment = In(j>j_0)
)
plot_2 <- plot_1 + geom_line(data=df_preds)
ggsave(filename="../Figures + Tables/ppt_2.png", plot=plot_2, device="png", width=6, height=4)



################################################.
##### Plots 3,4: conceptualizing SW as ITS #####
################################################.

# Generate data
n <- 100
j <- runif(n) * 5
beta <- c(1,0.1,0.2)
delta <- 1
sigma <- 0.3
for (i in c(1:4)) {
  val <- beta[1] + beta[2]*j + delta*In(j>i) + beta[3]*(j-i)*delta*In(j>i)
  assign(x=paste0("lin_",i), value=val)
  assign(x=paste0("Y_j_",i), value=rnorm(n, mean=val, sd=sigma))
}

# Create plot
df_plot <- data.frame(
  time = rep(j,4),
  time_d = rep(ceiling(j),4),
  outcome = c(Y_j_1,Y_j_2,Y_j_3,Y_j_4),
  treatment = c(In(j>1), In(j>2), In(j>3), In(j>4)),
  cluster = rep(paste("Cluster",c(1:4)), each=length(j))
)
plot_3 <- ggplot(df_plot, aes(x=time, y=outcome, color=factor(treatment))) +
  geom_point(alpha=0.6) +
  labs(color="Treatment", x="Time", y="Outcome") +
  scale_color_manual(values=c("#E69F00", "#009E73")) +
  facet_grid(rows=dplyr::vars(cluster)) +
  xlim(0,5)
ggsave(filename="../Figures + Tables/ppt_3.png", plot=plot_3, device="png", width=8, height=4)

plot_4 <- ggplot(df_plot, aes(x=time_d, y=outcome, color=factor(treatment))) +
  geom_point(alpha=0.6) +
  labs(color="Treatment", x="Time", y="Outcome") +
  scale_color_manual(values=c("#E69F00", "#009E73")) +
  facet_grid(rows=dplyr::vars(cluster)) +
  xlim(0,5)
ggsave(filename="../Figures + Tables/ppt_4.png", plot=plot_4, device="png", width=8, height=4)



# ######################################################.
# ##### Plot X: two cluster two times illustration #####
# ######################################################.
# 
# n <- 200
# x_grid <- 2*runif(n)
# y_grid_1 <- x_grid + rnorm(n, sd=0.2)
# y_grid_2 <- 1 + rnorm(n, sd=0.2)
# 
# df_plot <- data.frame(
#   x = rep(x_grid, 4),
#   y = c(rep(y_grid_1, 2), rep(y_grid_2, 2)),
#   tx = c(rep(In(x_grid>1), 2), rep(0, 2*n)),
#   design = rep(rep(c("SW-CRT", "ITS"), each=n), 2),
#   cluster = rep(c("Cluster 1","Cluster 2"), each=2*n)
# )
# 
# ggplot(df_plot, aes(x=x, y=y, color=factor(tx))) +
#   geom_point(alpha=0.3) +
#   geom_line(
#     data = data.frame(
#       x = 2*rep(c(0,0.49,0.51,1,0,0.49,0.51,1), 2),
#       y = 2*c(c(0.25,0.25,0.75,0.75,0,0.5,0.5,1), rep(0.5,8)),
#       tx = c(0,0,1,1,0,0,1,1, rep(0,8)),
#       design = rep(rep(c("SW-CRT", "ITS"), each=4), 2),
#       cluster = rep(c("Cluster 1", "Cluster 2"), each=8)
#     ),
#     linewidth = 1.5
#   ) +
#   facet_grid(rows=dplyr::vars(cluster), cols=dplyr::vars(design)) +
#   scale_color_manual(values=c("#E69F00", "#009E73")) +
#   labs(color="Treatment", y="Outcome", x="Time") +
#   theme(legend.position="bottom")



# ########################################################.
# ##### Plot X: two cluster three times illustration #####
# ########################################################.
# 
# n <- 200
# x_grid <- 3*runif(n)
# y_grid_1 <- x_grid + rnorm(n, sd=0.1)
# # y_grid_2 <- 0.5 + rnorm(n, sd=0.1)
# y_grid_2 <- -1*x_grid + rnorm(n, sd=0.1)
# 
# df_plot <- data.frame(
#   x = rep(x_grid, 4),
#   time_d = ceiling(rep(x_grid, 4)),
#   y = c(rep(y_grid_1, 2), rep(y_grid_2, 2)),
#   tx = c(rep(In(x_grid>1), 2), rep(In(x_grid>2), 2)),
#   design = rep(rep(c("SW-CRT", "ITS"), each=n), 2),
#   cluster = rep(c("Cluster 1","Cluster 2"), each=2*n)
# )
# 
# ggplot(df_plot, aes(x=x, y=y, color=factor(tx))) +
#   geom_point(alpha=0.3) +
#   # geom_line(
#   #   data = data.frame(
#   #     x = 2*rep(c(0,0.49,0.51,1,0,0.49,0.51,1), 2),
#   #     y = 2*c(c(0.25,0.25,0.75,0.75,0,0.5,0.5,1), rep(0.5,8)),
#   #     tx = c(0,0,1,1,0,0,1,1, rep(0,8)),
#   #     design = rep(rep(c("SW-CRT", "ITS"), each=4), 2),
#   #     cluster = rep(c("Cluster 1", "Cluster 2"), each=8)
#   #   ),
#   #   linewidth = 1.5
#   # ) +
#   facet_grid(rows=dplyr::vars(cluster), cols=dplyr::vars(design)) +
#   scale_color_manual(values=c("#E69F00", "#009E73")) +
#   labs(color="Treatment", y="Outcome", x="Time") +
#   theme(legend.position="bottom")
# 
# model <- lme4::lmer(y~factor(time_d)-1+tx+(1|cluster), data=df_plot)
# summary(model)



##########################################################################.
##### Plot 5: illustration of SW estimation issues (no slope change) #####
##########################################################################.

# Generate data
# seed <- round(10^5*runif(1))
# seed <- 27887 # (0.4, 0.4)
# seed <- 17335 # (1.5, 1.7)
# seed <- 52556 # (1.5, 1.9)
seed <- 65602 # (0.2, 0.2) ***
set.seed(seed)
dat <- generate_data(
  data_type = "normal",
  time = "continuous",
  n_sequences = 4,
  n_clust_per_seq = 1,
  n_ind_per_cell = 30,
  delta = 1,
  sigma = 0.3,
  tau = 0.1,
  eta = 0.5,
  # eta2 = 0.5,
  re = "cluster"
)

# Plot
if (T) {
  dat$i <- paste("Cluster",dat$i)
  plot_5 <- ggplot(dat, aes(x=j, y=y_ij, color=factor(x_ij))) +
    geom_point(alpha=0.6) +
    labs(color="Treatment", x="Time", y="Outcome") +
    scale_color_manual(values=c("#E69F00", "#009E73")) +
    scale_y_continuous(breaks=c(-10:10)) +
    facet_wrap(~i, ncol=2) + # scales="free_y"
    xlim(0,5)
  ggsave(filename="../Figures + Tables/ppt_5.png", plot=plot_5, device="png", width=7, height=5)
}

# Calculated columns
dat$j_disc <- ceiling(dat$j)
dat$ij <- as.numeric(factor(paste0(dat$i, "-", dat$j_disc)))

model_hh <- lme4::lmer(
  y_ij ~ factor(j_disc) - 1 + (1|i) + x_ij,
  data = dat
)
model_ne <- lme4::lmer(
  y_ij ~ factor(j_disc) - 1 + (1|i) + (1|ij) + x_ij,
  data = dat
)
# model_pits <- lme4::lmer(
#   y_ij ~ j + (j|i) + (1|i) + x_ij,
#   data = dat
# )

tx_est_hh <- summary(model_hh)$coefficients["x_ij", "Estimate"]
tx_est_ne <- summary(model_ne)$coefficients["x_ij", "Estimate"]
# tx_est_pits <- summary(model_pits)$coefficients["x_ij", "Estimate"]
print(seed)
print(paste("tx_est_hh:", round(tx_est_hh,2)))
print(paste("tx_est_ne:", round(tx_est_ne,2)))
# print(paste("tx_est_pits:", tx_est_pits))



############################################################################.
##### Plot 6: illustration of SW estimation issues (with slope change) #####
############################################################################.

# Generate data
seed <- round(10^5*runif(1))
# seed <- 62214
set.seed(seed)
dat <- generate_data(
  data_type = "normal",
  time = "continuous",
  n_sequences = 4,
  n_clust_per_seq = 1,
  n_ind_per_cell = 30,
  delta = 0,
  sigma = 0.3,
  tau = 0.1,
  eta = 0.25,
  eta2 = 0.5,
  re = "cluster"
)

# Plot
if (T) {
  dat$i <- paste("Cluster",dat$i)
  plot_6 <- ggplot(dat, aes(x=j, y=y_ij, color=factor(x_ij))) +
    geom_point(alpha=0.6) +
    labs(color="Treatment", x="Time", y="Outcome") +
    scale_color_manual(values=c("#E69F00", "#009E73")) +
    scale_y_continuous(breaks=c(-10:10)) +
    facet_wrap(~i, ncol=2) + # scales="free_y"
    xlim(0,5)
  plot_6
  # ggsave(filename="../Figures + Tables/ppt_6.png", plot=plot_6, device="png", width=7, height=5)
}

# Calculated columns
dat$j_disc <- ceiling(dat$j)
dat$ij <- as.numeric(factor(paste0(dat$i, "-", dat$j_disc)))

# dat %>% group_by(i,j_disc) %>% dplyr::summarize(max_x=max(x_ij))

model_hh <- lme4::lmer(
  y_ij ~ factor(j_disc) - 1 + (1|i) + x_ij,
  data = dat
)
model_ne <- lme4::lmer(
  y_ij ~ factor(j_disc) - 1 + (1|i) + (1|ij) + x_ij,
  data = dat
)
# model_pits <- lme4::lmer(
#   y_ij ~ j + (j|i) + (1|i) + x_ij,
#   data = dat
# )

tx_est_hh <- summary(model_hh)$coefficients["x_ij", "Estimate"]
tx_est_ne <- summary(model_ne)$coefficients["x_ij", "Estimate"]
tx_se_hh <- summary(model_hh)$coefficients["x_ij", "Std. Error"]
tx_se_ne <- summary(model_ne)$coefficients["x_ij", "Std. Error"]
# tx_est_pits <- summary(model_pits)$coefficients["x_ij", "Estimate"]
print(seed)
print(paste("tx_est_hh:", round(tx_est_hh,2)))
tx_est_hh + c(-1.96,1.96)*tx_se_hh
print(paste("tx_est_ne:", round(tx_est_ne,2)))
tx_est_ne + c(-1.96,1.96)*tx_se_ne
# print(paste("tx_est_pits:", tx_est_pits))



################################.
##### Plot 7: Sample paths #####
################################.

set.seed(123)
dat <- generate_data(
  data_type = "normal",
  time = "continuous",
  n_sequences = 6,
  n_clust_per_seq = 1,
  n_ind_per_cell = 20,
  delta = 1,
  sigma = 1,
  tau = 0.25,
  eta = 0.75,
  re = "cluster"
)
dat$i <- paste("Cluster", dat$i)

plot_7 <- ggplot(dat, aes(x=j, y=y_ij, color=factor(x_ij))) +
  geom_point(alpha=0.5) +
  labs(color="Treatment", x="Time", y="Outcome") +
  scale_color_manual(values=c("#E69F00", "#009E73")) +
  facet_wrap(~factor(i), ncol=3)
ggsave(filename="../Figures + Tables/ppt_7.png", plot=plot_7, device="png", width=7, height=5)


######################################.
##### Plot 8: Simulation results #####
######################################.

sim <- readRDS("SimEngine.out/estimation_1_20250712.rds")

for (n_per in sim$levels$n_ind_per_cell) {
  
  res <- dplyr::filter(sim$results, n_ind_per_cell==n_per)
  plot <- ggplot(res, aes(x=tx_est, y=0, color=model)) +
    geom_jitter(width=0, height=1, alpha=0.2, size=3) +
    facet_wrap(~model, ncol=1, strip.position="left") +
    labs(y=NULL, x="Treatment effect estimate") +
    ylim(-2,2) +
    xlim(-0.5,2.5) +
    theme(axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.grid.major.y=element_blank(),
          panel.grid.minor.y=element_blank(),
          strip.text.y.left=element_text(angle=0),
          legend.position="none")
  fn <- ifelse(n_per==20, "fig_8.png", "fig_9.png")
  ggsave(filename=paste0("../Figures + Tables/",fn), plot=plot, device="png", width=6, height=4)
  
  sim %>% SimEngine::summarize(
    list(stat="mean", x="tx_est"),
    list(stat="sd", x="tx_est"),
    list(stat="coverage", estimate="tx_est", se="tx_se", truth=1, name="cov")
  )
  
}




# n <- 200
# seed <- round(10^5*runif(1))
# print(seed)
# set.seed(seed)
# # set.seed(84809)
# 
# 
# x_grid <- runif(n)
# prm <- runif(8)*5
# y_grid_1 <- sin(prm[1]*x_grid-prm[2]) + rnorm(n, sd=0.1)
# y_grid_2 <- sin(prm[3]*x_grid-prm[4]) + rnorm(n, sd=0.1)
# y_grid_3 <- sin(prm[5]*x_grid-prm[6]) + rnorm(n, sd=0.1)
# y_grid_4 <- sin(prm[7]*x_grid-prm[8]) + rnorm(n, sd=0.1)
# 
# df_plot <- data.frame(
#   x = rep(x_grid, 4),
#   y = c(y_grid_1, y_grid_2, y_grid_3, y_grid_4),
#   tx = c(In(x_grid>0.2), In(x_grid>0.4), In(x_grid>0.6), In(x_grid>0.8)),
#   sequence = rep(c("Seq 1","Seq 2","Seq 3","Seq 4"), each=n)
# )
# tx_eff_true <- 1
# df_plot %<>% dplyr::mutate(
#   y = y + tx*tx_eff_true
# )
# 
# ggplot(df_plot, aes(x=x, y=y, color=factor(tx))) +
#   geom_point(alpha=0.3) +
#   scale_color_manual(values=c("orange", "forestgreen")) +
#   labs(color="Treatment") +
#   facet_wrap(~sequence)
# 
# df_analysis <- df_plot
# df_analysis %<>% dplyr::mutate(
#   time = dplyr::case_when(
#     x <= 0.2 ~ 1,
#     x <= 0.4 ~ 2,
#     x <= 0.6 ~ 3,
#     x <= 0.8 ~ 4,
#     x <= 1.0 ~ 5
#   ),
#   rte = 10*In(factor(sequence)) + time,
#   x2 = dplyr::case_when(
#     sequence=="Seq 1" & tx==1 ~ x - 0.2,
#     sequence=="Seq 2" & tx==1 ~ x - 0.4,
#     sequence=="Seq 3" & tx==1 ~ x - 0.6,
#     sequence=="Seq 4" & tx==1 ~ x - 0.8,
#     TRUE ~ 0
#   )
# )
# 
# model <- lme4::lmer(y~factor(time)-1+tx+(1|sequence), data=df_analysis)
# tx_est <- summary(model)$coefficients["tx", "Estimate"]
# model2 <- lme4::lmer(y~factor(time)-1+tx+(1|sequence)+(1|rte), data=df_analysis)
# tx_est2 <- summary(model2)$coefficients["tx", "Estimate"]
# print(paste0("Tx effect true: ", tx_eff_true))
# print(paste0("Tx effect est (HH): ", round(tx_est, 1)))
# print(paste0("Tx effect est (NE): ", round(tx_est2, 1)))
# 
# 
# model_its <- lme4::lmer(
#   # y ~ (1|sequence) + (x|sequence) + tx + (x2|sequence) - 1,
#   y ~ (1|sequence) + (x|sequence) + tx + (x2|sequence) + x + x2,
#   data = df_analysis
# )
# tx_est3 <- summary(model_its)$coefficients["tx", "Estimate"]
# print(paste0("Tx effect est (ITS): ", round(tx_est3, 1)))
