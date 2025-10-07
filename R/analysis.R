library(ggplot2)
library(magrittr)

# Loop through datasets and run analysis
# 23 HH 1.9, PITS 0.7; not singf
# 24 HH 29, PITS 39; interesting visual
# 33 opposite sign but not significant; no interesting visual
# 41 strong linear pattern
# 62 strong linear pattern, but PITS did not converge
for (i in c(24)) {
  
  dir <-"G:/Shared drives/Stepped Wedge Data Files/Trials/"
  file <- paste0(dir, dir(dir)[i], "/Standardized data/trial_data.csv")
  dat <- read.csv(file)
  # print(nrow(dat))
  
  ################################
  
  dat %<>% dplyr::rename(
    "j" = `time`,
    "tx" = `trt`,
    "i" = `id_cluster`
  )
  
  if (!is.null(dat$`out_cont_1`)) {
    dat %<>% dplyr::rename("outcome"=`out_cont_1`)
  } else if (!is.null(dat$`out_poiss_1`)) {
    dat %<>% dplyr::rename("outcome"=`out_poiss_1`)
  # } else if (!is.null(dat$`out_bin_1`)) {
  #   dat %<>% dplyr::rename("outcome"=`out_bin_1`)
  } else {
    stop("Not cont or poiss.")
  }
  
  dat_summ <- dat %>%
    dplyr::group_by(i) %>%
    dplyr::summarize("crossover" = min(j+1000*(1-tx)))
  
  dat %<>% dplyr::left_join(dat_summ, by="i")
  dat %<>% dplyr::mutate(j2=tx*(j-crossover))
  dat$ij <- as.numeric(factor(paste0(dat$i, "-", dat$j)))
  
  if (F) {
    dat$preds_hh <- as.numeric(predict(model_hh))
    dat$preds_pits <- as.numeric(predict(model_pits))
    dat$preds_pits2 <- as.numeric(predict(model_pits2))
  }
  set.seed(1)
  # dat2 <- dplyr::filter(dat, i %in% sample(unique(dat$i))[1:8])
  dat2 <- dat
  dat2$i <- paste("Cluster", dat2$i)
  
  plot <- ggplot(dat2, aes(x=j, y=outcome, color=factor(tx))) +
    geom_point(alpha=0.5) +
    geom_line(aes(y=preds_hh), color="darkred", linewidth=1) +
    # geom_line(aes(y=preds_pits), color="blue", linewidth=1) +
    # geom_line(aes(y=preds_pits2), color="darkorchid3", linewidth=1) +
    labs(color="Treatment", x="Time", y="Outcome") +
    scale_color_manual(values=c("#E69F00", "#009E73")) +
    facet_wrap(~factor(i), ncol=4)
  print(plot)
  
  model_hh <- lme4::lmer(
    outcome ~ factor(j) - 1 + (1|i) + tx,
    data = dat
  )
  model_ne <- lme4::lmer(
    outcome ~ factor(j) - 1 + (1|i) + (1|ij) + tx,
    data = dat
  )
  dat %<>% dplyr::mutate("time"=j, "time2"=j2)
  model_pits <- steppedwedge::analyze_pits(dat, rte=F)
  model_pits2 <- steppedwedge::analyze_pits(dat, rte=T)
  
  model_pits <- lme4::lmer(
    outcome ~ j + (j+j2|i) + tx + j2,
    data = dat
  )
  model_pits2 <- lme4::lmer(
    outcome ~ j + (j+j2+tx|i) + tx + j2,
    data = dat
  )
  
  print(paste("HH:", round(summary(model_hh)$coefficients["tx", "Estimate"], 3)))
  print(paste("NE:", round(summary(model_ne)$coefficients["tx", "Estimate"], 3)))
  print(paste("PITS:", round(summary(model_pits)$coefficients["tx", "Estimate"], 3)))
  print(paste("PITS2:", round(summary(model_pits2)$coefficients["tx", "Estimate"], 3)))
  
}
