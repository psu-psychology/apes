# Source 
library(shiny)
library(ggplot2)
library(pwr)
library(greekLetters)
library(ggExtra)

# Define support functions
freqpoly <- function(x1, x2, binwidth = 0.1, xlim = c(-3, 3)) {
  df <- data.frame(
    x = c(x1, x2),
    g = c(rep("a", length(x1)), rep("b", length(x2)))
  )
  
  ggplot(df, aes(x, colour = g)) +
    geom_freqpoly(binwidth = binwidth, linewidth = 1) +
    coord_cartesian(xlim = xlim)
}

my_hist <- function(x1, x2, binwidth = 0.5, xlim = c(-3, 3)) {
  df <- data.frame(
    x = c(x1, x2),
    g = c(rep("a", length(x1)), rep("b", length(x2)))
  )
  
  p <- ggplot(df, aes(x, fill = g)) +
    geom_histogram(binwidth = binwidth, linewidth = 1) +
    coord_cartesian(xlim = xlim) +
    theme(legend.position = "none") +
    theme(axis.title.x = element_blank()) +
    geom_vline(xintercept = mean(x1)) +
    geom_vline(xintercept = mean(x2))
    
  p
}

my_box_violin <- function(x1, x2, binwidth = 0.5, xlim = c(-3, 3)) {
  df <- data.frame(
    x = c(x1, x2),
    g = c(rep("a", length(x1)), rep("b", length(x2)))
  )
  
  df.summ <- df |>
    dplyr::group_by(g) |>
    dplyr::summarize(se = sd(x)/sqrt(length(x)),
                     x = mean(x))
  
  p <- df |>
    ggplot() +
    aes(x = g, y = x, fill = g) +
    geom_boxplot() +
    geom_violin(alpha = .25) +
    geom_pointrange(aes(ymin = x + se, ymax = x - se), data = df.summ) +
    coord_flip(ylim = xlim) +
    theme(axis.title = element_blank()) +
    theme(legend.position = "none")
  p
}

my_point_range <- function(x1, x2, xlim = c(-3, 3)) {
  df <- data.frame(
    x = c(x1, x2),
    g = c(rep("a", length(x1)), rep("b", length(x2)))
  )
  
  df.summ <- df |>
    dplyr::group_by(g) |>
    dplyr::summarize(sd = sd(x),
                     se = sd/sqrt(length(x)),
                     x = mean(x))
  
  p <- df |>
    ggplot() +
    aes(x = g, y = x, fill = g, color = g) +
    geom_jitter(alpha = .3, height = 0, width = .1) +
    geom_pointrange(aes(ymin = x + sd, ymax = x - sd), color = "black", data = df.summ) +
    coord_flip(ylim = xlim) +
    theme(axis.title.x = element_blank()) +
    theme(axis.title.y = element_blank()) +
    theme(legend.position = "none")
  p
}

t_test <- function(x1, x2, alpha = 0.05, var.equal = TRUE, paired = FALSE) {
  test <- t.test(x2, x1, conf.level = 1 - alpha, var.equal = var.equal,
                 paired = paired)
  
  sig_test <- (alpha >= test$p.value)
  
  # use sprintf() to format t.test() results compactly
  sprintf(
    "Var==?: %s\nt[df=%3.3f]: %2.3f\np-value: %0.3f\nalpha: %0.3f\nSig?: %s\nmean A: %2.3f\nmean B: %2.3f\nB-A: %2.3f\nCI: [%0.2f, %0.2f]",
    var.equal,
    test$parameter, test$statistic,
    test$p.value,
    alpha,
    as.character(sig_test),
    mean(x1),
    mean(x2),
    test$estimate[1]-test$estimate[2],
    test$conf.int[1], test$conf.int[2]
  )
}

t_test_power <- function(n1, n2, d, sig.level, power = NULL) {
  test_result <- pwr.t2n.test(n1, n2, d, sig.level)
  
  sprintf("n1: %1d\nn2: %1d\npower: %0.3f\nalpha: %0.3f", 
          test_result$n1, test_result$n2, test_result$power,
          test_result$sig.level)
}

t_test_power_plot <- function(n1, n2, d, sig.level) {
  pwr.t2n.test(n1=n1, d=d, power=0.8, sig.level = sig.level, alternative="two.sided") |>
    plot()
}
