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

myhist <- function(x1, x2, binwidth = 0.5, xlim = c(-3, 3)) {
  df <- data.frame(
    x = c(x1, x2),
    g = c(rep("a", length(x1)), rep("b", length(x2)))
  )
  
  p <- ggplot(df, aes(x, fill = g)) +
    geom_histogram(binwidth = binwidth, linewidth = 1) +
    coord_cartesian(xlim = xlim) +
    theme(legend.position = "none") +
    geom_vline(xintercept = mean(x1)) +
    geom_vline(xintercept = mean(x2))
    
  # p + annotate("text", x = -3, y = 25, label = paste0(greeks("mu"), "A: ", format(mean(x1), 3, 2))) +
  #   annotate("text", x = 3, y = 25, label = paste0(greeks("mu"), "B: ", format(mean(x2), 3, 2)))
  p
}

myhist_bpmarg <- function(x1, x2, binwidth = 0.5, xlim = c(-3, 3)) {
  df <- data.frame(
    x = c(x1, x2),
    g = c(rep("a", length(x1)), rep("b", length(x2)))
  )
  
  p <- ggplot(df, aes(x, fill = g)) +
    geom_histogram(binwidth = binwidth, linewidth = 1) +
    coord_cartesian(xlim = xlim)
  
  ggMarginal(p, type="boxplot")
}

t_test <- function(x1, x2, alpha = 0.05, var.equal = TRUE) {
  test <- t.test(x2, x1, conf.level = 1 - alpha, var.equal = var.equal)
  
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

t_test_power <- function(n1, n2, d) {
  test_result <- pwr.t2n.test(n1, n2, d)
  
  sprintf("n1: %1d\nn2: %1d\npower: %0.3f\nalpha: %0.3f", 
          test_result$n1, test_result$n2, test_result$power,
          test_result$sig.level)
}

t_test_power_plot <- function(n1, d) {
  test_result <- pwr.t2n.test(n1=n1, d=d, power=0.8, alternative="two.sided")
  plot(test_result)
}
