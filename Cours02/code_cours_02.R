## ----setup, include = FALSE-----------------------------------------------------------------------------------------
library(tidyverse)
library(knitr)

# setting up knitr options
knitr::opts_chunk$set(
  cache = TRUE, echo = TRUE, warning = FALSE, message = FALSE,
  fig.align = "center", dev = "svg"
  )


## ----binomialcoef---------------------------------------------------------------------------------------------------
# computing the total number of possible configurations in R
choose(n = 3, k = 1)


## ----echo = FALSE, fig.align = "center", dev = "svg", fig.width = 7.5, fig.height = 5, cache = FALSE----------------
coin <- dbinom(x = 0:10, size = 10, prob = 0.5)

barplot(
  coin, names.arg = 0:10, border = NA, axes = FALSE,
  cex.names = 1.5, col = "grey20"
  )


## ----berndata, fig.width = 10, fig.height = 5-----------------------------------------------------------------------
library(tidyverse)
set.seed(666) # for reproducibility

rbinom(n = 500, size = 1, prob = 0.6) %>% # theta = 0.6
        data.frame %>%
        mutate(x = seq_along(.), y = cumsum(.) / seq_along(.) ) %>%
        ggplot(aes(x = x, y = y), log = "y") +
        geom_line(lwd = 1) +
        geom_hline(yintercept = 0.6, lty = 2) +
        labs(x = "Nombre de lancers", y = "Proportion de faces") +
        ylim(0, 1) + theme_bw(base_size = 18)


## ----likelihood, echo = TRUE, eval = TRUE, fig.width = 10, fig.height = 5-------------------------------------------
# Représentation graphique de la fonction de vraisemblance de theta pour y = 1 et n = 2

y <- 1 # nombre de faces
n <- 2 # nombre d'essais

data.frame(theta = seq(from = 0, to = 1, length.out = 1e3) ) %>%
  mutate(likelihood = dbinom(x = y, size = n, prob = theta) ) %>%
  ggplot(aes(x = theta, y = likelihood) ) +
  geom_area(color = "orangered", fill = "orangered", alpha = 0.5) +
  xlab(expression(paste(theta, " - Pr(face)") ) ) + ylab("Likelihod") +
  theme_bw(base_size = 20)


## ----echo = TRUE----------------------------------------------------------------------------------------------------
f <- function(theta) {2 * theta * (1 - theta) }
integrate(f = f, lower = 0, upper = 1)


## ----echo = FALSE, results = "asis"---------------------------------------------------------------------------------
library(knitr)
library(kableExtra)

data.frame(
  theta = as.character(c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0) ),
  x0 = c(1.0, 0.64, 0.36, 0.16, 0.04, 0.00),
  x1 = c(0.0, 0.32, 0.48, 0.48, 0.32, 0.00),
  x2 = c(0.0, 0.04, 0.16, 0.36, 0.64, 1.00)
  ) %>%
  mutate(Total = rowSums(.[2:4]) %>% as.character) %>%
  tibble::add_row(
    theta = "Total", x0 = sum(.$x0), x1 = sum(.$x1), x2 = sum(.$x2),
    Total = ""
    ) %>%
  kable(
    col.names = c("$\\theta$", "0", "1", "2", "Total"),
    format = "html",
    align = c("c", "c", "c", "c", "c"),
    caption = "Probabilité / vraisemblance pour deux lancers de pièce.",
    escape = TRUE
    ) %>%
  kable_styling(full_width = TRUE, position = "center") %>%
  add_header_above(c(" " = 1, "Nombre de Faces (y)" = 3, " " = 1) )


## ----beta1, echo = FALSE, fig.width = 12, fig.height = 6------------------------------------------------------------
p <- seq(0, 1, length = 100)

data.frame(
  p = p,
  p1 = dbeta(p, 1, 1),
  p2 = dbeta(p, 2, 2),
  p3 = dbeta(p, 4, 2),
  p4 = dbeta(p, 2, 4)
  ) %>%
  pivot_longer(p1:p4, names_to = "params", values_to = "prob") %>%
  ggplot(aes(x = p, y = prob / sum(prob), color = NULL, fill = params) ) +
  geom_area(position = "identity", alpha = 0.6) +
  theme_bw(base_size = 20) +
  xlab(expression(theta) ) +
  ylab("Densité de probabilité") +
  scale_fill_discrete(
    name = "Parameters",
    labels = c(
      "a = 1, b = 1",
      "a = 2, b = 2",
      "a = 4, b = 2",
      "a = 2, b = 4"
      )
    )


## ----beta2, echo = FALSE, fig.width = 12, fig.height = 6------------------------------------------------------------
p <- seq(0, 1, length = 100)

data.frame(
  p = p,
  p1 = dbeta(p, 1, 1),
  p2 = dbeta(p, 2, 2),
  p3 = dbeta(p, 4, 2),
  p4 = dbeta(p, 2, 4)
  ) %>%
  pivot_longer(p1:p4, names_to = "params", values_to = "prob") %>%
  ggplot(aes(x = p, y = prob / sum(prob), color = NULL, fill = params) ) +
  geom_area(position = "identity", alpha = 0.6) +
  theme_bw(base_size = 20) +
  xlab(expression(theta) ) +
  ylab("Densité de probabilité") +
  scale_fill_discrete(
    name = "Parameters",
    labels = c(
      "a = 1, b = 1",
      "a = 2, b = 2",
      "a = 4, b = 2",
      "a = 2, b = 4"
      )
    )


## ----beta3, echo = FALSE, fig.width = 10, fig.height = 5------------------------------------------------------------
p <- seq(0, 1, length = 100)

data.frame(
  p = p,
  p1 = dbeta(p, 1, 1),
  p2 = dbeta(p, 5, 5),
  p3 = dbeta(p, 50, 50)
  ) %>%
  pivot_longer(p1:p3, names_to = "params", values_to = "prob") %>%
  ggplot(aes(x = p, y = prob / sum(prob), color = NULL, fill = params) ) +
  geom_area(position = "identity", alpha = 0.6) +
  theme_bw(base_size = 20) +
  xlab(expression(theta) ) +
  ylab("Densité de probabilité") +
  scale_fill_discrete(
    name = "Parameters",
    labels = c(
      "a = 1, b = 1",
      "a = 5, b = 5",
      "a = 50, b = 50"
      )
    )


## ----beta4, echo = FALSE, fig.width = 12, fig.height = 5------------------------------------------------------------
p <- seq(0, 1, length = 100)

W <- 0.65
K1 <- 25
K2 <- 10

data.frame(
  p = p,
  p1 = dbeta(p, W * (K1 - 2) + 1, (1 - W) * (K1 - 2) + 1),
  p2 = dbeta(p, W * (K2 - 2) + 1, (1 - W) * (K2 - 2) + 1)
  ) %>%
  pivot_longer(p1:p2, names_to = "params", values_to = "prob") %>%
  ggplot(aes(x = p, y = prob / sum(prob), color = NULL, fill = params) ) +
  geom_area(position = "identity", alpha = 0.6) +
  theme_bw(base_size = 20) +
  xlab(expression(theta) ) +
  ylab("Densité de probabilité") +
  scale_fill_discrete(
    name = "Paramètres",
    labels = c(
      expression(paste(omega, " = 0.65 ", kappa, " = 25", sep = "") ),
      expression(paste(omega, " = 0.65 ", kappa, " = 10", sep = "") )
      )
    )


## ----beta-exemple, echo = FALSE, fig.width = 9, fig.height = 9------------------------------------------------------
library(patchwork)

df <- data.frame(theta = seq(0, 1, length = 100) ) %>%
  mutate(
    prior = dbeta(p, 1, 1),
    likelihood = dbinom(7, 10, p)
    ) %>%
  mutate(posterior = (prior * likelihood) )

p1 <- df %>%
  ggplot(aes(x = theta, y = prior, colour = NULL) ) +
  geom_area(
    fill = "steelblue",
    position = "identity", alpha = 0.5
    ) +
  theme_bw(base_size = 16) +
  xlab(expression(theta) ) +
  ylab("Densité de probabilité") +
  ylim(0, 2) +
  ggtitle("Prior distribution Beta(1, 1)")

p2 <- df %>%
  ggplot(aes(x = theta, y = likelihood, colour = NULL) ) +
  geom_area(
    fill = "orangered",
    position = "identity", alpha = 0.5
    ) +
  theme_bw(base_size = 16) +
  xlab(expression(theta) ) +
  ylab("Densité de probabilité") +
  ggtitle("Likelihood function Bin(7, 10)")

p3 <- df %>%
  ggplot(aes(x = theta, y = posterior, colour = NULL) ) +
  geom_area(
    fill = "purple",
    position = "identity", alpha = 0.5
    ) +
  theme_bw(base_size = 16) +
  xlab(expression(theta) ) +
  ylab("Densité de probabilité") +
  ggtitle("Posterior distribution Beta(8, 4)")

p1 / p2 / p3


## ----posterior-exemple1, echo = FALSE, fig.width = 12, fig.height = 8-----------------------------------------------
a <- 4
b <- 16
y <- 6
n <- 10

data.frame(theta = seq(0, 1, length = 100) ) %>%
  mutate(
    prior = dbeta(p, a, b),
    likelihood = dbinom(y, n, p)
    ) %>%
  mutate(prior = prior / sum(prior) ) %>%
  mutate(likelihood = likelihood / sum(likelihood) ) %>%
  mutate(posterior = (prior * likelihood) / sum(prior * likelihood) ) %>%
  pivot_longer(prior:posterior, names_to = "params", values_to = "prob") %>%
  ggplot(aes(x = theta, y = prob, colour = NULL, fill = params) ) +
  geom_area(position = "identity", alpha = 0.5) +
  theme_bw(base_size = 20) +
  xlab(expression(theta) ) +
  ylab("Densité de probabilité") +
  scale_fill_manual(
    name = "",
    labels = c("Likelihood", "Posterior", "Prior"),
    values = c("orangered", "purple", "steelblue")
    )


## ----posterior-exemple2, echo = FALSE, fig.width = 12, fig.height = 8-----------------------------------------------
a <- 4
b <- 16
y <- 12
n <- 20

data.frame(theta = seq(0, 1, length = 100) ) %>%
  mutate(
    prior = dbeta(p, a, b),
    likelihood = dbinom(y, n, p)
    ) %>%
  mutate(prior = prior / sum(prior) ) %>%
  mutate(likelihood = likelihood / sum(likelihood) ) %>%
  mutate(posterior = (prior * likelihood) / sum(prior * likelihood) ) %>%
  pivot_longer(prior:posterior, names_to = "params", values_to = "prob") %>%
  ggplot(aes(x = theta, y = prob, colour = NULL, fill = params) ) +
  geom_area(position = "identity", alpha = 0.5) +
  theme_bw(base_size = 20) +
  xlab(expression(theta) ) +
  ylab("Densité de probabilité") +
  scale_fill_manual(
    name = "",
    labels = c("Likelihood", "Posterior", "Prior"),
    values = c("orangered", "purple", "steelblue")
    )


## ----posterior-exemple3, echo = FALSE, fig.width = 12, fig.height = 8-----------------------------------------------
a <- 4
b <- 16
y <- 24
n <- 40

data.frame(theta = seq(0, 1, length = 100) ) %>%
  mutate(
    prior = dbeta(p, a, b),
    likelihood = dbinom(y, n, p)
    ) %>%
  mutate(prior = prior / sum(prior) ) %>%
  mutate(likelihood = likelihood / sum(likelihood) ) %>%
  mutate(posterior = (prior * likelihood) / sum(prior * likelihood) ) %>%
  pivot_longer(prior:posterior, names_to = "params", values_to = "prob") %>%
  ggplot(aes(x = theta, y = prob, colour = NULL, fill = params) ) +
  geom_area(position = "identity", alpha = 0.5) +
  theme_bw(base_size = 20) +
  xlab(expression(theta) ) +
  ylab("Densité de probabilité") +
  scale_fill_manual(
    name = "",
    labels = c("Likelihood", "Posterior", "Prior"),
    values = c("orangered", "purple", "steelblue")
    )


## ----posterior-exemple4, echo = FALSE, fig.width = 14, fig.height = 7-----------------------------------------------
a <- 3
b <- 17
y <- 8
n <- 10

data.frame(theta = seq(0, 1, length = 100) ) %>%
  mutate(
    prior = dbeta(p, a, b),
    likelihood = dbinom(y, n, p)
    ) %>%
  mutate(prior = prior / sum(prior) ) %>%
  mutate(likelihood = likelihood / sum(likelihood) ) %>%
  mutate(posterior = (prior * likelihood) / sum(prior * likelihood) ) %>%
  pivot_longer(prior:posterior, names_to = "params", values_to = "prob") %>%
  ggplot(aes(x = theta, y = prob, colour = NULL, fill = params) ) +
  geom_area(position = "identity", alpha = 0.5) +
  theme_bw(base_size = 20) +
  xlab(expression(theta) ) +
  ylab("Densité de probabilité") +
  scale_fill_manual(
    name = "",
    labels = c("Likelihood", "Posterior", "Prior"),
    values = c("orangered", "purple", "steelblue")
    )


## ----posterior-exemple5, echo = FALSE, fig.width = 12, fig.height = 6-----------------------------------------------
p <- seq(0, 1, length = 1e3)

y <- 8
n <- 10

data.frame(theta = seq(0, 1, length = 1e3) ) %>%
  mutate(
    prior = dunif(p, 0.25, 0.75),
    likelihood = dbinom(y, n, p)
    ) %>%
  mutate(prior = prior / sum(prior) ) %>%
  mutate(likelihood = likelihood / sum(likelihood) ) %>%
  mutate(posterior = (prior * likelihood) / sum(prior * likelihood) ) %>%
  pivot_longer(prior:posterior, names_to = "params", values_to = "prob") %>%
  ggplot(aes(x = theta, y = prob, colour = NULL, fill = params) ) +
  geom_area(position = "identity", alpha = 0.5) +
  theme_bw(base_size = 20) +
  xlab(expression(theta) ) +
  ylab("Densité de probabilité") +
  scale_fill_manual(
    name = "",
    labels = c("Likelihood", "Posterior", "Prior"),
    values = c("orangered", "purple", "steelblue")
    )


## ----discrete, echo = FALSE, out.width = "100%"---------------------------------------------------------------------
knitr::include_graphics("figures/discrete.png")


## ----continuous, echo = FALSE, out.width = "100%"-------------------------------------------------------------------
knitr::include_graphics("figures/continuous.png")


## ----grid1, echo = FALSE, fig.width = 12, fig.height = 6------------------------------------------------------------
thetaSize <- 30

data.frame(
  theta = seq(from = 0, to = 1, length.out = thetaSize),
  pTheta = rep(0.03, thetaSize)
  ) %>% 
  ggplot(aes(x = theta, y = 0, xend = theta) ) +
  geom_segment(aes(yend = pTheta), size = 1, colour = "#339900") +
  ylim(0, 0.15) +
  labs(x = expression(theta), y = "Densité de probabilité") +
  theme_bw(base_size = 20)


## ----grid2, echo = FALSE, fig.width = 12, fig.height = 6------------------------------------------------------------
thetaSize <- 30
a <- 3
b <- 7

data.frame(
  theta = seq(from = 0, to = 1, length.out = thetaSize)
  ) %>%
  mutate(
    prior = dbeta(theta, 3, 7) / sum(dbeta(theta, 3, 7) )
  ) %>% 
  ggplot(aes(x = theta, y = 0, xend = theta) ) +
  geom_segment(aes(yend = prior), size = 1, colour = "steelblue") +
  labs(x = expression(theta), y = "Densité de probabilité") +
  theme_bw(base_size = 20)


## ----grid3, echo = FALSE, fig.width = 12, fig.height = 6------------------------------------------------------------
thetaSize <- 30
a <- 3
b <- 7
y <- 12
n <- 20

data.frame(
  theta = seq(from = 0, to = 1, length.out = thetaSize)
  ) %>%
  mutate(
    prior = dbeta(theta, 3, 7) / sum(dbeta(theta, 3, 7) )
  ) %>% 
  mutate(
    likelihood = dbinom(y, n, theta) / sum(dbinom(y, n, theta) )
  ) %>% 
  ggplot(aes(x = theta, y = 0, xend = theta) ) +
  geom_segment(aes(yend = prior), size = 1, colour = "steelblue") +
  geom_segment(aes(
    x = theta + 0.01, yend = likelihood, xend = theta + 0.01),
    colour = "orangered", size = 1
    ) +
  labs(x = expression(theta), y = "Densité de probabilité") +
  theme_bw(base_size = 20)


## ----grid4, echo = FALSE, fig.width = 12, fig.height = 6------------------------------------------------------------
thetaSize <- 30
a <- 3
b <- 7
y <- 12
n <- 20

data.frame(
  theta = seq(from = 0, to = 1, length.out = thetaSize)
  ) %>%
  mutate(
    prior = dbeta(theta, 3, 7) / sum(dbeta(theta, 3, 7) )
  ) %>% 
  mutate(
    likelihood = dbinom(y, n, theta) / sum(dbinom(y, n, theta) )
  ) %>%
  mutate(
    posterior = (prior * likelihood) / sum(prior * likelihood)
  ) %>% 
  ggplot(aes(x = theta, y = 0, xend = theta) ) +
  geom_segment(aes(yend = prior), size = 1, colour = "steelblue") +
  geom_segment(aes(
    x = theta + 0.01, yend = likelihood, xend = theta + 0.01),
    colour = "orangered", size = 1
    ) +
  geom_segment(aes(
    x = theta - 0.01, yend = posterior, xend = theta - 0.01),
    colour = "purple", size = 1
    ) +
  labs(x = expression(theta), y = "Densité de probabilité") +
  theme_bw(base_size = 20)


## ----grid5, echo = FALSE, fig.width = 12, fig.height = 6------------------------------------------------------------
thetaSize <- 100
a <- 3
b <- 7
y <- 12
n <- 20

data.frame(
  theta = seq(from = 0, to = 1, length.out = thetaSize)
  ) %>%
  mutate(
    prior = dbeta(theta, 3, 7) / sum(dbeta(theta, 3, 7) )
  ) %>% 
  mutate(
    likelihood = dbinom(y, n, theta) / sum(dbinom(y, n, theta) )
  ) %>%
  mutate(
    posterior = (prior * likelihood) / sum(prior * likelihood)
  ) %>% 
  ggplot(aes(x = theta, y = 0, xend = theta) ) +
  geom_segment(aes(yend = prior), size = 1, colour = "steelblue") +
  geom_segment(aes(
    x = theta + 0.01, yend = likelihood, xend = theta + 0.01),
    colour = "orangered", size = 1
    ) +
  geom_segment(aes(
    x = theta - 0.01, yend = posterior, xend = theta - 0.01),
    colour = "purple", size = 1
    ) +
  labs(x = expression(theta), y = "Densité de probabilité") +
  theme_bw(base_size = 20)


## ----work-gif, echo = FALSE, out.width = "600px"--------------------------------------------------------------------
knitr::include_graphics("figures/not_gonna_work.gif")


## ----sampling1, eval = TRUE, echo = TRUE, fig.width = 8, fig.height = 4---------------------------------------------
p_grid <- seq(from = 0, to = 1, length.out = 1000) # creates a grid
prior <- rep(1, 1000) # uniform prior
likelihood <- dbinom(y, size = n, prob = p_grid) # computes likelihood
posterior <- (likelihood * prior) / sum(likelihood * prior) # computes posterior
samples <- sample(posterior, size = 1e3, prob = posterior, replace = TRUE) # sampling
hist(samples, main = "", xlab = expression(theta), cex.axis = 1, cex.lab = 1.5) # histogram


## ---- eval = TRUE, echo = FALSE, fig.width = 20, fig.height = 5-----------------------------------------------------
set.seed(666)

trajLength <- 100
theta <- 1:7
ptheta <- theta

trajectory <- sample(
  theta, prob = ptheta, size = trajLength, replace = TRUE
  )

layout(matrix(1:2, ncol = 2), widths = c(0.75, 0.25) )

plot(
  trajectory,
  main = "Distribution postérieure basée sur 100 tirages",
  ylab = expression(theta),
  xlim = c(0, trajLength),
  xlab = "Nombre d'itérations",
  type = "o", pch = 20, col = "purple",
  cex.lab = 2, cex.main = 2, cex.axis = 1.5
  )

barplot(
  table(trajectory), col = "purple",
  horiz = TRUE, axes = FALSE, axisnames = FALSE
  )


## ---- eval = TRUE, echo = FALSE, fig.width = 20, fig.height = 5-----------------------------------------------------
set.seed(666)

trajLength <- 1000
theta <- 1:7
ptheta <- theta

trajectory <- sample(
  theta, prob = ptheta, size = trajLength, replace = TRUE
  )

layout(matrix(1:2, ncol = 2), widths = c(0.75, 0.25) )

plot(
  trajectory,
  main = "Distribution postérieure basée sur 1000 tirages",
  ylab = expression(theta),
  xlim = c(0, trajLength),
  xlab = "Nombre d'itérations",
  type = "o", pch = 20, col = "purple",
  cex.lab = 2, cex.main = 2, cex.axis = 2
  )

barplot(
  table(trajectory), col = "purple",
  horiz = TRUE, axes = FALSE, axisnames = FALSE
  )


## ----eval = FALSE, echo = TRUE--------------------------------------------------------------------------------------
## p_grid <- seq(from = 0, to = 1, length.out = 1000)
## a <- b <- 1 # parameters of the Beta prior
## n <- 9 # number of observations
## y <- 6 # number of successes
## posterior <- dbeta(p_grid, y + a, n - y + b)


## ----eval = FALSE, echo = TRUE--------------------------------------------------------------------------------------
## p_grid <- seq(from = 0, to = 1, length.out = 1000)
## prior <- rep(1, 1000) # uniform prior
## likelihood <- dbinom(x = y, size = n, prob = p_grid)
## posterior <- (likelihood * prior) / sum(likelihood * prior)


## ----eval = FALSE, echo = TRUE--------------------------------------------------------------------------------------
## sample(x = p_grid, size = 1e4, prob = posterior, replace = TRUE)


## ----tendance-centrale1, eval = FALSE, echo = TRUE------------------------------------------------------------------
## mode_posterior <- find_mode(samples) # en bleu
## mean_posterior <- mean(samples) # en orange
## median_posterior <- median(samples) # en vert


## ----tendance-centrale2, echo = FALSE, fig.width = 18, fig.height = 6-----------------------------------------------
set.seed(666)
p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- rep(1, 1000)
likelihood <- dbinom(x = 3, size = 10, prob = p_grid)
posterior <- (likelihood * prior) / sum(likelihood * prior)
samples <- sample(p_grid, size = 1e4, replace = TRUE, prob = posterior)

find_mode <- function(samples, ...) {
  
    dd <- density(samples, ...)
    dd$x[which.max(dd$y)]
    
}

mode_posterior <- find_mode(samples)
mean_posterior <- mean(samples)
median_posterior <- median(samples)

data.frame(theta = p_grid, posterior = posterior) %>%
  ggplot(aes(x = theta, y = posterior, colour = NULL) ) +
  geom_area(position = "identity", fill = "purple", alpha = 0.25) +
  geom_vline(aes(xintercept = mode_posterior), size = 1, color = "steelblue") +
  annotate(
    geom = "text",
    x = mode_posterior - 0.01, y = max(posterior) / 2,
    label = "mode", color = "steelblue", angle = 90, size = 5
    ) +
  geom_vline(aes(xintercept = mean_posterior), size = 1, color = "orangered") +
  annotate(
    geom = "text",
    x = mean_posterior + 0.01, y = max(posterior) / 2,
    label = "moyenne", color = "orangered", angle = 90, size = 5
    ) +
  geom_vline(aes(xintercept = median_posterior), size = 1, color = "forestgreen") +
  annotate(
    geom = "text",
    x = median_posterior - 0.01, y = max(posterior) / 2,
    label = "médiane", color = "forestgreen", angle = 90, size = 5
    ) +
  theme_bw(base_size = 20) +
  xlab(expression(theta) ) + ylab("Densité de probabilité")


## ----superiority-prob, eval = TRUE, echo = TRUE---------------------------------------------------------------------
sum(samples > 0.5) / length(samples) # équivalent à mean(samples > 0.5)


## ----interval-prob, eval = TRUE, echo = TRUE------------------------------------------------------------------------
sum(samples > 0.2 & samples < 0.4) / length(samples)


## ----interval-prob-plot, echo = FALSE, fig.width = 12, fig.height = 4-----------------------------------------------
df <- data.frame(theta = p_grid, posterior = posterior)

ggplot(df, aes(x = theta, y = posterior, colour = NULL) ) +
    geom_area(
    position = "identity", fill = "purple", alpha = 0.5
    ) +
  geom_area(
    data = subset(df, theta > 0.4),
    position = "identity", fill = "purple", alpha = 0.5
    ) +
  geom_area(
    data = subset(df, 0.2 > theta),
    position = "identity", fill = "purple", alpha = 0.5
    ) +
  theme_bw(base_size = 20) +
  xlab(expression(theta) ) + ylab("Densité de probabilité")


## ----hdi-plot, echo = FALSE, out.width = "35%"----------------------------------------------------------------------
knitr::include_graphics("figures/HDI.png")


## ----eval = TRUE, echo = TRUE, fig.width = 12, fig.height = 6-------------------------------------------------------
library(BEST)

set.seed(666)
p_grid <- seq(from = 0, to = 1, length.out = 1e3)
pTheta <- dbeta(p_grid, 3, 10)
massVec <- pTheta / sum(pTheta)
samples <- sample(p_grid, size = 1e4, replace = TRUE, prob = pTheta)

plotPost(samples, credMass = 0.89, cex = 1.5, xlab = expression(theta), xlim = c(0, 1) )


## ----eval = TRUE, echo = FALSE, fig.width = 12, fig.height = 6------------------------------------------------------
library(BEST)

set.seed(666)

p_grid <- seq(from = 0, to = 1, length.out = 1e3)
pTheta <- dbeta(p_grid, 3, 10)
massVec <- pTheta / sum(pTheta)
samples <- sample(p_grid, size = 1e4, replace = TRUE, prob = pTheta)

plotPost(
  samples, credMass = 0.89,
  cex = 2, cex.axis = 1.5, cex.lab = 2,
  xlab = expression(theta), xlim = c(0, 1),
  ROPE = c(0.49, 0.51)
  )


## ----eval = TRUE, echo = TRUE---------------------------------------------------------------------------------------
samples <- rbinom(n = 1e4, size = 10, prob = 0.6)


## ----eval = TRUE, echo = TRUE---------------------------------------------------------------------------------------
posterior <- rbeta(n = 1e4, shape1 = 16, shape2 = 10)
samples <- rbinom(n = 1e4, size = 10, prob = posterior)


## ----ppc, echo = FALSE, fig.width = 12, fig.height = 8--------------------------------------------------------------
thetaSize <- 1e3
a <- 3
b <- 7
y <- 9
n <- 10

df <- data.frame(
  theta = seq(from = 0, to = 1, length.out = thetaSize)
  ) %>%
  mutate(
    prior = dbeta(theta, a, b) / sum(dbeta(theta, a, b) ),
    likelihood = dbinom(y, n, theta) / sum(dbinom(y, n, theta) )
    ) %>%
  mutate(
    unnormalised_posterior = prior * likelihood,
    posterior = unnormalised_posterior / sum(unnormalised_posterior)
    ) %>%
  mutate(
    # prior predictive distribution
    ppc1 = rbinom(thetaSize, n, rbeta(thetaSize, a, b) ),
    # posterior predictive distribution
    ppc2 = rbinom(thetaSize, n, rbeta(thetaSize, y + a, n - y + b) )
    )
  
p1 <- df %>%
  ggplot(aes(x = theta, y = prior) ) +
  geom_area(aes(colour = NULL), stat = "identity", fill = "steelblue", alpha = 0.8) +
  labs(
    x = expression(theta), y = "Densité de probabilité",
    title = "Prior distribution",
    subtitle = "rbeta(n = 1e4, shape1 = 3, shape2 = 7)"
    ) +
  theme_bw(base_size = 16)

p2 <- df %>%
  ggplot(aes(x = ppc1) ) +
  geom_histogram(aes(colour = NULL), fill = "black") +
  theme_bw(base_size = 16) +
  labs(
    x = "Nombre de Faces",
    y = "Nombre d'échantillons",
    title = "Prior predictive distribution",
    subtitle = "rbinom(n = 1e4, size = 10, prob = prior)"
    ) +
  scale_x_continuous(breaks = seq(0, 10, 1), labels = seq(0, 10, 1) )

p3 <- df %>%
  ggplot(aes(x = theta, y = posterior) ) +
  geom_area(aes(colour = NULL), stat = "identity", fill = "purple", alpha = 0.8) +
  labs(
    x = expression(theta), y = "Densité de probabilité",
    title = "Posterior distribution",
    subtitle = "rbeta(n = 1e4, shape1 = 12, shape2 = 8)"
    ) +
  theme_bw(base_size = 16)

p4 <- df %>%
  ggplot(aes(x = ppc2) ) +
  geom_histogram(aes(colour = NULL), fill = "black") +
  theme_bw(base_size = 16) +
  labs(
    x = "Nombre de Faces",
    y = "Nombre d'échantillons",
    title = "Posterior predictive distribution",
    subtitle = "rbinom(n = 1e4, size = 10, prob = posterior)"
    ) +
  scale_x_continuous(breaks = seq(0, 10, 1), labels = seq(0, 10, 1) )

library(patchwork)

(p1 | p2) / (p3 | p4) # stacking plots


## ----model-ppc, echo = FALSE, out.width = "75%"---------------------------------------------------------------------
knitr::include_graphics("figures/ModelPredictions.jpg")


## ----tartine, echo = FALSE, out.width = "25%"-----------------------------------------------------------------------
knitr::include_graphics("figures/tartine.jpg")


## ----eval = TRUE, echo = TRUE---------------------------------------------------------------------------------------
library(tidyverse)

# importer les données
data <- read.csv("data/experiment_TP2_1.csv")

# description sommaire des données
str(data)


## ----eval = TRUE, echo = TRUE, fig.width = 6, fig.height = 4--------------------------------------------------------
# nombre d'essais
nbTrial <- length(data$trial)

# nombre de "succès" (i.e., la tartine tombe du côté du beurre)
nbSuccess <- sum(data$value)

# taile de la grille
grid_size <- 1e3

# génère la grille
p_grid <- seq(from = 0, to = 1, length.out = grid_size)

# prior uniforme
prior <- rep(1, grid_size)

# calcul de la vraisemblance
likelihood <- dbinom(x = nbSuccess, size = nbTrial, prob = p_grid)

# calcul du posterior
posterior <- likelihood * prior / sum(likelihood * prior)


## ----eval = TRUE, echo = TRUE, fig.width = 10, fig.height = 6-------------------------------------------------------
samples <- sample(x = p_grid, prob = posterior, size = 1e4, replace = TRUE)
plotPost(samples, cex = 2, cex.axis = 1.5, cex.lab = 2, xlab = expression(theta) )


## ----eval = TRUE, echo = TRUE, fig.width = 10, fig.height = 6-------------------------------------------------------
plotPost(
  samples, cex = 2, cex.axis = 1.5, cex.lab = 2,
  xlab = expression(theta),
  ROPE = c(0.49, 0.51), compVal = 0.5
  )


## ----eval = TRUE, echo = TRUE---------------------------------------------------------------------------------------
data2 <- read.csv("data/experiment_TP2_2.csv")
str(data2)
nbTrial2 <- length(data2$trial) # nombre d'essais
nbSucces2 <- sum(data2$value) # nombre de succès


## ----eval = TRUE, echo = TRUE, fig.width = 10, fig.height = 5-------------------------------------------------------
mode1 <- find_mode(samples)
prior2 <- dbeta(p_grid, mode1 * (nbTrial - 2) + 1, (1 - mode1) * (nbTrial - 2) + 1)

plot(
  p_grid, prior2, type = "l", col = "steelblue", lwd = 3,
  xlab = expression(theta), ylab = "Densité de probabilité",
  cex = 2, cex.lab = 1.5, cex.axis = 1.25 
  )


## ----eval = TRUE, echo = TRUE, fig.width = 10, fig.height = 6-------------------------------------------------------
likelihood2 <- dbinom(x = nbSucces2, size = nbTrial2, prob = p_grid)
posterior2 <- likelihood2 * prior2 / sum(likelihood2 * prior2)
samples2 <- sample(p_grid, prob = posterior2, size = 1e4, replace = TRUE)

plotPost(
  samples2, cex = 2, cex.axis = 1.5, cex.lab = 2,
  xlab = expression(theta),
  ROPE = c(0.49, 0.51), compVal = 0.5
  )

