## ----setup, include = FALSE--------------------------------------------------------------------------------------------
library(tidyverse)
library(patchwork)
library(plotly)
library(knitr)
library(BEST)
library(brms)

# setting up knitr options
opts_chunk$set(
  cache = TRUE, echo = TRUE, warning = FALSE, message = FALSE,
  fig.align = "center", dev = "svg"
  )


## ----eval = TRUE, echo = TRUE------------------------------------------------------------------------------------------
men <- rnorm(100, 175, 10) # 100 tailles d'hommes
women <- rnorm(100, 170, 10) # 100 tailles de femmes


## ----eval = TRUE, echo = TRUE------------------------------------------------------------------------------------------
t.test(men, women)


## ----eval = TRUE, echo = TRUE------------------------------------------------------------------------------------------
nsims <- 1e4 # nombre de simulations
t <- rep(NA, nsims) # initialisation d'un vecteur vide

for (i in 1:nsims) {
    
    men2 <- rnorm(100, 170, 10) # 100 tailles d'hommes
    women2 <- rnorm(100, 170, 10) # 100 tailles de femmes
    t[i] <- t.test(men2, women2)$statistic # on conserve la t-valeur
    
}


## ----eval = TRUE, echo = TRUE------------------------------------------------------------------------------------------
# une autre manière de réaliser la même opération, sans boucle for
t <- replicate(nsims, t.test(rnorm(100, 170, 10), rnorm(100, 170, 10) )$statistic)


## ----eval = TRUE, echo = TRUE, message = FALSE, fig.width = 6, fig.height = 6------------------------------------------
data.frame(t = t) %>%
    ggplot(aes(x = t) ) +
    geom_histogram() +
    theme_bw(base_size = 20) +
    labs(x = "Valeur de t", y = "Nombre d'échantillons")


## ----eval = TRUE, echo = TRUE, fig.width = 6, fig.height = 6-----------------------------------------------------------
data.frame(x = c(-5, 5) ) %>%
    ggplot(aes(x = x) ) +
    stat_function(fun = dt, args = list(df = t.test(men, women)$parameter), size = 1.5) +
    theme_bw(base_size = 20) +
    labs(x = "Valeur de t", y = "Densité de probabilité")


## ----eval = TRUE, echo = TRUE------------------------------------------------------------------------------------------
alpha <- 0.05
abs(qt(alpha / 2, df = t.test(men, women)$parameter) ) # valeur de t critique


## ----eval = TRUE, echo = FALSE, fig.width = 7, fig.height = 7----------------------------------------------------------
data.frame(t = c(-5, 5) ) %>%
    ggplot(aes(x = t) ) +
    stat_function(fun = dt, args = list(df = t.test(men, women)$parameter), size = 1.5) +
    stat_function(
        fun = dt, args = list(df = t.test(men, women)$parameter),
        xlim = c(-5, qt(0.025, df = t.test(men, women)$parameter) ),
        geom = "area", alpha = 0.5
        ) +
    stat_function(
        fun = dt, args = list(df = t.test(men, women)$parameter),
        xlim = c(qt(0.975, df = t.test(men, women)$parameter), 5),
        geom = "area", alpha = 0.5
        ) +
    theme_bw(base_size = 20) +
    labs(x = "Valeur de t", y = "Densité de probabilité")


## ----eval = TRUE, echo = TRUE------------------------------------------------------------------------------------------
tobs <- t.test(men, women)$statistic # valeur de t observée
tobs %>% as.numeric


## ----eval = TRUE, echo = FALSE, fig.width = 7, fig.height = 7----------------------------------------------------------
data.frame(t = c(-5, 5) ) %>%
    ggplot(aes(x = t) ) +
    stat_function(fun = dt, args = list(df = t.test(men, women)$parameter), size = 1.5) +
    stat_function(
        fun = dt, args = list(df = t.test(men, women)$parameter),
        xlim = c(-5, qt(0.025, df = t.test(men, women)$parameter) ),
        geom = "area", alpha = 0.5
            ) +
    stat_function(
        fun = dt, args = list(df = t.test(men, women)$parameter),
        xlim = c(qt(0.975, df = t.test(men, women)$parameter), 5),
        geom = "area", alpha = 0.5
        ) +
    stat_function(
        fun = dt, args = list(df = t.test(men, women)$parameter),
        xlim = c(-5, - tobs),
        geom = "area") +
    stat_function(
        fun = dt, args = list(df = t.test(men, women)$parameter),
        xlim = c(tobs, 5),
        geom = "area") +
    theme_bw(base_size = 20) +
    labs(x = "Valeur de t", y = "Densité de probabilité")


## ----eval = TRUE, echo = TRUE------------------------------------------------------------------------------------------
t.test(men, women)$p.value

tvalue <- abs(t.test(men, women)$statistic)
df <- t.test(men, women)$parameter

2 * integrate(dt, tvalue, Inf, df = df)$value

2 * (1 - pt(abs(t.test(men, women)$statistic), t.test(men, women)$parameter) )


## ---- echo = FALSE, fig.width = 10, fig.height = 5---------------------------------------------------------------------
data.frame(x = seq(from = 0, to = 1, length.out = 1e2) ) %>%
    mutate(M1 = dbeta(x, 6, 10), M2 = dbeta(x, 20, 12) ) %>%
    gather(prior, value, M1:M2) %>%
    ggplot(aes(x = x, y = value, fill = prior) ) +
    geom_area(alpha = 0.75, position = "identity") +
    scale_fill_manual(values = c("#016392", "#c72e29") ) +
    xlab(expression(paste("Probabilité d'obtenir Face ", theta) ) ) +
    ylab("Densité de probabilité") +
    theme_bw(base_size = 20)


## ----echo = FALSE, out.width = "800px"---------------------------------------------------------------------------------
knitr::include_graphics("figures/bf.gif")


## ----eval = TRUE, echo = TRUE------------------------------------------------------------------------------------------
library(BayesFactor)

ttestBF(men, women)


## ----eval = TRUE, echo = TRUE, fig.width = 6, fig.height = 6-----------------------------------------------------------
data.frame(x = c(-10, 10) ) %>%
    ggplot(aes(x = x) ) +
    stat_function(
        fun = dcauchy, args = list(location = 0, scale = sqrt(2) / 2), size = 1.5
        ) +
    theme_bw(base_size = 20) + labs(x = expression(theta), y = "Densité de probabilité")


## ----eval = TRUE, echo = FALSE, fig.align = "center", fig.width = 7.5, fig.height = 5----------------------------------
data.frame(x = c(-20, 20) ) %>%
    ggplot(aes(x = x) ) +
    stat_function(fun = dnorm, args = list(mean = 0, sd = 1), size = 1.5) +
    stat_function(fun = dnorm, args = list(mean = 0, sd = 2), size = 1.5) +
    stat_function(fun = dnorm, args = list(mean = 0, sd = 5), size = 1.5) +
    stat_function(fun = dnorm, args = list(mean = 0, sd = 10), size = 1.5) +
    theme_bw(base_size = 20) +
    labs(x = expression(theta), y = "Densité de probabilité")


## ----eval = TRUE, echo = TRUE, fig.align = "center", fig.width = 7.5, fig.height = 5-----------------------------------
ppnames <- c("afarensis", "africanus", "habilis", "boisei",
        "rudolfensis", "ergaster", "sapiens")
brainvolcc <- c(438, 452, 612, 521, 752, 871, 1350)
masskg <- c(37.0, 35.5, 34.5, 41.5, 55.5, 61.0, 53.5)

d <- data.frame(species = ppnames, brain = brainvolcc, mass = masskg)

d %>%
    ggplot(aes(x = mass, y = brain, label = species) ) +
    geom_point() +
    ggrepel::geom_label_repel(hjust = 0, nudge_y = 50, size = 5) +
    theme_bw(base_size = 18) + xlim(30, 70)


## ----eval = TRUE, echo = TRUE------------------------------------------------------------------------------------------
mod1.1 <- lm(brain ~ mass, data = d)
(var(d$brain) - var(residuals(mod1.1) ) ) / var(d$brain)

mod1.2 <- lm(brain ~ mass + I(mass^2), data = d)
(var(d$brain) - var(residuals(mod1.2) ) ) / var(d$brain)

mod1.3 <- lm(brain ~ mass + I(mass^2) + I(mass^3), data = d)
(var(d$brain) - var(residuals(mod1.3) ) ) / var(d$brain)


## ----eval = TRUE, echo = TRUE------------------------------------------------------------------------------------------
mod1.4 <- lm(brain ~ mass + I(mass^2) + I(mass^3) + I(mass^4), data = d)
(var(d$brain) - var(residuals(mod1.4) ) ) / var(d$brain)

mod1.5 <- lm(brain ~ mass + I(mass^2) + I(mass^3) + I(mass^4) +
    I(mass^5), data = d)
(var(d$brain) - var(residuals(mod1.5) ) ) / var(d$brain)

mod1.6 <- lm( brain ~ mass + I(mass^2) + I(mass^3) + I(mass^4) +
    I(mass^5) + I(mass^6), data = d)
(var(d$brain) - var(residuals(mod1.6) ) ) / var(d$brain)


## ----eval = TRUE, echo = FALSE, fig.width = 18, fig.height = 9---------------------------------------------------------
library(patchwork)
library(gridExtra)
library(ggplot2)

# p <- list()
# 
# for (i in 1:2) {
#     
#     p[[i]] <-
#         ggplot(data = d, aes(x = mass, y = brain) ) +
#         geom_point() +
#         theme_bw(base_size = 20) +
#         ylim(-400, 2000) +
#         ggtitle(bquote(R^'2'~'='~.(round(summary(get(paste0("mod1.", i) ) )$r.squared, 2) ) ) ) +
#         geom_line(
#             data = data.frame(mass = seq(min(d$mass), max(d$mass), length.out = 100) ) %>%
#                 mutate(pred = predict(get(paste0("mod1.", i) ), newdata = .) ), aes(x = mass, y = pred) ) +
#         geom_hline(yintercept = 0, linetype = 2)
#     
# }
# 
# do.call(grid.arrange, p)

p1 <- 
  ggplot(data = d, aes(x = mass, y = brain) ) +
  geom_point() +
  theme_bw(base_size = 20) +
  ylim(-400, 2000) +
  ggtitle(bquote(R^'2'~'='~.(round(summary(mod1.1)$r.squared, 2) ) ) ) +
  geom_line(
      data = data.frame(mass = seq(min(d$mass), max(d$mass), length.out = 100) ) %>%
          mutate(pred = predict(mod1.1, newdata = .) ), aes(x = mass, y = pred) ) +
  geom_hline(yintercept = 0, linetype = 2)

p2 <- 
  ggplot(data = d, aes(x = mass, y = brain) ) +
  geom_point() +
  theme_bw(base_size = 20) +
  ylim(-400, 2000) +
  ggtitle(bquote(R^'2'~'='~.(round(summary(mod1.2)$r.squared, 2) ) ) ) +
  geom_line(
      data = data.frame(mass = seq(min(d$mass), max(d$mass), length.out = 100) ) %>%
          mutate(pred = predict(mod1.2, newdata = .) ), aes(x = mass, y = pred) ) +
  geom_hline(yintercept = 0, linetype = 2)

p1 + p2


## ----eval = TRUE, echo = FALSE, fig.align = "center", fig.width = 18, fig.height = 9-----------------------------------
p3 <- 
  ggplot(data = d, aes(x = mass, y = brain) ) +
  geom_point() +
  theme_bw(base_size = 20) +
  ylim(-400, 2000) +
  ggtitle(bquote(R^'2'~'='~.(round(summary(mod1.3)$r.squared, 2) ) ) ) +
  geom_line(
      data = data.frame(mass = seq(min(d$mass), max(d$mass), length.out = 100) ) %>%
          mutate(pred = predict(mod1.3, newdata = .) ), aes(x = mass, y = pred) ) +
  geom_hline(yintercept = 0, linetype = 2)

p4 <- 
  ggplot(data = d, aes(x = mass, y = brain) ) +
  geom_point() +
  theme_bw(base_size = 20) +
  ylim(-400, 2000) +
  ggtitle(bquote(R^'2'~'='~.(round(summary(mod1.4)$r.squared, 2) ) ) ) +
  geom_line(
      data = data.frame(mass = seq(min(d$mass), max(d$mass), length.out = 100) ) %>%
          mutate(pred = predict(mod1.4, newdata = .) ), aes(x = mass, y = pred) ) +
  geom_hline(yintercept = 0, linetype = 2)

p3 + p4


## ----eval = TRUE, echo = FALSE, fig.width = 18, fig.height = 9---------------------------------------------------------
p5 <- 
  ggplot(data = d, aes(x = mass, y = brain) ) +
  geom_point() +
  theme_bw(base_size = 20) +
  ylim(-400, 2000) +
  ggtitle(bquote(R^'2'~'='~.(round(summary(mod1.5)$r.squared, 2) ) ) ) +
  geom_line(
      data = data.frame(mass = seq(min(d$mass), max(d$mass), length.out = 100) ) %>%
          mutate(pred = predict(mod1.5, newdata = .) ), aes(x = mass, y = pred) ) +
  geom_hline(yintercept = 0, linetype = 2)

p6 <- 
  ggplot(data = d, aes(x = mass, y = brain) ) +
  geom_point() +
  theme_bw(base_size = 20) +
  ylim(-400, 2000) +
  ggtitle(bquote(R^'2'~'='~.(round(summary(mod1.6)$r.squared, 2) ) ) ) +
  geom_line(
      data = data.frame(mass = seq(min(d$mass), max(d$mass), length.out = 100) ) %>%
          mutate(pred = predict(mod1.6, newdata = .) ), aes(x = mass, y = pred) ) +
  geom_hline(yintercept = 0, linetype = 2)

p5 + p6


## ----eval = TRUE, echo = TRUE------------------------------------------------------------------------------------------
mod1.7 <- lm(brain ~ 1, data = d)


## ----eval = TRUE, echo = FALSE, fig.width = 6, fig.height = 6----------------------------------------------------------
d %>%
  ggplot(aes(x = mass, y = brain) ) +
  geom_point() +
  theme_bw(base_size = 20) +
  ylim(-400, 2000) +
  ggtitle(bquote(R^'2'~'='~.(round(summary(mod1.7)$r.squared, 2) ) ) ) +
  geom_line(
      data = data.frame(mass = seq(min(d$mass), max(d$mass), length.out = 100) ) %>%
          mutate(pred = predict(mod1.7, newdata = .) ), aes(x = mass, y = pred) ) +
  geom_hline(yintercept = 0, linetype = 2)


## ----eval = TRUE, echo = TRUE------------------------------------------------------------------------------------------
p <- c(0.3, 0.7)
- sum(p * log(p) )


## ----eval = TRUE, echo = TRUE------------------------------------------------------------------------------------------
p <- c(0.01, 0.99)
- sum(p * log(p) )


## ----eval = TRUE, echo = TRUE------------------------------------------------------------------------------------------
p <- c(0.3, 0.7)
q <- c(0.25, 0.75)

sum(p * log(p / q) )
sum(q * log(q / p) ) # NB : La divergence n'est pas symétrique...


## ----echo = FALSE, out.width = "75%"-----------------------------------------------------------------------------------
knitr::include_graphics("figures/divergence.gif")


## ----eval = FALSE, echo = TRUE-----------------------------------------------------------------------------------------
## sum(p * (log(q) ) )


## ----eval = TRUE, echo = TRUE------------------------------------------------------------------------------------------
- sum (p * (log(q) - log(p) ) )


## ----eval = TRUE, echo = FALSE-----------------------------------------------------------------------------------------
knitr::include_graphics("figures/mind_blowing.jpg")


## ----eval = TRUE, echo = FALSE-----------------------------------------------------------------------------------------
knitr::include_graphics("figures/KL_distance.png")


## ----eval = TRUE, echo = TRUE------------------------------------------------------------------------------------------
d$mass.s <- scale(d$mass)
mod1.8 <- lm(brain ~ mass.s, data = d)

-2 * logLik(mod1.8) # calcul de la déviance


## ----eval = TRUE, echo = TRUE------------------------------------------------------------------------------------------
# paramètres estimés (intercept et pente)

alpha <- coef(mod1.8)[1]
beta <- coef(mod1.8)[2]

# calcul de la log-vraisemblance

ll <- sum(dnorm(
    d$brain,
    mean = alpha + beta * d$mass.s,
    sd = sd(residuals(mod1.8) ),
    log = TRUE)
    )

# calcul de la déviance

(-2) * ll


## ----eval = TRUE, echo = FALSE, out.width = "60%"----------------------------------------------------------------------
knitr::include_graphics("figures/inout1.png")


## ----eval = TRUE, echo = FALSE, fig.width = 10, fig.height = 6---------------------------------------------------------
data.frame(x = c(-3, 3) ) %>%
    ggplot(aes(x = x) ) +
    stat_function(
        fun = dnorm, args = list(mean = 0, sd = 0.25),
        size = 1.5, linetype = 1) +
    stat_function(
        fun = dnorm, args = list(mean = 0, sd = 0.5),
        size = 1.5, linetype = 2) +
    stat_function(
        fun = dnorm, args = list(mean = 0, sd = 1),
        size = 1.5, linetype = 3) +
    theme_bw(base_size = 20) +
    labs(x = expression(theta), y = "Densité de probabilité")


## ----eval = TRUE, echo = FALSE, out.width = "60%"----------------------------------------------------------------------
knitr::include_graphics("figures/inout2.png")


## ----eval = TRUE, echo = FALSE, out.width = "60%"----------------------------------------------------------------------
knitr::include_graphics("figures/inout3.png")


## ----echo = FALSE, fig.align = "center", out.width = "400px"-----------------------------------------------------------
knitr::include_graphics("figures/mind_blowing2.gif")


## ----eval = TRUE, echo = TRUE, results = "hide"------------------------------------------------------------------------
library(brms)
data(cars)

priors <- c(
  set_prior("normal(0, 100)", class = "Intercept"),
  set_prior("normal(0, 10)", class = "b"),
  set_prior("exponential(0.1)", class = "sigma")
  )

mod1 <- brm(
  dist ~ 1 + speed,
  prior = priors,
  data = cars
  )


## ----eval = TRUE, echo = TRUE------------------------------------------------------------------------------------------
n_obs <- nrow(cars) # number of observations

ll <-
  log_lik(mod1) %>% # pointwise log-likelihood (S rows * N observations)
  data.frame() %>% # converts to dataframe
  set_names(c(str_c(0, 1:9), 10:n_obs) ) # renaming columns

(lppd <- 
  ll %>% 
  pivot_longer(
    everything(), # for all columns
    names_to = "i",
    values_to = "loglikelihood"
    ) %>%
  # log-likelihood to likelihood
  mutate(likelihood = exp(loglikelihood) ) %>% 
  group_by(i) %>%
  # taking the log of the average likelihood
  summarise(log_mean_likelihood = mean(likelihood) %>% log() ) %>%
  # computing the sum of these values
  summarise(lppd = sum(log_mean_likelihood) ) %>%
  pull(lppd) )


## ----eval = TRUE, echo = TRUE------------------------------------------------------------------------------------------
(pwaic <-
  ll %>% 
  pivot_longer(
    everything(),
    names_to = "i",
    values_to = "loglikelihood"
    ) %>% 
  group_by(i) %>% 
  summarise(var_loglikelihood = var(loglikelihood) ) %>%
  summarise(pwaic = sum(var_loglikelihood) ) %>% 
  pull() )


## ----eval = TRUE, echo = TRUE------------------------------------------------------------------------------------------
(WAIC <- -2 * (lppd - pwaic) )


## ----eval = TRUE, echo = TRUE------------------------------------------------------------------------------------------
waic(mod1)


## ----echo = FALSE, out.width = "25%"-----------------------------------------------------------------------------------
knitr::include_graphics("figures/combining.png")


## ----eval = TRUE, echo = TRUE------------------------------------------------------------------------------------------
data(milk)

d <- milk[complete.cases(milk), ] # removing NAs
d$neocortex <- d$neocortex.perc / 100 # rescaling explanatory variable

head(d)


## ----eval = TRUE, echo = TRUE, results = "hide"------------------------------------------------------------------------
mod2.1 <- brm(
  kcal.per.g ~ 1,
  family = gaussian,
  data = d,
  prior = c(
    brms::prior(normal(0, 100), class = Intercept),
    brms::prior(exponential(0.01), class = sigma)
    ),
  iter = 2000, warmup = 1000,
  chains = 4, cores = 4
  )

mod2.2 <- brm(
  kcal.per.g ~ 1 + neocortex,
  family = gaussian,
  data = d,
  prior = c(
    brms::prior(normal(0, 100), class = Intercept),
    brms::prior(normal(0, 10), class = b),
    brms::prior(exponential(0.01), class = sigma)
    ),
  iter = 2000, warmup = 1000,
  chains = 4, cores = 4
  )


## ----eval = TRUE, echo = TRUE, results = "hide"------------------------------------------------------------------------
mod2.3 <- update(
  mod2.2,
  newdata = d,
  formula = kcal.per.g ~ 1 + log(mass)
  )

mod2.4 <- update(
  mod2.3,
  newdata = d,
  formula = kcal.per.g ~ 1 + neocortex + log(mass)
  )


## ----eval = TRUE, echo = TRUE------------------------------------------------------------------------------------------
# calcul du WAIC et ajout du WAIC à chaque modèle

mod2.1 <- add_criterion(mod2.1, "waic")
mod2.2 <- add_criterion(mod2.2, "waic")
mod2.3 <- add_criterion(mod2.3, "waic")
mod2.4 <- add_criterion(mod2.4, "waic")

# comparaison des WAIC de chaque modèle

w <- loo_compare(mod2.1, mod2.2, mod2.3, mod2.4, criterion = "waic")
print(w, simplify = FALSE)


## ----eval = TRUE, echo = TRUE------------------------------------------------------------------------------------------
model_weights(mod2.1, mod2.2, mod2.3, mod2.4, weights = "waic") %>% round(digits = 3)


## ----eval = TRUE, echo = TRUE, results = "hide"------------------------------------------------------------------------
# grille de valeurs pour lesquelles on va générer des prédictions
new_data <- data.frame(
  neocortex = seq(from = 0.5, to = 0.8, length.out = 30),
  mass = 4.5
  )

# prédictions du modèle mod2.4
f <- fitted(mod2.4, newdata = new_data) %>%
  as.data.frame() %>%
  bind_cols(new_data)

# prédictions moyennées sur les 4 modèles
averaged_predictions <- pp_average(
  mod2.1, mod2.2, mod2.3, mod2.4,
  weights = "waic",
  method  = "fitted",
  newdata = new_data
  ) %>%
  as.data.frame() %>%
  bind_cols(new_data)


## ----eval = TRUE, echo = FALSE, fig.width = 9, fig.height = 6----------------------------------------------------------
averaged_predictions %>%
  ggplot(aes(x = neocortex, y = Estimate) ) +
  geom_ribbon(
    aes(ymin = Q2.5, ymax = Q97.5),
    alpha = 0.25
    ) +
  geom_line() +
  geom_ribbon(
    data = f,
    aes(ymin = Q2.5, ymax = Q97.5),
    color = "steelblue",
    fill = "steelblue",
    alpha = 0.5,
    linetype = 2
    ) +
  geom_line(
    data = f,
    color = "steelblue",
    linetype = 2
    ) +
  geom_point(
    data = d,
    aes(y = kcal.per.g), 
    size = 2
    ) +
  labs(y = "kcal.per.g") +
  coord_cartesian(xlim = range(d$neocortex), ylim = range(d$kcal.per.g) ) +
  theme_bw(base_size = 20)


## ----eval = TRUE, echo = TRUE, fig.width = 6, fig.height = 6-----------------------------------------------------------
bayes_R2(mod2.4) %>% round(digits = 3)
plotPost(bayes_R2(mod2.4, summary = FALSE) )


## ----eval = TRUE, echo = TRUE------------------------------------------------------------------------------------------
plotPost(bayes_R2(mod2.4, summary = FALSE) - bayes_R2(mod2.3, summary = FALSE) )


## ----eval = TRUE, echo = TRUE------------------------------------------------------------------------------------------
data(Howell1)
d <- Howell1 %>% mutate(age = scale(age) )

set.seed(666)
i <- sample(1:nrow(d), size = nrow(d) / 2)

d1 <- d[i, ] # échantillon d'entraînement
d2 <- d[-i, ] # échantillon de test


## ----eval = TRUE, echo = TRUE, results = "hide"------------------------------------------------------------------------
mod3.1 <- brm(
  height ~ 1 + age,
  family = gaussian(),
  data = d1,
  prior = c(
    brms::prior(normal(0, 100), class = Intercept),
    brms::prior(exponential(0.01), class = sigma)
    )
  )

mod3.2 <- update(
  mod3.1,
  newdata = d1,
  formula = height ~ 1 + age + I(age^2)
  )

mod3.3 <- update(
  mod3.1,
  newdata = d1,
  formula = height ~ 1 + age + I(age^2) + I(age^3)
  )


## ----eval = TRUE, echo = TRUE, results = "hide"------------------------------------------------------------------------
mod3.4 <- update(
  mod3.1,
  newdata = d1,
  formula = height ~ 1 + age + I(age^2) + I(age^3) + I(age^4)
  )

mod3.5 <- update(
  mod3.1,
  newdata = d1,
  formula = height ~ 1 + age + I(age^2) + I(age^3) + I(age^4) + I(age^5)
  )

mod3.6 <- update(
  mod3.1,
  newdata = d1,
  formula = height ~ 1 + age + I(age^2) + I(age^3) + I(age^4) + I(age^5) + I(age^6)
  )


## ----eval = TRUE, echo = TRUE------------------------------------------------------------------------------------------
# calcul du WAIC et ajout du WAIC à chaque modèle

mod3.1 <- add_criterion(mod3.1, "waic")
mod3.2 <- add_criterion(mod3.2, "waic")
mod3.3 <- add_criterion(mod3.3, "waic")
mod3.4 <- add_criterion(mod3.4, "waic")
mod3.5 <- add_criterion(mod3.5, "waic")
mod3.6 <- add_criterion(mod3.6, "waic")

# comparaison des WAIC de chaque modèle

mod_comp <- loo_compare(mod3.1, mod3.2, mod3.3, mod3.4, mod3.5, mod3.6, criterion = "waic")
print(mod_comp, digits = 2, simplify = FALSE)


## ----eval = TRUE, echo = TRUE------------------------------------------------------------------------------------------
# on crée un vecteur de valeurs possibles pour "age"
age_seq <- data.frame(age = seq(from = -2, to = 3, length.out = 1e2) )

# on récupère les prédictions du modèle pour ces valeurs
mu <- data.frame(fitted(mod3.1, newdata = age_seq) ) %>% bind_cols(age_seq)

# on récupère les prédictions du modèle pour ces valeurs
pred_age <- data.frame(predict(mod3.1, newdata = age_seq) ) %>% bind_cols(age_seq)

# on affiche les dix premières prédictions
head(pred_age, 10)


## ----eval = TRUE, echo = TRUE, fig.width = 10, fig.height = 5----------------------------------------------------------
d1 %>%
  ggplot(aes(x = age, y = height) ) +
  geom_ribbon(
    data = mu, aes(x = age, ymin = Q2.5, ymax = Q97.5),
    alpha = 0.8, inherit.aes = FALSE
    ) +
  geom_smooth(
    data = pred_age, aes(y = Estimate, ymin = Q2.5, ymax = Q97.5),
    stat = "identity", color = "black", alpha = 0.5, size = 1
    ) +
  geom_point(colour = "white", fill = "black", pch = 21, size = 3, alpha = 0.8) +
  theme_bw(base_size = 20) + labs(x = "Age", y = "Height")


## ----eval = TRUE, echo = FALSE, fig.width = 16, fig.height = 8---------------------------------------------------------
# on crée un vecteur de valeurs possibles pour "age"
age_seq <- data.frame(age = seq(from = -2, to = 3, length.out = 1e2) )

# on récupère les prédictions du modèle pour ces valeurs
mu <- data.frame(fitted(mod3.1, newdata = age_seq) ) %>% bind_cols(age_seq)

# on récupère les prédictions du modèle pour ces valeurs
pred_age <- data.frame(predict(mod3.1, newdata = age_seq) ) %>% bind_cols(age_seq)

p1 <- d1 %>%
  ggplot(aes(x = age, y = height) ) +
  geom_ribbon(
    data = mu, aes(x = age, ymin = Q2.5, ymax = Q97.5),
    alpha = 0.8, inherit.aes = FALSE
    ) +
  geom_smooth(
    data = pred_age, aes(y = Estimate, ymin = Q2.5, ymax = Q97.5),
    stat = "identity", color = "black", alpha = 0.5, size = 1
    ) +
  geom_point(colour = "white", fill = "black", pch = 21, size = 3, alpha = 0.8) +
  theme_bw(base_size = 20) +
  labs(x = "Age", y = "Height", title = "Predictions of mod3.1")

# on récupère les prédictions du modèle pour ces valeurs
mu <- data.frame(fitted(mod3.2, newdata = age_seq) ) %>% bind_cols(age_seq)

# on récupère les prédictions du modèle pour ces valeurs
pred_age <- data.frame(predict(mod3.2, newdata = age_seq) ) %>% bind_cols(age_seq)

p2 <- d1 %>%
  ggplot(aes(x = age, y = height) ) +
  geom_ribbon(
    data = mu, aes(x = age, ymin = Q2.5, ymax = Q97.5),
    alpha = 0.8, inherit.aes = FALSE
    ) +
  geom_smooth(
    data = pred_age, aes(y = Estimate, ymin = Q2.5, ymax = Q97.5),
    stat = "identity", color = "black", alpha = 0.5, size = 1
    ) +
  geom_point(colour = "white", fill = "black", pch = 21, size = 3, alpha = 0.8) +
  theme_bw(base_size = 20) +
  labs(x = "Age", y = "Height", title = "Predictions of mod3.2")

# on récupère les prédictions du modèle pour ces valeurs
mu <- data.frame(fitted(mod3.3, newdata = age_seq) ) %>% bind_cols(age_seq)

# on récupère les prédictions du modèle pour ces valeurs
pred_age <- data.frame(predict(mod3.3, newdata = age_seq) ) %>% bind_cols(age_seq)

p3 <- d1 %>%
  ggplot(aes(x = age, y = height) ) +
  geom_ribbon(
    data = mu, aes(x = age, ymin = Q2.5, ymax = Q97.5),
    alpha = 0.8, inherit.aes = FALSE
    ) +
  geom_smooth(
    data = pred_age, aes(y = Estimate, ymin = Q2.5, ymax = Q97.5),
    stat = "identity", color = "black", alpha = 0.5, size = 1
    ) +
  geom_point(colour = "white", fill = "black", pch = 21, size = 3, alpha = 0.8) +
  theme_bw(base_size = 20) +
  labs(x = "Age", y = "Height", title = "Predictions of mod3.3")

# on récupère les prédictions du modèle pour ces valeurs
mu <- data.frame(fitted(mod3.4, newdata = age_seq) ) %>% bind_cols(age_seq)

# on récupère les prédictions du modèle pour ces valeurs
pred_age <- data.frame(predict(mod3.4, newdata = age_seq) ) %>% bind_cols(age_seq)

p4 <- d1 %>%
  ggplot(aes(x = age, y = height) ) +
  geom_ribbon(
    data = mu, aes(x = age, ymin = Q2.5, ymax = Q97.5),
    alpha = 0.8, inherit.aes = FALSE
    ) +
  geom_smooth(
    data = pred_age, aes(y = Estimate, ymin = Q2.5, ymax = Q97.5),
    stat = "identity", color = "black", alpha = 0.5, size = 1
    ) +
  geom_point(colour = "white", fill = "black", pch = 21, size = 3, alpha = 0.8) +
  theme_bw(base_size = 20) +
  labs(x = "Age", y = "Height", title = "Predictions of mod3.4")

# on récupère les prédictions du modèle pour ces valeurs
mu <- data.frame(fitted(mod3.5, newdata = age_seq) ) %>% bind_cols(age_seq)

# on récupère les prédictions du modèle pour ces valeurs
pred_age <- data.frame(predict(mod3.5, newdata = age_seq) ) %>% bind_cols(age_seq)

p5 <- d1 %>%
  ggplot(aes(x = age, y = height) ) +
  geom_ribbon(
    data = mu, aes(x = age, ymin = Q2.5, ymax = Q97.5),
    alpha = 0.8, inherit.aes = FALSE
    ) +
  geom_smooth(
    data = pred_age, aes(y = Estimate, ymin = Q2.5, ymax = Q97.5),
    stat = "identity", color = "black", alpha = 0.5, size = 1
    ) +
  geom_point(colour = "white", fill = "black", pch = 21, size = 3, alpha = 0.8) +
  theme_bw(base_size = 20) +
  labs(x = "Age", y = "Height", title = "Predictions of mod3.5")

# on récupère les prédictions du modèle pour ces valeurs
mu <- data.frame(fitted(mod3.6, newdata = age_seq) ) %>% bind_cols(age_seq)

# on récupère les prédictions du modèle pour ces valeurs
pred_age <- data.frame(predict(mod3.6, newdata = age_seq) ) %>% bind_cols(age_seq)

p6 <- d1 %>%
  ggplot(aes(x = age, y = height) ) +
  geom_ribbon(
    data = mu, aes(x = age, ymin = Q2.5, ymax = Q97.5),
    alpha = 0.8, inherit.aes = FALSE
    ) +
  geom_smooth(
    data = pred_age, aes(y = Estimate, ymin = Q2.5, ymax = Q97.5),
    stat = "identity", color = "black", alpha = 0.5, size = 1
    ) +
  geom_point(colour = "white", fill = "black", pch = 21, size = 3, alpha = 0.8) +
  theme_bw(base_size = 20) +
  labs(x = "Age", y = "Height", title = "Predictions of mod3.6")

(p1 + p2 + p3) / (p4 + p5 + p6)


## ----eval = TRUE, echo = TRUE------------------------------------------------------------------------------------------
# prédictions moyennées sur les 4 modèles
averaged_predictions_mu <- pp_average(
  mod3.1, mod3.2, mod3.3, mod3.4, mod3.5, mod3.6,
  weights = "waic",
  method  = "fitted",
  newdata = age_seq
  ) %>%
  as.data.frame() %>%
  bind_cols(age_seq)

# prédictions moyennées sur les 4 modèles
averaged_predictions_age <- pp_average(
  mod3.1, mod3.2, mod3.3, mod3.4, mod3.5, mod3.6,
  weights = "waic",
  method  = "predict",
  newdata = age_seq
  ) %>%
  as.data.frame() %>%
  bind_cols(age_seq)


## ----eval = TRUE, echo = TRUE, fig.width = 10, fig.height = 5----------------------------------------------------------
d1 %>%
  ggplot(aes(x = age, y = height) ) +
  geom_ribbon(
    data = averaged_predictions_mu, aes(x = age, ymin = Q2.5, ymax = Q97.5),
    alpha = 0.8, inherit.aes = FALSE
    ) +
  geom_smooth(
    data = averaged_predictions_age, aes(y = Estimate, ymin = Q2.5, ymax = Q97.5),
    stat = "identity", color = "black", alpha = 0.5, size = 1
    ) +
  geom_point(colour = "white", fill = "black", pch = 21, size = 3, alpha = 0.8) +
  theme_bw(base_size = 20) + labs(x = "Age", y = "Height", title = "Model-averaged predictions")


## ----eval = TRUE, echo = TRUE, results = "hide"------------------------------------------------------------------------
# extract log-likelihood of mod3.1
# each row is an iteration and each column is an observation
log_lik_mod3.1 <- log_lik(mod3.1)

# NB: the deviance has a distribution too in the Bayesian world
dev.mod3.1 <- mean(-2 * rowSums(log_lik_mod3.1) )

# model 3.2
dev.mod3.2 <- mean(-2 * rowSums(log_lik(mod3.2) ) )

# model 3.3
dev.mod3.3 <- mean(-2 * rowSums(log_lik(mod3.3) ) )

# model 3.4
dev.mod3.4 <- mean(-2 * rowSums(log_lik(mod3.4) ) )

# model 3.5
dev.mod3.5 <- mean(-2 * rowSums(log_lik(mod3.5) ) )

# model 3.6
dev.mod3.6 <- mean(-2 * rowSums(log_lik(mod3.6) ) )


## ----eval = TRUE, echo = TRUE------------------------------------------------------------------------------------------
deviances <- c(dev.mod3.1, dev.mod3.2, dev.mod3.3, dev.mod3.4, dev.mod3.5, dev.mod3.6)
comparison <- mod_comp %>% data.frame %>% select(waic) %>% rownames_to_column()
waics <- comparison %>% arrange(rowname) %>% pull(waic)


## ----eval = TRUE, echo = FALSE, fig.width = 14, fig.height = 7---------------------------------------------------------
data.frame(deviance = deviances, waic = waics) %>%
    gather(type, value) %>%
    mutate(x = rep(1:6, 2) ) %>%
    ggplot(aes(x = x, y = value, colour = type) ) +
    scale_colour_grey() +
    geom_point(size = 2) +
    geom_line(linetype = 2) +
    geom_line(aes(group = x) ) +
    scale_x_continuous(breaks = 1:6) +
    theme_bw(base_size = 20) + labs(x = "Modèle", y = "Déviance - WAIC")

