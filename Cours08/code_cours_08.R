## ----setup, include = FALSE--------------------------------------------------------------------------------------------
library(rethinking)
library(tidyverse)
library(patchwork)
library(knitr)
library(BEST)
library(brms)

# setting up knitr options
opts_chunk$set(
  cache = TRUE, echo = TRUE, warning = FALSE, message = FALSE,
  fig.align = "center", dev = "svg"
  )


## ----echo = FALSE, out.width = "400px"---------------------------------------------------------------------------------
knitr::include_graphics("figures/robot.png")


## ----eval = TRUE, echo = TRUE------------------------------------------------------------------------------------------
library(tidyverse)

df <- read.csv("data/robot.csv")
head(df, 15)


## ----eval = TRUE, echo = TRUE, fig.width = 15, fig.height = 5----------------------------------------------------------
df %>%
  ggplot(aes(x = factor(cafe), y = wait, fill = factor(afternoon) ) ) +
  geom_dotplot(
    stackdir = "center", binaxis = "y",
    dotsize = 1, show.legend = FALSE
    ) +
  geom_hline(yintercept = mean(df$wait), linetype = 3) +
  facet_wrap(~afternoon, ncol = 2) +
  theme_bw(base_size = 20) +
  labs(x = "Café", y = "Temps d'attente (en minutes)")


## ----eval = TRUE, echo = TRUE, ig.width = 7.5, fig.height = 5----------------------------------------------------------
ggplot(data = data.frame(x = c(0, 10) ), aes(x = x) ) +
    stat_function(
        fun = dcauchy,
        args = list(location = 0, scale = 2), size = 1.5
        ) +
    theme_bw(base_size = 20)


## ----eval = TRUE, echo = TRUE, results = "hide"------------------------------------------------------------------------
library(rethinking)
library(brms)

mod1 <- brm(
  wait ~ 1,
  prior = c(
    set_prior("normal(5, 10)", class = "Intercept"),
    set_prior("cauchy(0, 2)", class = "sigma")
    ),
  data = df,
  cores = parallel::detectCores()
  )


## ----eval = TRUE, echo = TRUE------------------------------------------------------------------------------------------
posterior_summary(mod1, probs = c(0.025, 0.975) )


## ----eval = TRUE, echo = TRUE, fig.width = 14, fig.height = 7----------------------------------------------------------
plot(mod1, combo = c("dens_overlay", "trace"), theme = theme_bw(base_size = 16) )


## ----eval = TRUE, echo = TRUE, results = "hide"------------------------------------------------------------------------
mod2 <- brm(
  wait ~ 0 + factor(cafe),
  prior = c(
    set_prior("normal(5, 10)", class = "b"),
    set_prior("cauchy(0, 2)", class = "sigma")
    ),
  data = df,
  cores = parallel::detectCores()
  )


## ----eval = TRUE, echo = TRUE------------------------------------------------------------------------------------------
posterior_summary(mod2)


## ----eval = TRUE, echo = TRUE, out.width = "33%"-----------------------------------------------------------------------
y1 <- rnorm(1e4, 10, 1)
y2 <- 10 + rnorm(1e4, 0, 1)

dens(y1)
dens(y2, add = TRUE)


## ----eval = TRUE, echo = TRUE, results = "hide"------------------------------------------------------------------------
mod3 <- brm(
  wait ~ 1 + (1 | cafe),
  prior = c(
    set_prior("normal(5, 10)", class = "Intercept"),
    set_prior("cauchy(0, 2)", class = "sigma"),
    set_prior("cauchy(0, 2)", class = "sd")
    ),
  data = df,
  warmup = 1000, iter = 5000,
  cores = parallel::detectCores()
  )


## ----echo = FALSE, fig.width = 12, fig.height = 8----------------------------------------------------------------------
library(wesanderson) # for plotting
post <- posterior_samples(mod3) # extracts posterior samples

df %>%
    group_by(cafe) %>%
    summarise(Observed = mean(wait) ) %>%
    mutate(Estimated = coef(mod3)$cafe[, , ] %>% data.frame %>% pull(Estimate) ) %>%
    gather(type, Observed, Observed:Estimated) %>%
    ggplot(aes(x = cafe, y = Observed, fill = type) ) +
    geom_hline(yintercept = mean(post$b_Intercept), linetype = 2) +
    geom_point(pch = 21, size = 5, alpha = 0.8, colour = "white", show.legend = TRUE) +
    scale_color_manual(values = rev(wes_palette(n = 2, name = "Chevalier1") ) )  +
    scale_fill_manual(values = rev(wes_palette(n = 2, name = "Chevalier1") ) )  +
    scale_x_continuous(name = "Café", breaks = 1:20) +
    theme_bw(base_size = 20) +
    ylab("Temps d'attente (en minutes)") +
    theme(legend.title = element_blank() )


## ----echo = FALSE, fig.align = "center", out.width = "66%"-------------------------------------------------------------
knitr::include_graphics("figures/stein1.png")


## ----echo = FALSE, fig.align = "center", out.width = "75%"-------------------------------------------------------------
knitr::include_graphics("figures/stein2.png")


## ----eval = TRUE, echo = TRUE------------------------------------------------------------------------------------------
# calcul du WAIC et ajout du WAIC à chaque modèle
mod1 <- add_criterion(mod1, "waic")
mod2 <- add_criterion(mod2, "waic")
mod3 <- add_criterion(mod3, "waic")

# comparaison des WAIC de chaque modèle
w <- loo_compare(mod1, mod2, mod3, criterion = "waic")
print(w, simplify = FALSE)


## ----eval = TRUE, echo = TRUE------------------------------------------------------------------------------------------
posterior_summary(mod1, pars = c("^b", "sigma") )
posterior_summary(mod3, pars = c("^b", "sigma") )


## ----eval = TRUE, echo = TRUE, results = "hide"------------------------------------------------------------------------
df2 <- read.csv("data/robot_inequal.csv")

mod4 <- brm(
  wait ~ 1 + (1 | cafe),
  prior = c(
    set_prior("normal(5, 10)", class = "Intercept"),
    set_prior("cauchy(0, 2)", class = "sigma"),
    set_prior("cauchy(0, 2)", class = "sd")
    ),
  data = df2,
  warmup = 1000, iter = 5000,
  cores = parallel::detectCores()
  )


## ----echo = FALSE, fig.width = 12, fig.height = 6----------------------------------------------------------------------
post <- posterior_samples(mod4)

df2 %>%
    group_by(cafe) %>%
    summarise(Observed = mean(wait) ) %>%
    mutate(Estimated = coef(mod4)$cafe[, , ] %>% data.frame %>% pull(Estimate) ) %>%
    gather(type, Observed, Observed:Estimated) %>%
    ggplot(aes(x = cafe, y = Observed, fill = type) ) +
    geom_hline(yintercept = mean(post$b_Intercept), linetype = 2) +
    geom_point(pch = 21, size = 5, alpha = 0.8, colour = "white", show.legend = TRUE) +
    scale_color_manual(values = rev(wes_palette(n = 2, name = "Chevalier1") ) )  +
    scale_fill_manual(values = rev(wes_palette(n = 2, name = "Chevalier1") ) )  +
    scale_x_continuous(name = "Café (du moins visité au plus visité)", breaks = 1:20) +
    theme_bw(base_size = 20) +
    ylab("Temps d'attente (en minutes)") +
    theme(legend.title = element_blank() )


## ----echo = FALSE, out.width = "800px"---------------------------------------------------------------------------------
knitr::include_graphics("figures/bivariate.png")


## ----eval = TRUE, echo = TRUE------------------------------------------------------------------------------------------
sigma_a <- 1
sigma_b <- 0.75
rho <- 0.7
cov_ab <- sigma_a * sigma_b * rho
(Sigma1 <- matrix(c(sigma_a^2, cov_ab, cov_ab, sigma_b^2), ncol = 2) )


## ----eval = TRUE, echo = TRUE------------------------------------------------------------------------------------------
(sigmas <- c(sigma_a, sigma_b) ) # standard deviations
(Rho <- matrix(c(1, rho, rho, 1), nrow = 2) ) # correlation matrix
(Sigma2 <- diag(sigmas) %*% Rho %*% diag(sigmas) )


## ---- echo = FALSE, fig.width = 14, fig.height = 7, cache = TRUE-------------------------------------------------------
# nsims <- 1e6
# 
# data.frame(
#     zeta05 = rethinking::rlkjcorr(nsims, K = 2, eta = 0.5)[, 1, 2],
#     zeta1 = rethinking::rlkjcorr(nsims, K = 2, eta = 2)[, 1, 2],
#     zeta5 = rethinking::rlkjcorr(nsims, K = 2, eta = 5)[, 1, 2],
#     zeta50 = rethinking::rlkjcorr(nsims, K = 2, eta = 10)[, 1, 2] ) %>%
#     gather(shape, y, zeta05:zeta50) %>%
#     ggplot(aes(x = y, colour = shape) ) +
#     geom_line(stat = "density", position = "identity", size = 1) +
#     labs(x = expression(rho), y = "Densité de probabilité") +
#     theme_bw(base_size = 18) +
#     scale_colour_manual(
#         values = c("steelblue", "orangered", "purple", "darkgreen"),
#         labels = c(
#             expression(paste(zeta, " = ", "0.5") ),
#             expression(paste(zeta, " = ", "2") ),
#             expression(paste(zeta, " = ", "10") ),
#             expression(paste(zeta, " = ", "50") )
#             )
#         ) +
#     theme(
#         legend.text.align = 0,
#         legend.position = c(0.75, 0.8),
#         legend.background = element_rect(size = 0.5, colour = "black")
#         )

library(ggdist)

expand.grid(eta = c(0.5, 2, 5, 10), K = c(2, 3, 4, 5) ) %>%
  ggplot(aes(y = ordered(eta), dist = "lkjcorr_marginal", arg1 = K, arg2 = eta, fill = as.factor(eta) ) ) +
  stat_dist_slab(p_limits = c(0, 1), alpha = 0.8) +
  facet_grid(~paste0(K, "x", K) ) +
  labs(x = expression(rho), y = "Densité de probabilité (par prior)") +
  theme_bw(base_size = 18) +
  scale_fill_manual(
    values = c("steelblue", "orangered", "purple", "darkgreen"),
    labels = c(
      expression(paste(zeta, " = ", "0.5") ),
      expression(paste(zeta, " = ", "2") ),
      expression(paste(zeta, " = ", "10") ),
      expression(paste(zeta, " = ", "50") )
    )
  ) +
  theme(
    legend.title = element_blank(),
    legend.text.align = 0,
    legend.background = element_rect(size = 0.5, colour = "black")
  )


## ----eval = FALSE, echo = TRUE-----------------------------------------------------------------------------------------
## Reaction ~ Days + (1 + Days | Subject)


## ----eval = FALSE, echo = TRUE-----------------------------------------------------------------------------------------
## c(Reaction, Memory) ~ Days + (1 + Days | Subject)
## c(Reaction, Memory) ~ 1 + Days + (1 + Days | Subject)


## ----eval = FALSE, echo = TRUE-----------------------------------------------------------------------------------------
## c(Reaction, Memory) ~ Days + (1 | Subject)
## c(Reaction, Memory) ~ Days + (Days | Subject)


## ----eval = FALSE, echo = TRUE-----------------------------------------------------------------------------------------
## c(Reaction, Memory) ~ Days + (1 + Days || Subject)


## ----eval = FALSE, echo = TRUE-----------------------------------------------------------------------------------------
## brm(Reaction ~ 1 + Days + (1 + Days | Subject), family = lognormal() )


## ----eval = TRUE, echo = TRUE, results = "hide"------------------------------------------------------------------------
mod5 <- brm(
  wait ~ 1 + afternoon + (1 + afternoon | cafe),
  prior = c(
    set_prior("normal(0, 10)", class = "Intercept"),
    set_prior("normal(0, 10)", class = "b"),
    set_prior("cauchy(0, 2)", class = "sigma"),
    set_prior("cauchy(0, 2)", class = "sd")
    ),
  data = df,
  warmup = 1000, iter = 5000,
  cores = parallel::detectCores()
  )


## ----eval = TRUE, echo = TRUE, fig.width = 9, fig.height = 6-----------------------------------------------------------
post <- posterior_samples(mod5) # extracts posterior samples
R <- rlkjcorr(16000, K = 2, eta = 2) # samples from prior

data.frame(prior = R[, 1, 2], posterior = post$cor_cafe__Intercept__afternoon) %>%
    gather(type, value, prior:posterior) %>%
    ggplot(aes(x = value, color = type, fill = type) ) +
    geom_histogram(position = "identity", alpha = 0.2) +
    theme_bw(base_size = 20) +
    labs(x = expression(rho), y = "Nombre d'échantillons")


## ----eval = TRUE, echo = FALSE, fig.width = 12, fig.height = 8---------------------------------------------------------
a1 <- sapply(1:20, function(i) mean(df$wait[df$cafe == i & df$afternoon == 0]) )
b1 <- sapply(1:20, function(i) mean(df$wait[df$cafe == i & df$afternoon == 1]) ) - a1

no_pooling <-
  data.frame(Intercept = a1, afternoon = b1) %>%
  mutate(model = "no pooling")

partial_pooling <-
  data.frame(coef(mod5)$cafe[, 1, 1:2]) %>%
  mutate(model = "partial pooling")

shrinkage <- bind_rows(no_pooling, partial_pooling)

mu <- c(mean(post$b_Intercept), mean(post$b_afternoon) )
rho <- mean(post$cor_cafe__Intercept__afternoon)
sda <- mean(post$sd_cafe__Intercept)
sdb <- mean(post$sd_cafe__afternoon)
cov_ab <- sda * sdb * rho
sigma <- matrix(c(sda^2, cov_ab, cov_ab, sdb^2), ncol = 2)

##############################################################################
# Helper function to make ellipse, credits to Tristan Mahr                   #
# https://tjmahr.github.io/plotting-partial-pooling-in-mixed-effects-models/ #
##############################################################################

library(ellipse)

make_ellipse <- function(cov_mat, center, level) {
    
    ellipse(cov_mat, centre = center, level = level) %>%
        as.data.frame() %>%
        add_column(level = level)
    
}

levels <- c(.1, .3, .5, .7)

df_ellipse <-
    levels %>%
    purrr::map_df(~ make_ellipse(sigma, mu, level = .x) ) %>% 
    rename(Intercept = x, afternoon = y)

shrinkage %>%
    mutate(id = rep(1:20, 2) ) %>%
    ggplot(aes(x = Intercept, y = afternoon, color = model) ) +
    scale_color_manual(values = wesanderson::wes_palette(n = 2, name = "Chevalier1") ) +
    geom_point(size = 5, show.legend = FALSE) +
    # connecting lines
    geom_path(
        aes(group = id, color = NULL),
        arrow = arrow(length = unit(.015, "npc"), type = "closed"), 
        show.legend = FALSE
        ) +
    # ellipses
    geom_path(
        aes(group = level, color = NULL),
        data = df_ellipse,
        linetype = "dashed", color = "grey40", alpha = 0.8
        ) +
    labs(x = "Intercept", y = "Slope") +
    theme_bw(base_size = 20)


## ----eval = TRUE, echo = TRUE------------------------------------------------------------------------------------------
# comparaison des WAIC de chaque modèle
mod5 <- add_criterion(mod5, "waic")
w <- loo_compare(mod1, mod2, mod3, mod5, criterion = "waic")
print(w, simplify = FALSE)
model_weights(mod1, mod2, mod3, mod5, weights = "waic")


## ----eval = TRUE, echo = TRUE, warning = FALSE-------------------------------------------------------------------------
posterior_summary(mod1, pars = c("^b", "sigma") )
posterior_summary(mod3, pars = c("^b", "sigma") )
posterior_summary(mod5, pars = c("^b", "sigma") )


## ----eval = TRUE, echo = TRUE------------------------------------------------------------------------------------------
library(lme4)

data(sleepstudy)
head(sleepstudy, 20)


## ----eval = TRUE, echo = TRUE, fig.width = 12, fig.height = 6----------------------------------------------------------
sleepstudy %>%
    ggplot(aes(x = Days, y = Reaction) ) +
    geom_smooth(method = "lm", colour = "black") +
    geom_point() +
    facet_wrap(~Subject, nrow = 2) +
    theme_bw(base_size = 20) +
    scale_x_continuous(breaks = c(0, 2, 4, 6, 8) )


## ----eval = TRUE, echo = TRUE------------------------------------------------------------------------------------------
fmod0 <- lm(Reaction ~ Days, sleepstudy)
fmod1 <- lmer(Reaction ~ Days + (1|Subject), sleepstudy)
fmod2 <- lmer(Reaction ~ Days + (1 + Days|Subject), sleepstudy)

anova(fmod1, fmod2)


## ----eval = TRUE, echo = TRUE, results = "hide"------------------------------------------------------------------------
mod6 <- brm(
  Reaction ~ 1 + Days,
  prior = c(
    set_prior("normal(200, 100)", class = "Intercept"),
    set_prior("normal(0, 10)", class = "b"),
    set_prior("cauchy(0, 10)", class = "sigma")
    ),
  data = sleepstudy,
  warmup = 1000, iter = 5000,
  cores = parallel::detectCores()
  )


## ----eval = TRUE, echo = TRUE------------------------------------------------------------------------------------------
posterior_summary(mod6)


## ----eval = TRUE, echo = TRUE, results = "hide"------------------------------------------------------------------------
mod7 <- brm(
  Reaction ~ 1 + Days + (1 | Subject),
  prior = c(
    set_prior("normal(200, 100)", class = "Intercept"),
    set_prior("normal(0, 10)", class = "b"),
    set_prior("cauchy(0, 10)", class = "sigma"),
    set_prior("cauchy(0, 10)", class = "sd")
    ),
  data = sleepstudy,
  warmup = 1000, iter = 5000,
  cores = parallel::detectCores()
  )


## ----eval = TRUE, echo = TRUE------------------------------------------------------------------------------------------
posterior_summary(mod7, pars = c("^b", "sigma") )


## ----eval = TRUE, echo = TRUE, results = "hide"------------------------------------------------------------------------
mod8 <- brm(
  Reaction ~ 1 + Days + (1 + Days | Subject),
  prior = c(
    set_prior("normal(200, 100)", class = "Intercept"),
    set_prior("normal(0, 10)", class = "b"),
    set_prior("cauchy(0, 10)", class = "sigma"),
    set_prior("cauchy(0, 10)", class = "sd")
    ),
  data = sleepstudy,
  warmup = 1000, iter = 5000,
  cores = parallel::detectCores()
  )


## ----eval = TRUE, echo = TRUE------------------------------------------------------------------------------------------
posterior_summary(mod8, pars = c("^b", "sigma") )


## ----eval = TRUE, echo = TRUE------------------------------------------------------------------------------------------
# calcul du WAIC et ajout du WAIC à chaque modèle
mod6 <- add_criterion(mod6, "waic")
mod7 <- add_criterion(mod7, "waic")
mod8 <- add_criterion(mod8, "waic")

# comparaison des WAIC de chaque modèle
w <- loo_compare(mod6, mod7, mod8, criterion = "waic")
print(w, simplify = FALSE)

# calcul du poids relatif de chaque modèle
model_weights(mod6, mod7, mod8, weights = "waic")

