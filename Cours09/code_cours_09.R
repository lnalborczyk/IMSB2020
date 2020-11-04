## ----setup, include = FALSE-------------------------------------------------------------------------------------------
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


## ----eval = FALSE, echo = TRUE----------------------------------------------------------------------------------------
## Reaction ~ Days + (1 + Days | Subject)


## ----eval = FALSE, echo = TRUE----------------------------------------------------------------------------------------
## c(Reaction, Memory) ~ Days + (1 + Days | Subject)


## ----eval = FALSE, echo = TRUE----------------------------------------------------------------------------------------
## c(Reaction, Memory) ~ Days + (1 + Days | Subject)
## c(Reaction, Memory) ~ 1 + Days + (1 + Days | Subject)


## ----eval = FALSE, echo = TRUE----------------------------------------------------------------------------------------
## c(Reaction, Memory) ~ 0 + Days + (1 + Days | Subject)


## ----eval = FALSE, echo = TRUE----------------------------------------------------------------------------------------
## c(Reaction, Memory) ~ Days + (1 | Subject)
## c(Reaction, Memory) ~ Days + (Days | Subject)


## ----eval = FALSE, echo = TRUE----------------------------------------------------------------------------------------
## c(Reaction, Memory) ~ Days + (1 + Days || Subject)


## ----eval = FALSE, echo = TRUE----------------------------------------------------------------------------------------
## brm(Reaction ~ 1 + Days + (1 + Days | Subject), family = lognormal() )


## ----eval = TRUE, echo = TRUE-----------------------------------------------------------------------------------------
library(tidyverse)

data <- read.csv("data/absenteeism.csv")
data %>% sample_frac %>% head(10)


## ----eval = TRUE, echo = TRUE-----------------------------------------------------------------------------------------
prior1 <- c(
    prior(normal(0, 1), class = Intercept, coef = ""),
    prior(normal(0, 1), class = b),
    prior(cauchy(0, 1), class = sd),
    prior(lkj(2), class = cor)
    )


## ----eval = TRUE, echo = TRUE, results = "hide"-----------------------------------------------------------------------
mod1 <- brm(
    presence | trials(total) ~ 1 + reminder + (1 + reminder | researcher), 
    family = binomial(link = "logit"),
    prior = prior1,
    data = data,
    sample_prior = TRUE,
    warmup = 2000, iter = 1e4,
    cores = parallel::detectCores(),
    control = list(adapt_delta = 0.95)
    )


## ----eval = TRUE, echo = TRUE, fig.width = 12, fig.height = 6---------------------------------------------------------
mod1 %>%
    plot(
        combo = c("dens_overlay", "trace"), pars = c("^b_", "^cor_"), widths = c(1, 1.5),
        theme = theme_bw(base_size = 16)
        )


## ----eval = TRUE, echo = TRUE-----------------------------------------------------------------------------------------
posterior_summary(mod1, pars = c("^b_", "^sd_") )


## ----eval = TRUE, echo = TRUE-----------------------------------------------------------------------------------------
a <- fixef(mod1)[1] # extract the intercept
exp(a) / (1 + exp(a) ) # equivalent to plogis(a)


## ----eval = TRUE, echo = TRUE-----------------------------------------------------------------------------------------
fixef(mod1)[2, c(1, 3, 4)] %>% exp


## ---- echo = FALSE, eval = TRUE, message = FALSE, warning = FALSE, fig.width = 12, fig.height = 7---------------------
library(tidybayes)
library(modelr)

data %>%
    group_by(researcher, total) %>%
    data_grid(reminder = seq_range(reminder, n = 1e2) ) %>%
    add_fitted_samples(mod1, newdata = ., n = 100, scale = "linear") %>%
    mutate(estimate = plogis(estimate) ) %>%
    group_by(reminder, .iteration) %>%
    summarise(estimate = mean(estimate) ) %>%
    ggplot(aes(x = reminder, y = estimate, group = .iteration) ) +
    geom_hline(yintercept = 0.5, lty = 2) +
    geom_line(aes(y = estimate, group = .iteration), size = 0.5, alpha = 0.1) +
    theme_bw(base_size = 20) + labs(x = "Mail de rappel", y = "Pr(présent)")


## ---- echo = TRUE, eval = TRUE, message = FALSE, warning = FALSE, fig.width = 16, fig.height = 6----------------------
data %>%
    group_by(researcher, total) %>%
    data_grid(reminder = seq_range(reminder, n = 1e2) ) %>%
    add_fitted_samples(mod1, newdata = ., n = 100, scale = "linear") %>%
    mutate(estimate = plogis(estimate) ) %>%
    ggplot(aes(x = reminder, y = estimate, group = .iteration) ) +
    geom_hline(yintercept = 0.5, lty = 2) +
    geom_line(aes(y = estimate, group = .iteration), size = 0.5, alpha = 0.1) +
    facet_wrap(~researcher, nrow = 2) +
    theme_bw(base_size = 20) + labs(x = "Mail de rappel", y = "Pr(présent)")


## ---- echo = TRUE-----------------------------------------------------------------------------------------------------
(hyp1 <- hypothesis(mod1, "reminder = 0") )
1 / hyp1$hypothesis$Evid.Ratio


## ---- echo = TRUE, fig.width = 10, fig.height = 7---------------------------------------------------------------------
plot(hyp1, plot = FALSE, theme = theme_bw(base_size = 20) )[[1]] +
  geom_vline(xintercept = 0, linetype = 2) +
  coord_cartesian(xlim = c(-5, 5) )


## ---- echo = TRUE, fig.width = 14, fig.height = 6---------------------------------------------------------------------
data.frame(prior = hyp1$prior_samples$H1, posterior = hyp1$samples$H1) %>%
    gather(type, value) %>%
    mutate(type = factor(type, levels = c("prior", "posterior") ) ) %>%
    ggplot(aes(x = value) ) +
    geom_histogram(bins = 50, alpha = 0.8, col = "white", fill = "steelblue") +
    geom_vline(xintercept = 0, lty = 2, size = 1) +
    facet_wrap(~type, scales = "free") +
    labs(x = expression(beta[reminder]), y = "Nombre d'échantillons") +
    theme_bw(base_size = 20)


## ----eval = TRUE, echo = TRUE, results = "hide"-----------------------------------------------------------------------
prior2 <- c(
    prior(normal(0, 10), class = Intercept, coef = ""),
    prior(cauchy(0, 10), class = sd),
    prior(lkj(2), class = cor) )

mod2 <- brm(presence | trials(total) ~ 1 + reminder + (1 + reminder | researcher), 
    family = binomial(link = "logit"),
    prior = prior1,
    data = data,
    # this line is important for bridgesampling
    save_all_pars = TRUE,
    warmup = 2000, iter = 1e4,
    cores = parallel::detectCores(),
    control = list(adapt_delta = 0.95) )

mod3 <- brm(presence | trials(total) ~ 1 + (1 + reminder | researcher), 
    family = binomial(link = "logit"),
    prior = prior2,
    data = data,
    save_all_pars = TRUE,
    warmup = 2000, iter = 1e4,
    cores = parallel::detectCores(),
    control = list(adapt_delta = 0.95) )


## ----eval = TRUE, echo = TRUE-----------------------------------------------------------------------------------------
bayes_factor(mod2, mod3)


## ---- echo = TRUE, eval = TRUE----------------------------------------------------------------------------------------
waic(mod2, mod3, compare = FALSE)


## ---- echo = TRUE, fig.width = 12, fig.height = 6---------------------------------------------------------------------
data %>%
    ggplot(aes(x = presence / total) ) +
    geom_density(fill = "grey20") +
    theme_bw(base_size = 20)


## ---- echo = TRUE, fig.width = 12, fig.height = 6---------------------------------------------------------------------
pp_check(mod2, nsamples = 1e2) + theme_bw(base_size = 20)


## ---- echo = TRUE, fig.width = 12, fig.height = 8---------------------------------------------------------------------
pp_check(mod2, nsamples = 1e3, type = "stat_2d") + theme_bw(base_size = 20)


## ----eval = FALSE, echo = TRUE----------------------------------------------------------------------------------------
## mod2 <- brm(
##     presence | trials(total) ~ 1 + reminder + (1 + reminder|researcher),
##     family = binomial(link = "logit"),
##     prior = prior2,
##     data = data,
##     warmup = 2000, iter = 1e4,
##     cores = parallel::detectCores(), # using all availables cores
##     control = list(adapt_delta = 0.95) # adjusting the delta step size
##     )


## ----eval = TRUE, echo = TRUE-----------------------------------------------------------------------------------------
d <- read.csv("data/meta.csv")
head(d, 15)


## ----echo = FALSE, out.width = "1200px"-------------------------------------------------------------------------------
knitr::include_graphics("figures/meta_structure.png")


## ----eval = TRUE, echo = TRUE, results = "hide"-----------------------------------------------------------------------
prior4 <- c(
    prior(normal(0, 1), coef = intercept),
    prior(cauchy(0, 1), class = sd)
    )

mod4 <- brm(
    yi | se(sqrt(vi) ) ~ 0 + intercept + (1 | study) + (1 | experiment),
    data = d,
    prior = prior4,
    save_all_pars = TRUE,
    warmup = 2000, iter = 1e4,
    cores = parallel::detectCores(),
    control = list(adapt_delta = .99)
    )


## ----eval = TRUE, echo = TRUE-----------------------------------------------------------------------------------------
summary(mod4)


## ----eval = TRUE, echo = TRUE, fig.width = 12, fig.height = 6---------------------------------------------------------
mod4 %>%
  plot(
    pars = c("^b_", "^sd_"),
    combo = c("dens_overlay", "trace"),
    theme = theme_bw(base_size = 16)
    )


## ----eval = TRUE, echo = FALSE, out.width = "50%"---------------------------------------------------------------------
# source("code/fplot2.R")
# fplot2(d, mod4, level = 0.95)
knitr::include_graphics("figures/forest.png")


## ----eval = TRUE, echo = TRUE-----------------------------------------------------------------------------------------
d <- read.csv("data/popular.csv")
head(d, 10)


## ----echo = FALSE, out.width = "500px"--------------------------------------------------------------------------------
knitr::include_graphics("figures/cat.gif")


## ----eval = TRUE, echo = TRUE, fig.width = 10, fig.height = 6---------------------------------------------------------
d %>%
    ggplot(aes(x = popular) ) +
    geom_histogram() +
    facet_wrap(~sex) +
    theme_bw(base_size = 20) +
    scale_x_continuous(breaks = 1:10, limits = c(1, 10) )


## ----eval = TRUE, echo = TRUE, fig.width = 10, fig.height = 6---------------------------------------------------------
d %>%
    ggplot(aes(x = texp, y = popular) ) +
    geom_point(alpha = 0.2) +
    geom_smooth(method = "lm", colour = "black") +
    facet_wrap(~sex) +
    theme_bw(base_size = 20)


## ----eval = TRUE, echo = TRUE, results = "hide"-----------------------------------------------------------------------
library(magrittr)

d %<>%
    mutate(
        # contrast-coding gender
        sex = ifelse(sex == "boy", -0.5, 0.5),
        # centering and standardising teacher experience
        texp = scale(texp) %>% as.numeric
        )

prior5 <- c(
    prior(normal(5, 2.5), class = Intercept),
    prior(cauchy(0, 10), class = sd),
    prior(cauchy(0, 10), class = sigma)
    )

mod5 <- brm(
    popular ~ 1 + (1 | school),
    data = d,
    prior = prior5,
    save_all_pars = TRUE,
    warmup = 2000, iter = 1e4,
    cores = parallel::detectCores()
    )


## ----eval = TRUE, echo = TRUE, results = "hide"-----------------------------------------------------------------------
prior6 <- c(
    prior(normal(0, 1), class = Intercept),
    prior(normal(0, 1), class = b),
    prior(cauchy(0, 1), class = sd),
    prior(cauchy(0, 10), class = sigma)
    )

mod6 <- brm(
    popular ~ 1 + texp + (1 | school),
    data = d,
    prior = prior6,
    save_all_pars = TRUE,
    warmup = 2000, iter = 1e4,
    cores = parallel::detectCores()
    )


## ----eval = TRUE, echo = TRUE, results = "hide"-----------------------------------------------------------------------
prior7 <- c(
    prior(normal(0, 1), class = Intercept),
    prior(normal(0, 1), class = b),
    prior(cauchy(0, 1), class = sd),
    prior(cauchy(0, 10), class = sigma),
    prior(lkj(2), class = cor)
    )

mod7 <- brm(
    popular ~ 1 + sex + texp + (1 + sex | school),
    data = d,
    prior = prior7,
    save_all_pars = TRUE,
    warmup = 2000, iter = 1e4,
    cores = parallel::detectCores()
    )


## ----eval = TRUE, echo = TRUE, results = "hide"-----------------------------------------------------------------------
mod8 <- brm(
    popular ~ 1 + sex + texp + sex:texp + (1 + sex | school),
    data = d,
    prior = prior7,
    save_all_pars = TRUE,
    warmup = 2000, iter = 1e4,
    cores = parallel::detectCores()
    )


## ----eval = TRUE, echo = TRUE-----------------------------------------------------------------------------------------
# calcul du WAIC et ajout du WAIC à chaque modèle
mod5 <- add_criterion(mod5, "waic")
mod6 <- add_criterion(mod6, "waic")
mod7 <- add_criterion(mod7, "waic")
mod8 <- add_criterion(mod8, "waic")


## ----eval = TRUE, echo = TRUE-----------------------------------------------------------------------------------------
# comparaison des WAIC de chaque modèle
w <- loo_compare(mod5, mod6, mod7, mod8, criterion = "waic")
print(w, simplify = FALSE)


## ----eval = TRUE, echo = TRUE, fig.width = 10, fig.height = 5---------------------------------------------------------
pp_check(mod8, nsamples = 1e2) + theme_bw(base_size = 20)


## ----eval = TRUE, echo = FALSE, fig.width = 14, fig.height = 6--------------------------------------------------------
library(patchwork)

p1 <-
  d %>% 
  ggplot(aes(x = popular, fill = ..x..) ) +
  geom_histogram(binwidth = 0.5, size = 0) +
  scale_x_continuous(breaks = 1:10, limits = c(1, 10) ) +
  labs(x = "Popularité", y = "Nombre de réponses") +
  theme_bw(base_size = 16) +
  theme(
    axis.title.y = element_text(angle = 90),
    legend.position = "none"
    )

p2 <-
  d %>%
  count(popular) %>%
  mutate(pr_k = n / nrow(d), cum_pr_k = cumsum(pr_k) ) %>% 
  ggplot(aes(x = popular, y = cum_pr_k, color = popular, fill = popular) ) +
  geom_line() +
  geom_point(shape = 21, color = "white", size = 2.5, stroke = 1) +
  labs(x = "Popularité", y = "Proportion cumulée") +
  theme_bw(base_size = 16) +
  scale_x_continuous(breaks = 1:10, limits = c(1, 10) ) +
  theme(
    axis.title.y = element_text(angle = 90),
    legend.position = "none"
    )

p3 <-
  d %>%
  count(popular) %>%
  mutate(cum_pr_k = cumsum(n / nrow(d) ) ) %>% 
  filter(popular < 9) %>% 
  ggplot(aes(
    x = popular, y = log(cum_pr_k / (1 - cum_pr_k) ),
    color = popular, fill = popular
    ) ) +
  geom_line() +
  geom_point(shape = 21, colour = "white", size = 2.5, stroke = 1) +
  labs(x = "Popularité", y = "Log cote cumulée") +
  theme_bw(base_size = 16) +
  scale_x_continuous(breaks = 1:10, limits = c(1, 10) ) +
  theme(
    axis.title.y = element_text(angle = 90),
    legend.position = "none"
    )

(p1 | p2 | p3)


## ----eval = TRUE, echo = FALSE, fig.width = 6, fig.height = 6---------------------------------------------------------
d_plot <- d %>%
  count(popular) %>%
  mutate(pr_k = n / nrow(d), cum_pr_k = cumsum(n / nrow(d) ) ) %>%
  mutate(discrete_probability = ifelse(popular == 1, cum_pr_k, cum_pr_k - pr_k) )

text <- tibble(
  text = 2:9,
  popular = seq(from = 2.25, to = 9.25, by = 1),
  cum_pr_k = d_plot$cum_pr_k - 0.065
  )

d_plot %>% 
  ggplot(aes(x = popular, y = cum_pr_k, color = cum_pr_k, fill = cum_pr_k) ) +
  geom_line() +
  geom_point(shape = 21, colour = "white", size = 2.5, stroke = 1) +
  geom_linerange(aes(ymin = 0, ymax = cum_pr_k), alpha = 0.5) +
  geom_linerange(
    aes(
      x = popular + .025,
      ymin = ifelse(popular == 1, 0, discrete_probability),
      ymax = cum_pr_k),
    color = "black"
    ) +
  geom_text(data = text,aes(label = text), size = 4) +
  scale_x_continuous(breaks = 2:9) +
  labs(x = "Popularité", y = "Proportion cumulée") +
  theme_bw(base_size = 16) +
  theme(
    axis.title.y = element_text(angle = 90),
    legend.position = "none"
    )


## ----eval = TRUE, echo = TRUE, results = "hide"-----------------------------------------------------------------------
mod9 <- brm(
    popular ~ 1 + sex + texp + sex:texp + (1 | school),
    data = d,
    prior = prior6,
    warmup = 2000, iter = 1e4,
    cores = parallel::detectCores()
    )


## ----eval = TRUE, echo = TRUE, results = "hide"-----------------------------------------------------------------------
prior10 <- c(
    brms::prior(normal(0, 10), class = Intercept),
    brms::prior(normal(0, 10), class = b),
    brms::prior(cauchy(0, 10), class = sd)
    )

mod10 <- brm(
    popular ~ 1 + sex + texp + sex:texp + (1 | school),
    data = d,
    family = cumulative(link = "logit"),
    prior = prior10,
    cores = parallel::detectCores(),
    control = list(adapt_delta = 0.99, max_treedepth = 15)
    )


## ----eval = TRUE, echo = TRUE-----------------------------------------------------------------------------------------
waic(mod9, mod10, compare = FALSE)


## ----eval = TRUE, echo = TRUE, fig.width = 12, fig.height = 6---------------------------------------------------------
pp_check(mod10, nsamples = 1e2, type = "bars", prob = 0.95, freq = FALSE) +
  theme_bw(base_size = 20) +
  scale_x_continuous(breaks = 1:9) +
  labs(x = "Popularité", y = "Proportion")

