## ----setup, include = FALSE-----------------------------------------------------------------------------------------
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


## ---- echo = FALSE, fig.width = 12, fig.height = 6, out.width = "75%"-----------------------------------------------
# plot from https://bookdown.org/content/4857/big-entropy-and-the-generalized-linear-model.html#generalized-linear-models

tibble(x = seq(from = -1, to = 3, by = 0.01) ) %>%
  mutate(probability = 0.35 + x * 0.5) %>%
  ggplot(aes(x = x, y = probability) ) +
  geom_rect(xmin = -1, xmax = 3, ymin = 0,  ymax = 1, fill = "gray90") +
  geom_hline(yintercept = 0:1, linetype = 2) +
  geom_line(aes(linetype = probability > 1), size = 1) +
  geom_segment(x = 1.3, xend = 3, y = 1, yend = 1, size = 2 / 3) +
  scale_y_continuous(breaks = c(0, 0.5, 1) ) +
  coord_cartesian(xlim = c(0, 2), ylim = c(0, 1.2) ) +
  theme_bw(base_size = 20) +
  theme(legend.position = "none", panel.grid = element_blank() ) +
  labs(x = "Prédicteur", y = "Probabilité")


## ---- echo = FALSE, fig.width = 12, fig.height = 6, out.width = "75%"-----------------------------------------------
# plot from https://bookdown.org/content/4857/big-entropy-and-the-generalized-linear-model.html#generalized-linear-models
# fonction logit utilisée dans le GLM binomial (ou régression logistique)

# make data for the horizontal lines
alpha <- 0
beta  <- 4

lines <-
  tibble(x = seq(from = -1, to = 1, by = 0.25) ) %>% 
  mutate(
    `log-odds` = alpha + x * beta,
    probability = exp(alpha + x * beta) / (1 + exp(alpha + x * beta) )
    )

# make the primary data 
beta <- 2

d <-
  tibble(x = seq(from = -1, to = 1, length.out = 50) ) %>% 
  mutate(
    `log-odds`  = alpha + x * beta,
    probability = exp(alpha + x * beta) / (1 + exp(alpha + x * beta) )
    ) 

# make the individual plots
p1 <-
  d %>% 
  ggplot(aes(x = x, y = `log-odds`) ) +
  geom_hline(
    data = lines,
    aes(yintercept = `log-odds`),
    color = "gray"
  ) +
  geom_line(size = 1) +
  coord_cartesian(xlim = c(-1, 1) ) +
  theme_bw(base_size = 20) +
  theme(panel.grid = element_blank() ) +
  labs(x = "Prédicteur", y = "Log-odds")

p2 <-
  d %>% 
  ggplot(aes(x = x, y = probability) ) +
  geom_hline(
    data = lines,
    aes(yintercept = probability),
    color = "gray"
    ) +
  geom_line(size = 1) +
  coord_cartesian(xlim = c(-1, 1) ) +
  theme_bw(base_size = 20) +
  theme(panel.grid = element_blank() ) +
  labs(x = "Prédicteur", y = "Probabilité")

p1 + p2

# finally, we're ready to mash the plots together and behold their nerdy glory
# (p1 | p2) +
#   plot_annotation(subtitle = "The logit link transforms a linear model (left) into a probability (right).")
# 
# 
# x <- seq.int(from = -1, to = 1, length.out = 1e2)
# y <- 0 + 2 * x
# 
# library(scales)
# 
# tn <- trans_new(
#   name = "logpeps",
#   transform = function(x) plogis(x),
#   inverse = function(y) log(y / (1 - y) ),
#   # inverse = function(y) y,
#   domain = c(0, Inf),
#   breaks = c(0, 0.1, 1)
#   )
# 
# data.frame(x = x, y = y) %>%
#   ggplot(aes(x = x, y = y) ) +
#   geom_line() +
#   theme_bw(base_size = 12) +
#   ylim(-4, 4) +
#   labs(y = "Log-odds")
# 
# data.frame(x = x, y = y) %>%
#   ggplot(aes(x = x, y = y) ) +
#   geom_line() +
#   coord_trans(y = tn) +
#   theme_bw(base_size = 12) +
#   ylim(0, 1) +
#   labs(y = "Probability")


## ---- echo = FALSE, fig.width = 12, fig.height = 6, out.width = "75%"-----------------------------------------------
# plot from https://bookdown.org/content/4857/big-entropy-and-the-generalized-linear-model.html#generalized-linear-models
# fonction logit utilisée dans le GLM binomial (ou régression logistique)

# make data for the horizontal lines
alpha <- 0
beta  <- 4

lines <-
  tibble(x = seq(from = -1, to = 1, by = 0.25) ) %>% 
  mutate(
    `log-odds` = alpha + x * beta,
    probability = exp(alpha + x * beta) / (1 + exp(alpha + x * beta) )
    )

# make the primary data 
beta <- 2

d <-
  tibble(x = seq(from = -1, to = 1, length.out = 50) ) %>% 
  mutate(
    `log-odds`  = alpha + x * beta,
    probability = exp(alpha + x * beta) / (1 + exp(alpha + x * beta) )
    ) 

# make the individual plots
p1 <-
  d %>% 
  ggplot(aes(x = x, y = `log-odds`) ) +
  geom_hline(
    data = lines,
    aes(yintercept = `log-odds`),
    color = "gray"
  ) +
  geom_line(size = 1) +
  coord_cartesian(xlim = c(-1, 1) ) +
  theme_bw(base_size = 20) +
  theme(panel.grid = element_blank() ) +
  labs(x = "Prédicteur", y = "Log-odds")

p2 <-
  d %>% 
  ggplot(aes(x = x, y = probability) ) +
  geom_hline(
    data = lines,
    aes(yintercept = probability),
    color = "gray"
    ) +
  geom_line(size = 1) +
  coord_cartesian(xlim = c(-1, 1) ) +
  theme_bw(base_size = 20) +
  theme(panel.grid = element_blank() ) +
  labs(x = "Prédicteur", y = "Probabilité")

p1 + p2

# finally, we're ready to mash the plots together and behold their nerdy glory
# (p1 | p2) +
#   plot_annotation(subtitle = "The logit link transforms a linear model (left) into a probability (right).")
# 
# 
# x <- seq.int(from = -1, to = 1, length.out = 1e2)
# y <- 0 + 2 * x
# 
# library(scales)
# 
# tn <- trans_new(
#   name = "logpeps",
#   transform = function(x) plogis(x),
#   inverse = function(y) log(y / (1 - y) ),
#   # inverse = function(y) y,
#   domain = c(0, Inf),
#   breaks = c(0, 0.1, 1)
#   )
# 
# data.frame(x = x, y = y) %>%
#   ggplot(aes(x = x, y = y) ) +
#   geom_line() +
#   theme_bw(base_size = 12) +
#   ylim(-4, 4) +
#   labs(y = "Log-odds")
# 
# data.frame(x = x, y = y) %>%
#   ggplot(aes(x = x, y = y) ) +
#   geom_line() +
#   coord_trans(y = tn) +
#   theme_bw(base_size = 12) +
#   ylim(0, 1) +
#   labs(y = "Probability")


## ----chimp, echo = FALSE, out.width = "50%"-------------------------------------------------------------------------
knitr::include_graphics("figures/chimp_exp.jpg")


## ---- echo = TRUE---------------------------------------------------------------------------------------------------
library(tidyverse)
library(rethinking)

data(chimpanzees) # see ?chimpanzees for information on the variables
df1 <- chimpanzees
str(df1)


## ----mod1, eval = TRUE, echo = TRUE, results = "hide"---------------------------------------------------------------
library(brms)

mod1.1 <- brm(
  formula = pulled_left | trials(1) ~ 1,
  family = binomial,
  prior = set_prior("normal(0, 10)", class = "Intercept"),
  data = df1,
  # stores prior samples
  sample_prior = "yes"
  )


## ----ppc-mod1.1, eval = TRUE, echo = TRUE, fig.width = 12, fig.height = 6, out.width = "60%"------------------------
# extracts prior samples
prior_samples(mod1.1) %>%
  # applies the inverse link function
  mutate(p = brms::inv_logit_scaled(Intercept) ) %>%
  ggplot(aes(x = p) ) +
  geom_density(fill = "steelblue", adjust = 0.1) +
  theme_bw(base_size = 20) +
  labs(x = "Prior probability of pulling left", y = "Density")


## ----ppc-mod1.2, eval = TRUE, echo = FALSE, results = "hide", fig.width = 12, fig.height = 6, out.width = "80%"-----
mod1.2 <- brm(
  formula = pulled_left | trials(1) ~ 1,
  family = binomial,
  prior = set_prior("normal(0, 1)", class = "Intercept"),
  data = df1,
  sample_prior = "yes"
  )

bind_rows(prior_samples(mod1.1), prior_samples(mod1.2) ) %>% 
  mutate(
    p = inv_logit_scaled(Intercept),
    w = factor(rep(c(10, 1), each = n() / 2), levels = c(10, 1) )
    ) %>%
  ggplot(aes(x = p, fill = w) ) +
  geom_density(alpha = 0.8, adjust = 0.1) +
  scale_fill_manual(expression(italic(omega) ), values = c("steelblue", "blue") ) +
  theme_bw(base_size = 20) +
  labs(x = "Prior probability of pulling left", y = "Density")


## ---- eval = TRUE, echo = TRUE--------------------------------------------------------------------------------------
fixed_effects <- fixef(mod1.2)

rethinking::logistic(fixed_effects)
plogis(fixed_effects)


## ---- eval = TRUE, echo = TRUE, results = "hide", fig.width = 12, fig.height = 8, out.width = "50%"-----------------
post <- posterior_samples(mod1.2)
intercept_samples <- plogis(post$b_Intercept)

library(BEST)
library(coda)

plotPost(intercept_samples, compVal = 0.5, xlab = "Probability of pulling left")


## ----mod2, eval = TRUE, echo = TRUE, results = "hide"---------------------------------------------------------------
# recoding predictors
df1 <- df1 %>%
  mutate(
    prosoc_left = ifelse(prosoc_left == 1, 0.5, -0.5),
    condition = ifelse(condition == 1, 0.5, -0.5)
    )

priors <- c(
  set_prior("normal(0, 1)", class = "Intercept"),
  set_prior("normal(0, 10)", class = "b")
  )

mod2.1 <- brm(
  formula = pulled_left | trials(1) ~ 1 + prosoc_left * condition,
  family = binomial,
  prior = priors,
  data = df1,
  sample_prior = "yes"
  )


## ----ppc-mod2.1, eval = TRUE, echo = TRUE, fig.width = 12, fig.height = 6, out.width = "50%"------------------------
prior_samples(mod2.1) %>%
  mutate(
    condition1 = plogis(Intercept - 0.5 * b),
    condition2 = plogis(Intercept + 0.5 * b)
    ) %>%
  ggplot(aes(x = condition2 - condition1) ) +
  geom_density(fill = "steelblue", adjust = 0.1) +
  theme_bw(base_size = 20) +
  labs(x = "Prior difference in probability between conditions", y = "Probability density")


## ----mod2.2, eval = TRUE, echo = TRUE, results = "hide"-------------------------------------------------------------
priors <- c(
  set_prior("normal(0, 1)", class = "Intercept"),
  set_prior("normal(0, 1)", class = "b")
  )

mod2.2 <- brm(
  formula = pulled_left | trials(1) ~ 1 + prosoc_left * condition,
  family = binomial,
  prior = priors,
  data = df1,
  sample_prior = "yes"
  )


## ----ppc-mod2.2, eval = TRUE, echo = TRUE, fig.width = 12, fig.height = 6, out.width = "50%"------------------------
prior_samples(mod2.2) %>%
  mutate(
    condition1 = plogis(Intercept - 0.5 * b),
    condition2 = plogis(Intercept + 0.5 * b)
    ) %>%
  ggplot(aes(x = condition2 - condition1) ) +
  geom_density(fill = "steelblue", adjust = 0.1) +
  theme_bw(base_size = 20) +
  labs(x = "Prior difference in probability between conditions", y = "Probability density")


## ---- eval = TRUE, echo = TRUE--------------------------------------------------------------------------------------
summary(mod2.2)


## ---- eval = TRUE, echo = TRUE--------------------------------------------------------------------------------------
fixef(mod2.2)


## ---- eval = TRUE, echo = TRUE, fig.width = 12, fig.height = 6, out.width = "50%"-----------------------------------
post <- posterior_samples(mod2.2)
plotPost(exp(post$b_prosoc_left), compVal = 1, xlab = "Proportional odds of pulling left")


## ---- eval = TRUE, echo = TRUE--------------------------------------------------------------------------------------
model_predictions <- fitted(mod2.2) %>%
  data.frame() %>% 
  bind_cols(df1) %>%
  mutate(
    condition = factor(condition),
    prosoc_left = factor(prosoc_left)
    )


## ---- eval = TRUE, echo = FALSE, fig.width = 12, fig.height = 6, out.width = "50%"----------------------------------
model_predictions %>%
  ggplot(aes(
    x = prosoc_left, y = Estimate,
    ymin = Q2.5, ymax = Q97.5, colour = condition
    ) ) +
  geom_hline(yintercept = 0.5, lty = 2) +
  geom_line(
    aes(group = condition),
    size = 1,
    position = position_dodge(0.2)
    ) +
  geom_pointrange(
    aes(color = condition),
    size = 1, fatten = 2, show.legend = TRUE,
    position = position_dodge(0.2)
    ) +
  ylim(0, 1) +
  theme_bw(base_size = 20) +
  scale_x_discrete(labels = c("no", "yes") ) +
  scale_colour_discrete(labels = c("alone", "social") ) +
  labs(
  x = "Was the prosocial option on the left?",
  y = "Probability of pulling the left lever"
  )


## ----eval = TRUE, echo = TRUE---------------------------------------------------------------------------------------
library(rethinking)
data(UCBadmit)
(df2 <- UCBadmit)


## ----mod3, eval = TRUE, echo = TRUE, results = "hide"---------------------------------------------------------------
priors <- c(set_prior("normal(0, 1)", class = "Intercept") )

mod3 <- brm(
  formula = admit | trials(applications) ~ 1,
  family = binomial(link = "logit"),
  prior = priors,
  data = df2,
  sample_prior = "yes"
  )


## ----mod4, eval = TRUE, echo = TRUE, results = "hide"---------------------------------------------------------------
priors <- c(
  set_prior("normal(0, 1)", class = "Intercept"),
  set_prior("normal(0, 1)", class = "b")
  )

# dummy-coding
df2$male <- ifelse(df2$applicant.gender == "male", 1, 0)

mod4 <- brm(
  formula = admit | trials(applications) ~ 1 + male,
  family = binomial(link = "logit"),
  prior = priors,
  data = df2,
  sample_prior = "yes"
  )


## ----eval = TRUE, echo = TRUE---------------------------------------------------------------------------------------
summary(mod4)


## ----eval = TRUE, echo = TRUE, fig.width = 6, fig.height = 4, out.width = "50%"-------------------------------------
post <- posterior_samples(mod4)
p.admit.male <- plogis(post$b_Intercept + post$b_male)
p.admit.female <- plogis(post$b_Intercept)
diff.admit <- p.admit.male - p.admit.female
plotPost(diff.admit, compVal = 0)


## ----eval = TRUE, echo = FALSE, fig.width = 12, fig.height = 6, out.width = "75%"-----------------------------------
# pp_check(mod4, type = "intervals", nsamples = 1e2, prob = 0.5, prob_outer = 0.95) +
#   scale_x_continuous(breaks = 1:12, limits = c(1, 12) ) +
#   theme_bw(base_size = 20) +
#   labs(x = "Sexe / Département", y = "Nombre d'admissions")

df2 <- df2 %>% mutate(case = factor(1:12) )

p <- 
  predict(mod4) %>% 
  as_tibble() %>% 
  bind_cols(df2)

d_text <-
  df2 %>%
  group_by(dept) %>%
  summarise(
    case  = mean(as.numeric(case) ),
    admit = mean(admit / applications) + 0.05
    )

ggplot(data = df2, aes(x = case, y = admit / applications) ) +
  geom_pointrange(
    data = p, 
    aes(
      y = Estimate / applications,
      ymin = Q2.5 / applications ,
      ymax = Q97.5 / applications
      ),
    shape = 1, alpha = 0.5
    ) +
  geom_point(color = "steelblue") +
  geom_line(
    aes(group = dept),
    color = "steelblue"
    ) +
  geom_text(
    data = d_text,
    aes(y = admit, label = dept),
    color = "steelblue"
    ) +
  coord_cartesian(ylim = 0:1) +
  theme_bw(base_size = 20) +
  scale_x_discrete(
    breaks = 1:12,
    labels = rep(c("male", "female"), 6)
    ) +
  labs(
    x = "",
    y = "Probabilité d'admission",
    title = "Posterior predictive check"
    )


## ----eval = TRUE, echo = TRUE, results = "hide"---------------------------------------------------------------------
# modèle sans prédicteur
mod5 <- brm(
  admit | trials(applications) ~ 0 + dept,
  family = binomial(link = "logit"),
  prior = set_prior("normal(0, 1)", class = "b"),
  data = df2
  )

# modèle avec prédicteur
mod6 <- brm(
  admit | trials(applications) ~ 0 + dept + male,
  family = binomial(link = "logit"),
  prior = set_prior("normal(0, 1)", class = "b"),
  data = df2
  )


## ----eval = TRUE, echo = TRUE---------------------------------------------------------------------------------------
summary(mod6)


## ----eval = TRUE, echo = TRUE---------------------------------------------------------------------------------------
fixef(mod6)


## ----eval = TRUE, echo = FALSE, fig.width = 12, fig.height = 6, out.width = "75%"-----------------------------------
predict(mod6) %>%
  as_tibble() %>% 
  bind_cols(df2) %>%
  ggplot(aes(x = case, y = admit / applications) ) +
  geom_pointrange(
    aes(
      y = Estimate / applications,
      ymin = Q2.5 / applications,
      ymax = Q97.5 / applications
      ),
    color = "steelblue",
    shape = 1, alpha = 0.5
    ) +
  geom_point(color = "steelblue") +
  geom_line(
    aes(group = dept),
    color = "steelblue"
    ) +
  geom_text(
    data = d_text,
    aes(y = admit, label = dept),
    color = "steelblue"
    ) +
  coord_cartesian(ylim = 0:1) +
  theme_bw(base_size = 20) +
  scale_x_discrete(
    breaks = 1:12,
    labels = rep(c("male", "female"), 6)
    ) +
  labs(
    x = "",
    y = "Probabilité d'admission",
    title = "Posterior predictive check"
    )


## ----eval = TRUE, echo = TRUE---------------------------------------------------------------------------------------
df3 <- read.csv("data/absence.csv")
df3 %>% sample_frac %>% head(10)


## ----mod7, eval = TRUE, echo = TRUE, results = "hide"---------------------------------------------------------------
mod7 <- brm(
    presence | trials(total) ~ 1,
    family = binomial(link = "logit"),
    prior = set_prior("normal(0, 1)", class = "Intercept"),
    data = df3,
    cores = parallel::detectCores()
    )


## ---- eval = TRUE, echo = TRUE--------------------------------------------------------------------------------------
fixef(mod7) # effet relatif (cote)
fixef(mod7) %>% plogis # effet absolu (probabilité de présence)


## ----eval = TRUE, echo = TRUE---------------------------------------------------------------------------------------
df3 <-
  df3 %>%
  mutate(
    reminder = ifelse(reminder == "no", 0, 1),
    inscription = ifelse(inscription == "panel", 0, 1)
    )

head(df3, n = 10)


## ----mod8, eval = TRUE, echo = TRUE, results = "hide"---------------------------------------------------------------
priors <- c(
  set_prior("normal(0, 1)", class = "Intercept"),
  set_prior("normal(0, 1)", class = "b")
  )

mod8 <- brm(
    presence | trials(total) ~ 1 + reminder,
    family = binomial(link = "logit"),
    prior = priors,
    data = df3,
    cores = parallel::detectCores()
    )


## ----eval = TRUE, echo = TRUE---------------------------------------------------------------------------------------
exp(fixef(mod8)[2]) # odds ratio between no-reminder and reminder


## ----eval = TRUE, echo = TRUE, fig.width = 10, fig.height = 5-------------------------------------------------------
post <- posterior_samples(mod8) # extracting posterior samples
p.no <- plogis(post$b_Intercept) # mean probability of presence when no reminder
p.yes <- plogis(post$b_Intercept + post$b_reminder) # mean probability of presence when reminder
plotPost(p.yes - p.no, compVal = 0, showMode = TRUE) # plotting it


## ----eval = TRUE, echo = TRUE, fig.width = 8, fig.height = 4--------------------------------------------------------
library(tidybayes)
library(modelr)

df3 %>%
  group_by(total) %>%
  data_grid(reminder = seq_range(reminder, n = 1e2) ) %>%
  add_fitted_draws(mod8, newdata = ., n = 100, scale = "linear") %>%
  mutate(estimate = plogis(.value) ) %>%
  group_by(reminder, .draw) %>%
  summarise(estimate = mean(estimate) ) %>%
  ggplot(aes(x = reminder, y = estimate, group = .draw) ) +
  geom_hline(yintercept = 0.5, lty = 2) +
  geom_line(aes(y = estimate, group = .draw), size = 0.5, alpha = 0.1) +
  ylim(0, 1) + theme_bw(base_size = 20) +
  labs(x = "Mail de rappel", y = "Probabilité de présence")


## ----mod9, eval = TRUE, echo = TRUE, results = "hide"---------------------------------------------------------------
priors <- c(
  set_prior("normal(0, 1)", class = "Intercept"),
  set_prior("normal(0, 1)", class = "b")
  )

mod9 <- brm(
    presence | trials(total) ~ 1 + inscription,
    family = binomial(link = "logit"),
    prior = priors,
    data = df3,
    cores = parallel::detectCores()
    )


## ----eval = TRUE, echo = TRUE, fig.width = 10, fig.height = 5-------------------------------------------------------
post <- posterior_samples(mod9)
p.panel <- plogis(post$b_Intercept) # mean probability of presence for panel
p.doodle <- plogis(post$b_Intercept + post$b_inscription) # mean probability of presence for doodle
plotPost(p.panel - p.doodle, compVal = 0, showMode = TRUE) # plotting it


## ----mod10, eval = TRUE, echo = TRUE, results = "hide"--------------------------------------------------------------
priors <- c(
  set_prior("normal(0, 1)", class = "Intercept"),
  set_prior("normal(0, 1)", class = "b")
  )

mod10 <- brm(
    presence | trials(total) ~ 1 + reminder + inscription,
    family = binomial(link = "logit"),
    prior = priors,
    data = df3,
    cores = parallel::detectCores()
    )


## ----eval = TRUE, echo = TRUE---------------------------------------------------------------------------------------
summary(mod10)


## ----eval = TRUE, echo = TRUE---------------------------------------------------------------------------------------
fixef(mod8) %>% exp
fixef(mod9) %>% exp
fixef(mod10) %>% exp


## ----eval = TRUE, echo = TRUE, fig.width = 12, fig.height = 6-------------------------------------------------------
posterior_samples(mod10) %>%
    ggplot(aes(b_reminder, b_inscription) ) +
    geom_point(size = 3, pch = 21, alpha = 0.8, color = "white", fill = "black") +
    theme_bw(base_size = 20)


## ----eval = TRUE, echo = TRUE---------------------------------------------------------------------------------------
read.csv("data/absence.csv") %>%
  sample_frac %>%
  group_by(inscription, reminder) %>%
  summarise(n = sum(total) ) %>%
  spread(key = reminder, value = n) %>%
  data.frame

