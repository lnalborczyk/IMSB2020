## ----setup, include = FALSE-------------------------------------------------------------------------------------------
library(tidyverse)
library(knitr)
library(brms)

# setting up knitr options
opts_chunk$set(
  cache = TRUE, echo = TRUE, warning = FALSE, message = FALSE,
  fig.align = "center", dev = "svg"
  )


## ----parents, eval = TRUE, echo = TRUE--------------------------------------------------------------------------------
library(tidyverse)

d1 <- read.csv("data/parents.csv")
head(d1, 10)


## ----titanic, eval = TRUE, echo = TRUE--------------------------------------------------------------------------------
d2 <- read.csv("data/titanic.csv")
head(d2, 10)


## ----apples, eval = TRUE, echo = TRUE---------------------------------------------------------------------------------
d3 <- read.csv("data/apples.csv")
head(d3, 10)


## ----berkeley, eval = TRUE, echo = TRUE-------------------------------------------------------------------------------
library(rethinking)
d4 <- get(data(UCBadmit) )
head(d4, 10)


## ----echo = FALSE, fig.align = "center", out.width = "1000px"---------------------------------------------------------
knitr::include_graphics("figures/trolley.png")


## ----morale, eval = TRUE, echo = TRUE---------------------------------------------------------------------------------
d5 <- read.csv("data/morale.csv")
head(d5)


## ----plot-parents, eval = TRUE, echo = TRUE, fig.width = 10, fig.height = 5-------------------------------------------
d1 %>%
    gather(parent, parent.height, 3:4) %>%
    ggplot(aes(x = parent.height, y = height, colour = parent, fill = parent) ) +
    geom_point(pch = 21, size = 4, color = "white", alpha = 1) +
    stat_smooth(method = "lm", fullrange = TRUE) +
    facet_wrap(~ gender) +
    theme_bw(base_size = 20)


## ----models-parents-1, eval = TRUE, echo = TRUE, results = "hide"-----------------------------------------------------
library(brms)

d1$gender <- ifelse(d1$gender == "F", -0.5, 0.5)
d1$mother <- scale(d1$mother) %>% as.numeric
d1$father <- scale(d1$father) %>% as.numeric

p1 <- c(
    prior(normal(70, 10), class = Intercept),
    prior(cauchy(0, 10), class = sigma)
    )

m1 <- brm(
    height ~ 1 + gender,
    prior = p1,
    data = d1
    )

p2 <- c(
    prior(normal(70, 10), class = Intercept),
    prior(normal(0, 10), class = b),
    prior(cauchy(0, 10), class = sigma)
    )

m2 <- brm(
    height ~ 1 + gender + mother + father,
    prior = p2,
    data = d1
    )


## ----models-parents-2, eval = TRUE, echo = TRUE, results = "hide"-----------------------------------------------------
p3 <- c(
    prior(normal(70, 10), class = Intercept),
    prior(normal(0, 10), class = b),
    prior(cauchy(0, 10), class = sigma)
    )

m3 <- brm(
    height ~ 1 + gender + mother + father + gender:mother,
    prior = p3,
    data = d1
    )

p4 <- c(
    prior(normal(70, 10), class = Intercept),
    prior(normal(0, 10), class = b),
    prior(cauchy(0, 10), class = sigma)
    )

m4 <- brm(
    height ~ 1 + gender + mother + father + gender:father,
    prior = p4,
    data = d1
    )


## ----WAIC-parents, eval = TRUE, echo = TRUE---------------------------------------------------------------------------
m1 <- add_criterion(m1, "waic")
m2 <- add_criterion(m2, "waic")
m3 <- add_criterion(m3, "waic")
m4 <- add_criterion(m4, "waic")

model_comparison_table <- loo_compare(m1, m2, m3, m4, criterion = "waic") %>%
  data.frame %>%
  rownames_to_column(var = "model")

weights <- data.frame(weight = model_weights(m1, m2, m3, m4, weights = "waic") ) %>%
  round(digits = 3) %>%
  rownames_to_column(var = "model")

left_join(model_comparison_table, weights, by = "model")


## ----summary-parents, eval = TRUE, echo = TRUE------------------------------------------------------------------------
summary(m3)


## ----plot-m3-parents, eval = TRUE, echo = TRUE, fig.width = 12, fig.height = 6----------------------------------------
m3 %>%
    plot(
        pars = "^b_",
        combo = c("dens_overlay", "trace"), widths = c(1, 1.5),
        theme = theme_bw(base_size = 14)
        )


## ----ppc-m3-parents, eval = TRUE, echo = TRUE, fig.width = 12, fig.height = 6-----------------------------------------
pp_check(m3, nsamples = 1e2) + theme_bw(base_size = 20)


## ----reshape-titanic, eval = TRUE, echo = TRUE------------------------------------------------------------------------
# centering and standardising predictors

d2 <-
    d2 %>%
    mutate(
        pclass = ifelse(pclass == "lower", -0.5, 0.5),
        gender = ifelse(gender == "female", -0.5, 0.5),
        age = scale(age) %>% as.numeric,
        parch = scale(parch) %>% as.numeric
        )


## ----plot-titanic, eval = TRUE, echo = TRUE, fig.width = 10, fig.height = 6-------------------------------------------
d2 %>%
    group_by(pclass, gender) %>%
    summarise(p = mean(survival) ) %>%
    ggplot(aes(x = as.factor(pclass), y = p, fill = as.factor(gender) ) ) +
    geom_bar(position = position_dodge(0.5), stat = "identity", alpha = 0.8) +
    theme_bw(base_size = 20) + xlab("class") + ylab("p(survival)")


## ----models-titanic-1, eval = TRUE, echo = TRUE, results = "hide"-----------------------------------------------------
prior0 <- prior(normal(0, 10), class = Intercept)

m0 <- brm(
    survival ~ 1,
    family = binomial(link = "logit"),
    prior = prior0,
    data = d2,
    cores = parallel::detectCores()
    )

prior1 <- c(
    prior(normal(0, 10), class = Intercept),
    prior(normal(0, 10), class = b)
    )

m1 <- brm(
    # using the dot is equivalent to say "all predictors" (all columns)
    survival ~ .,
    family = binomial(link = "logit"),
    prior = prior1,
    data = d2,
    cores = parallel::detectCores()
    )


## ----models-titanic-2, eval = TRUE, echo = TRUE, results = "hide"-----------------------------------------------------
m2 <- brm(
    survival ~ 1 + pclass + gender + pclass:gender,
    family = binomial(link = "logit"),
    prior = prior1,
    data = d2,
    cores = parallel::detectCores()
    )

m3 <- brm(
    survival ~ 1 + pclass + gender + pclass:gender + age,
    family = binomial(link = "logit"),
    prior = prior1,
    data = d2,
    cores = parallel::detectCores()
    )


## ----WAIC-titanic, eval = TRUE, echo = TRUE, message = FALSE----------------------------------------------------------
m1 <- add_criterion(m1, "waic")
m2 <- add_criterion(m2, "waic")
m3 <- add_criterion(m3, "waic")

model_comparison_table <- loo_compare(m1, m2, m3, criterion = "waic") %>%
  data.frame %>%
  rownames_to_column(var = "model")

weights <- data.frame(weight = model_weights(m1, m2, m3, weights = "waic") ) %>%
  round(digits = 3) %>%
  rownames_to_column(var = "model")

left_join(model_comparison_table, weights, by = "model")


## ----ppc-titanic, eval = TRUE, echo = TRUE, message = FALSE, fig.width = 12, fig.height = 6---------------------------
pp_check(m3, nsamples = 1e2) + theme_bw(base_size = 20)


## ----summary-titanic, eval = TRUE, echo = TRUE------------------------------------------------------------------------
summary(m3)


## ----plot-titanic-m3, eval = TRUE, echo = TRUE, fig.width = 12, fig.height = 6----------------------------------------
m3 %>%
    plot(
        pars = "^b_",
        combo = c("dens_overlay", "trace"), widths = c(1, 1.5),
        theme = theme_bw(base_size = 14)
        )


## ---- eval = TRUE, echo = TRUE, fig.width = 10, fig.height = 5--------------------------------------------------------
d3 <- d3 %>% filter(diam != 0) # removing null data


## ---- eval = TRUE, echo = FALSE, fig.width = 10, fig.height = 5-------------------------------------------------------
d3 %>%
    ggplot(aes(x = time, y = diam, colour = as.factor(apple) ) ) +
    geom_point(show.legend = FALSE) +
    geom_line(show.legend = FALSE) +
    facet_wrap(~tree, ncol = 5) +
    theme_bw(base_size = 20)


## ---- eval = TRUE, echo = TRUE, results = "hide"----------------------------------------------------------------------
p1 <- c(
    prior(normal(0, 10), class = Intercept),
    prior(cauchy(0, 10), class = sigma)
    )

m1 <- brm(
    diam ~ 1,
    prior = p1,
    data = d3,
    cores = parallel::detectCores()
    )

p2 <- c(
    prior(normal(0, 10), class = Intercept),
    prior(normal(0, 10), class = b),
    prior(cauchy(0, 10), class = sigma)
    )

m2 <- brm(
    diam ~ 1 + time,
    prior = p2,
    data = d3,
    cores = parallel::detectCores()
    )


## ---- eval = TRUE, echo = TRUE, results = "hide"----------------------------------------------------------------------
p3 <- c(
    prior(normal(0, 10), class = Intercept),
    prior(normal(0, 10), class = b),
    prior(cauchy(0, 10), class = sd),
    prior(cauchy(0, 10), class = sigma)
    )

m3 <- brm(
    diam ~ 1 + time + (1 | tree),
    prior = p3,
    data = d3,
    cores = parallel::detectCores()
    )

p4 <- c(
    prior(normal(0, 10), class = Intercept),
    prior(normal(0, 10), class = b),
    prior(cauchy(0, 10), class = sd),
    prior(cauchy(0, 10), class = sigma),
    prior(lkj(2), class = cor)
    )

m4 <- brm(
    diam ~ 1 + time + (1 + time | tree),
    prior = p4,
    data = d3,
    cores = parallel::detectCores(),
    control = list(adapt_delta = 0.99)
    )


## ---- eval = TRUE, echo = TRUE, results = "hide"----------------------------------------------------------------------
p5 <- c(
    prior(normal(0, 10), class = Intercept),
    prior(normal(0, 10), class = b),
    prior(cauchy(0, 10), class = sd),
    prior(cauchy(0, 10), class = sigma),
    prior(lkj(2), class = cor)
    )

m5 <- brm(
    diam ~ 1 + time + (1 + time | tree / apple),
    prior = p5,
    data = d3,
    cores = parallel::detectCores(),
    control = list(adapt_delta = 0.99)
    )


## ---- eval = TRUE, echo = TRUE----------------------------------------------------------------------------------------
m1 <- add_criterion(m1, "waic")
m2 <- add_criterion(m2, "waic")
m3 <- add_criterion(m3, "waic")
m4 <- add_criterion(m4, "waic")
m5 <- add_criterion(m5, "waic")

model_comparison_table <- loo_compare(m1, m2, m3, m4, m5, criterion = "waic") %>%
  data.frame %>%
  rownames_to_column(var = "model")

weights <- data.frame(weight = model_weights(m1, m2, m3, m4, m5, weights = "waic") ) %>%
  round(digits = 3) %>%
  rownames_to_column(var = "model")

left_join(model_comparison_table, weights, by = "model")


## ---- eval = TRUE, echo = TRUE----------------------------------------------------------------------------------------
posterior_summary(m5, pars = c("^b", "sigma") )


## ---- eval = TRUE, echo = TRUE, fig.width = 12, fig.height = 6--------------------------------------------------------
post <- posterior_samples(m5, "b") # extracts posterior samples

ggplot(data = d3, aes(x = time, y = diam) ) +
    geom_point(alpha = 0.5, shape = 1) +
    geom_abline(
        data = post, aes(intercept = b_Intercept, slope = b_time),
        alpha = 0.01, size = 0.5) +
    theme_bw(base_size = 20) + labs(x = "Temps", y = "Diamètre")


## ---- eval = TRUE, echo = TRUE, fig.width = 10, fig.height = 5--------------------------------------------------------
library(tidybayes)
library(modelr)

d3 %>%
    group_by(tree, apple) %>%
    data_grid(time = seq_range(time, n = 1e2) ) %>%
    add_fitted_samples(m5, n = 1e2) %>%
    ggplot(aes(x = time, y = diam, colour = factor(apple) ) ) +
    geom_line(
        aes(y = estimate, group = paste(apple, .iteration) ),
        alpha = 0.2, show.legend = FALSE) +
    facet_wrap(~tree, ncol = 5) +
    theme_bw(base_size = 20) + labs(x = "Temps", y = "Diamètre")


## ----plot-berkeley, eval = TRUE, echo = TRUE, fig.width = 10, fig.height = 5------------------------------------------
d4 %>%
    ggplot(aes(x = dept, y = admit / applications) ) +
    geom_bar(stat = "identity") +
    facet_wrap(~ applicant.gender) +
    theme_bw(base_size = 18) + labs(x = "Département", y = "Probabilité d'admission")


## ----models-berkeley-1, eval = TRUE, echo = TRUE, results = "hide"----------------------------------------------------
# centering gender predictor
d4$gender <- ifelse(d4$applicant.gender == "female", -0.5, 0.5)

# creating an index for department
d4$dept_id <- coerce_index(d4$dept)

p1 <- c(
    prior(normal(0, 10), class = "Intercept"),
    prior(cauchy(0, 2), class = "sd")
    )

m1 <- brm(
    admit | trials(applications) ~ 1 + (1 | dept_id),
    data = d4, family = binomial,
    prior = p1,
    warmup = 1000, iter = 5000,
    control = list(adapt_delta = 0.99, max_treedepth = 12)
    )


## ----models-berkeley-2, eval = TRUE, echo = TRUE, results = "hide"----------------------------------------------------
p2 <- c(
    prior(normal(0, 10), class = "Intercept"),
    prior(normal(0, 1), class = "b"),
    prior(cauchy(0, 2), class = "sd")
    )

m2 <- brm(
    admit | trials(applications) ~ 1 + gender + (1 | dept_id),
    data = d4, family = binomial,
    prior = p2,
    warmup = 1000, iter = 5000,
    control = list(adapt_delta = 0.99, max_treedepth = 12)
    )

p3 <- c(
    prior(normal(0, 10), class = "Intercept"),
    prior(normal(0, 1), class = "b"),
    prior(cauchy(0, 2), class = "sd"),
    prior(lkj(2), class = "cor")
    )

m3 <- brm(
    admit | trials(applications) ~ 1 + gender + (1 + gender | dept_id),
    data = d4, family = binomial,
    prior = p3,
    warmup = 1000, iter = 5000,
    control = list(adapt_delta = 0.99, max_treedepth = 12)
    )


## ----WAIC-berkeley, eval = TRUE, echo = TRUE--------------------------------------------------------------------------
m1 <- add_criterion(m1, "waic")
m2 <- add_criterion(m2, "waic")
m3 <- add_criterion(m3, "waic")

model_comparison_table <- loo_compare(m1, m2, m3, criterion = "waic") %>%
  data.frame %>%
  rownames_to_column(var = "model")

weights <- data.frame(weight = model_weights(m1, m2, m3, weights = "waic") ) %>%
  round(digits = 3) %>%
  rownames_to_column(var = "model")

left_join(model_comparison_table, weights, by = "model")


## ----summary-berkeley, eval = TRUE, echo = TRUE-----------------------------------------------------------------------
summary(m3)


## ----predict-berkeley, eval = TRUE, echo = TRUE, fig.width = 10, fig.height = 5---------------------------------------
library(tidybayes)
library(modelr)

d4 %>%
    group_by(dept_id, applications) %>%
    data_grid(gender = seq_range(gender, n = 1e2) ) %>%
    add_fitted_samples(m3, newdata = ., n = 100, scale = "linear") %>%
    mutate(estimate = plogis(estimate) ) %>%
    ggplot(aes(x = gender, y = estimate, group = .iteration) ) +
    geom_hline(yintercept = 0.5, lty = 2) +
    geom_line(aes(y = estimate, group = .iteration), size = 0.5, alpha = 0.2) +
    facet_wrap(~dept_id, nrow = 2) +
    theme_bw(base_size = 20)


## ----plot-moral, eval = TRUE, echo = TRUE, fig.width = 10, fig.height = 6---------------------------------------------
d5$response %>% table %>% plot(xlab = "response", ylab = "", cex.axis = 2, cex.lab = 2)


## ----models-moral, eval = TRUE, echo = TRUE, results = "hide"---------------------------------------------------------
moral1 <- brm(
    response ~ 1,
    data = d5,
    family = cumulative("logit"),
    cores = parallel::detectCores(),
    control = list(adapt_delta = 0.99)
    )

moral2 <- brm(
    response ~ 1 + action + intention + contact,
    data = d5,
    family = cumulative("logit"),
    cores = parallel::detectCores(),
    control = list(adapt_delta = 0.99)
    )


## ----WAIC-moral, eval = TRUE, echo = TRUE-----------------------------------------------------------------------------
brms::waic(moral1, moral2)


## ----summary-moral, eval = TRUE, echo = TRUE--------------------------------------------------------------------------
summary(moral2, prob = 0.95)


## ---- eval = TRUE, echo = TRUE, message = FALSE, warning = FALSE------------------------------------------------------
marg1 <- marginal_effects(moral2, "action", ordinal = TRUE)
p1 <- plot(marg1, theme = theme_bw(base_size = 20), plot = FALSE)[[1]]

marg2 <- marginal_effects(moral2, "intention", ordinal = TRUE)
p2 <- plot(marg2, theme = theme_bw(base_size = 20), plot = FALSE)[[1]]

marg3 <- marginal_effects(moral2, "contact", ordinal = TRUE)
p3 <- plot(marg3, theme = theme_bw(base_size = 20), plot = FALSE)[[1]]


## ----patchwork-moral, eval = TRUE, echo = TRUE, message = FALSE, warning = FALSE, fig.width = 14, fig.height = 7------
library(patchwork)
p1 + p2 + p3 + plot_layout(guides = "collect") & theme(legend.position = "right")


## ----ppc-moral-1, eval = TRUE, echo = TRUE, fig.width = 12, fig.height = 6--------------------------------------------
pp_check(moral2, nsamples = 1e2) +
  theme_bw(base_size = 20) +
  labs(x = "Moralité", y = "Proportion")


## ----ppc-moral-2, eval = TRUE, echo = TRUE, fig.width = 12, fig.height = 6--------------------------------------------
pp_check(moral2, nsamples = 1e2, type = "bars", prob = 0.95, freq = FALSE) +
  theme_bw(base_size = 20) +
  scale_x_continuous(breaks = 1:7) +
  labs(x = "Moralité", y = "Proportion")

