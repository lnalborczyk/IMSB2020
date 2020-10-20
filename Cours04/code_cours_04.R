## ----setup, include = FALSE------------------------------------------------------------------------------------
library(tidyverse)
library(knitr)
library(brms)

# setting up knitr options
opts_chunk$set(
  cache = TRUE, echo = TRUE, warning = FALSE, message = FALSE,
  fig.align = "center", dev = "svg"
  )


## ----rappel-brms, eval = FALSE, echo = TRUE--------------------------------------------------------------------
## library(brms)
## 
## priors <- c(
##   prior(normal(100, 10), class = Intercept),
##   prior(normal(0, 10), class = b),
##   prior(exponential(0.01), class = sigma)
##   )
## 
## model <- brm(
##   y ~ 1 + x,
##   family = gaussian(),
##   prior = priors,
##   data = df
##   )
## 
## summary(model)


## ----import-waffle, eval = TRUE, echo = TRUE-------------------------------------------------------------------
library(rethinking)
library(tidyverse)

data(WaffleDivorce) # import des données
df1 <- WaffleDivorce # import dans une dataframe nommée df1

str(df1) # affiche la structure des données


## ----waffle-divorce, eval = TRUE, echo = TRUE, fig.width = 10, fig.height = 5----------------------------------
df1 %>%
  ggplot(aes(x = WaffleHouses, y = Divorce) ) +
  geom_text(aes(label = Loc) ) +
  geom_smooth(method = "lm", color = "black", se = TRUE) +
  theme_bw(base_size = 20) +
  labs(x = "Number of Waffle Houses per million people", y = "Divorce rate")


## ----waffle-divorce-mariage, eval = TRUE, echo = TRUE, fig.width = 10, fig.height = 5--------------------------
df1$Divorce.s <- (df1$Divorce - mean(df1$Divorce) ) / sd(df1$Divorce)
df1$Marriage.s <- (df1$Marriage - mean(df1$Marriage) ) / sd(df1$Marriage)

df1 %>%
  ggplot(aes(x = Marriage, y = Divorce) ) +
  geom_point(pch = 21, color = "white", fill = "black", size = 5, alpha = 0.8) +
  geom_smooth(method = "lm", color = "black", se = TRUE) +
  theme_bw(base_size = 20) + labs(x = "Taux de mariage", y = "Taux de divorce")


## ----waffle-divorce-median, eval = TRUE, echo = TRUE, fig.width = 10, fig.height = 5---------------------------
df1$MedianAgeMarriage.s <- (df1$MedianAgeMarriage - mean(df1$MedianAgeMarriage) ) /
  sd(df1$MedianAgeMarriage)

df1 %>%
  ggplot(aes(x = MedianAgeMarriage, y = Divorce) ) +
  geom_point(pch = 21, color = "white", fill = "black", size = 5, alpha = 0.8) +
  geom_smooth(method = "lm", color = "black", se = TRUE) +
  theme_bw(base_size = 20) + labs(x = "Age médian de mariage", y = "Taux de divorce")


## ----waffle-divorce-map, eval = FALSE, echo = FALSE, fig.width = 15, fig.height = 5----------------------------
## On peut représenter nos trois variables principales sur une carte des 50 états...
## 
## # plot from
## # https://bookdown.org/ajkurz/Statistical_Rethinking_recoded/multivariate-linear-models.html
## # devtools::install_github("wmurphyrd/fiftystater")
## 
## library(fiftystater)
## 
## df1 %>%
##   # first we'll standardize the three variables to put them all on the same scale
##   mutate(
##     Divorce_z = (Divorce - mean(Divorce) )/ sd(Divorce),
##     MedianAgeMarriage_z = (MedianAgeMarriage - mean(MedianAgeMarriage) ) / sd(MedianAgeMarriage),
##     Marriage_z = (Marriage - mean(Marriage) ) / sd(Marriage),
##     # need to make the state names lowercase to match with the map data
##     Location = str_to_lower(Location)
##     ) %>%
##   # here we select the relevant variables and put them in the long format to
##   # facet with `facet_wrap()`
##   select(Divorce_z:Marriage_z, Location) %>%
##   gather(key, value, -Location) %>%
##   # plotting it
##   ggplot(aes(map_id = Location)) +
##   geom_map(
##     aes(fill = value), map = fifty_states,
##     # color = "firebrick",
##     size = 1 / 15, show.legend = FALSE
##     ) +
##   expand_limits(x = fifty_states$long, y = fifty_states$lat) +
##   scale_x_continuous(NULL, breaks = NULL) +
##   scale_y_continuous(NULL, breaks = NULL) +
##   # scale_fill_gradient(low = "#f8eaea", high = "firebrick4") +
##   coord_map() +
##   facet_wrap(~key) +
##   theme_bw(base_size = 20)


## ----mod1, eval = TRUE, echo = TRUE, results = "hide"----------------------------------------------------------
priors <- c(
  prior(normal(0, 10), class = Intercept),
  prior(normal(0, 1), class = b),
  prior(exponential(1), class = sigma)
  )

mod1 <- brm(
  Divorce.s ~ 1 + Marriage.s,
  family = gaussian(),
  prior = priors,
  # for prior predictive checking
  sample_prior = TRUE,
  data = df1
  )


## ----ppc1-mod1, eval = TRUE, echo = TRUE-----------------------------------------------------------------------
# getting the samples from the prior distribution
prior <- prior_samples(mod1)

# displaying the first six samples
head(prior)


## ----ppc2-mod1, eval = TRUE, echo = TRUE, fig.width = 9, fig.height = 6----------------------------------------
prior %>% 
  sample_n(size = 1e2) %>% 
  rownames_to_column("draw") %>% 
  expand(nesting(draw, Intercept, b), a = c(-2, 2) ) %>%
  mutate(d = Intercept + b * a) %>% 
  ggplot(aes(x = a, y = d)) +
  geom_line(aes(group = draw), color = "steelblue", size = 0.5, alpha = 0.5) +
  labs(x = "Taux de mariage (standardisé)", y = "Taux de divorce (standardisé)") +
  theme_bw(base_size = 20)


## ----summary-mod1, eval = TRUE, echo = TRUE--------------------------------------------------------------------
summary(mod1)


## ----posterior-mod1, eval = TRUE, echo = TRUE, fig.width = 10, fig.height = 5----------------------------------
nd <- data.frame(Marriage.s = seq(from = -2.5, to = 3.5, length.out = 1e2) )

posterior_samples(mod1, pars = "^b_") %>%
  sample_n(size = 1e2) %>%
  rownames_to_column("draw") %>%
  expand(nesting(draw, b_Intercept, b_Marriage.s), a = c(-2.5, 3.5) ) %>%
  mutate(d = b_Intercept + b_Marriage.s * a) %>%
  ggplot(aes(x = a, y = d) ) +
  geom_point(data = df1, aes(x = Marriage.s, y = Divorce.s), size = 2) +
  geom_line(aes(group = draw), color = "purple", size = 0.5, alpha = 0.5) +
  labs(x = "Taux de mariage (standardisé)", y = "Taux de divorce (standardisé)") +
  theme_bw(base_size = 20)


## ----mod2, eval = TRUE, echo = TRUE, results = "hide"----------------------------------------------------------
priors <- c(
  prior(normal(0, 10), class = Intercept),
  prior(normal(0, 1), class = b),
  prior(exponential(1), class = sigma)
  )

mod2 <- brm(
  Divorce.s ~ 1 + MedianAgeMarriage.s,
  family = gaussian(),
  prior = priors,
  data = df1
  )


## ----summary-mod2, eval = TRUE, echo = TRUE--------------------------------------------------------------------
summary(mod2)


## ----posterior-mod2, eval = TRUE, echo = TRUE, fig.width = 10, fig.height = 5----------------------------------
nd <- data.frame(MedianAgeMarriage.s = seq(from = -3, to = 3.5, length.out = 1e2) )

posterior_samples(mod2, pars = "^b_") %>%
  sample_n(size = 1e2) %>%
  rownames_to_column("draw") %>%
  expand(nesting(draw, b_Intercept, b_MedianAgeMarriage.s), a = c(-2.5, 3.5) ) %>%
  mutate(d = b_Intercept + b_MedianAgeMarriage.s * a) %>%
  ggplot(aes(x = a, y = d) ) +
  geom_point(data = df1, aes(x = MedianAgeMarriage.s, y = Divorce.s), size = 2) +
  geom_line(aes(group = draw), color = "purple", size = 0.5, alpha = 0.5) +
  labs(x = "Age médian de mariage (standardisé)", y = "Taux de divorce (standardisé)") +
  theme_bw(base_size = 20)


## ----mod3, eval = TRUE, echo = TRUE, results = "hide"----------------------------------------------------------
priors <- c(
  prior(normal(0, 10), class = Intercept),
  prior(normal(0, 1), class = b),
  prior(exponential(1), class = sigma)
  )

mod3 <- brm(
  Divorce.s ~ 1 + Marriage.s + MedianAgeMarriage.s,
  family = gaussian(),
  prior = priors,
  data = df1
  )


## ----summary-mod3, eval = TRUE, echo = TRUE--------------------------------------------------------------------
posterior_summary(mod3, pars = "^b_")


## ----predictions-mod3, eval = TRUE, echo = FALSE, fig.width = 6, fig.height = 6--------------------------------
data.frame(fitted(mod3) ) %>%
  transmute(mu = Estimate, lb = Q2.5, ub = Q97.5) %>%
  bind_cols(predict(mod3) %>% data.frame() ) %>%
  bind_cols(df1) %>%
  ggplot(aes(x = Divorce.s, y = mu) ) +
  geom_pointrange(
    aes(ymin = lb, ymax = ub, y = mu),
    size = 1, shape = 1, col = "steelblue"
    ) +
  geom_text(
    data = . %>% filter(Loc %in% c("ID", "UT") ),
    aes(label = Loc),
    size = 5, nudge_x = 0.3, nudge_y = 0.3, col = "steelblue"
    ) +
  geom_abline(intercept = 0, slope = 1, lty = 2, lwd = 1) +
  theme_bw(base_size = 20) +
  labs(x = "Taux de divorce observé", y = "Taux de divorce prédit")


## ----ppc-mod3-1, eval = TRUE, echo = TRUE, fig.width = 12, fig.height = 6--------------------------------------
pp_check(mod3, type = "intervals", nsamples = 1e2, prob = 0.5, prob_outer = 0.95) +
  theme_bw(base_size = 20) + labs(x = "État", y = "Taux de divorce (standardisé)")


## ----ppc-mod3-2, eval = TRUE, echo = FALSE, fig.width = 20, fig.height = 10------------------------------------
mod3 %>%
  # adds the prediction intervals
  predict(., probs = c(0.025, 0.975) ) %>%
  data.frame %>%
  transmute(ll = Q2.5, ul = Q97.5) %>%
  # adds the fitted intervals
  bind_cols(fitted(mod3, probs = c(0.025, 0.975) ) %>% data.frame() ) %>%
  # adds the data
  bind_cols(mod3$data) %>%
  mutate(case = 1:nrow(.) ) %>%
  # plotting it
  ggplot(aes(x = case) ) +
  geom_linerange(
    aes(ymin = ll, ymax = ul),
    size = 4, alpha = 0.2
    ) +
  geom_pointrange(
    aes(ymin = Q2.5, ymax = Q97.5, y = Estimate),
    size = 1, shape = 1
  ) +
  geom_point(
    aes(
      y = Divorce.s,
      color = ifelse(Divorce.s > ll & Divorce.s < ul, "black", "orangered") ),
    size = 4, show.legend = FALSE
    ) +
  scale_x_continuous(breaks = 1:50, labels = df1$Loc, limits = c(1, 50) ) +
  scale_color_identity() +
  theme_bw(base_size = 20) +
  labs(x = "État", y = "Taux de divorce (standardisé)")


## ----leg-height, eval = TRUE, echo = TRUE----------------------------------------------------------------------
set.seed(666) # afin de pouvoir reproduire les résultats

N <- 100 # nombre d'individus
height <- rnorm(N, 179, 5) # génère N observations
leg_prop <- runif(N, 0.4, 0.5) # taille des jambes (proportion taille totale)
leg_left <- leg_prop * height + rnorm(N, 0, 0.5) # taille jambe gauche (+ erreur)
leg_right <- leg_prop * height + rnorm(N, 0, 0.5) # taille jambe droite (+ erreur)
df2 <- data.frame(height, leg_left, leg_right) # création d'une dataframe

head(df2) # affiche les six première lignes


## ----mod4, eval = TRUE, echo = TRUE, results = "hide"----------------------------------------------------------
priors <- c(
  prior(normal(174, 10), class = Intercept),
  prior(normal(0, 10), class = b),
  prior(exponential(0.01), class = sigma)
  )

mod4 <- brm(
  height ~ 1 + leg_left + leg_right,
  prior = priors,
  family = gaussian,
  data = df2
  )


## ----summary-mod4, eval = TRUE, echo = TRUE--------------------------------------------------------------------
summary(mod4) # look at the SE...


## ----pairs-mod4, eval = TRUE, echo = TRUE, fig.width = 9, fig.height = 6---------------------------------------
pairs(mod4, pars = parnames(mod4)[1:3])


## ----post-plot-mod4, eval = TRUE, echo = TRUE, fig.width = 7.5, fig.height = 5---------------------------------
post <- posterior_samples(mod4)

post %>%
  ggplot(aes(x = b_leg_left, y = b_leg_right) ) +
  geom_point(pch = 21, size = 4, color = "white", fill = "black", alpha = 0.5) +
  theme_bw(base_size = 20) +
  labs(x = expression(beta[gauche]), y = expression(beta[droite]) )


## ----plotpost-legs, eval = TRUE, echo = TRUE, fig.width = 7.5, fig.height = 5----------------------------------
library(BEST)
sum_legs <- post$b_leg_left + post$b_leg_right
plotPost(sum_legs, xlab = expression(beta[1] + beta[2]), compVal = 0)


## ----mod5, eval = TRUE, echo = TRUE, results = "hide"----------------------------------------------------------
priors <- c(
  prior(normal(174, 10), class = Intercept),
  prior(normal(0, 10), class = b),
  prior(exponential(0.01), class = sigma)
  )

mod5 <- brm(
  height ~ 1 + leg_left,
  prior = priors,
  family = gaussian,
  data = df2
  )


## ----summary-mod5, eval = TRUE, echo = TRUE--------------------------------------------------------------------
posterior_summary(mod5)


## ----fungus-data, eval = TRUE, echo = TRUE---------------------------------------------------------------------
# nombre de plantes
N <- 100

# on simule différentes tailles à l'origine
h0 <- rnorm(N, mean = 10, sd = 2)

# on assigne différents traitements et on
# simule la présence de fungus et la pousse des plantes
treatment <- rep(0:1, each = N / 2)
fungus <- rbinom(N, size = 1, prob = 0.5 - treatment * 0.4)
h1 <- h0 + rnorm(N, mean = 5 - 3 * fungus)

# on rassemble les données dans une dataframe
df3 <- data.frame(h0, h1, treatment, fungus)

head(df3)


## ----mod6, eval = TRUE, echo = TRUE, results = "hide"----------------------------------------------------------
priors <- c(
  prior(normal(0, 10), class = Intercept),
  prior(normal(0, 10), class = b),
  prior(exponential(0.01), class = sigma)
  )

mod6 <- brm(
  h1 ~ 1 + h0 + treatment + fungus,
  prior = priors,
  family = gaussian,
  data = df3
  )


## ----summary-mo6, eval = TRUE, echo = TRUE---------------------------------------------------------------------
posterior_summary(mod6)


## ----mod7, eval = TRUE, echo = TRUE, results = "hide"----------------------------------------------------------
mod7 <- brm(
  h1 ~ 1 + h0 + treatment,
  prior = priors,
  family = gaussian,
  data = df3
  )


## ----mod7bis, eval = FALSE, echo = TRUE, results = "hide"------------------------------------------------------
## mod7 <- update(mod6, formula = h1 ~ 1 + h0 + treatment)


## ----summary-mod7, eval = TRUE, echo = TRUE--------------------------------------------------------------------
summary(mod7)


## ----data-categ, eval = TRUE, echo = TRUE----------------------------------------------------------------------
data(Howell1)
df4 <- Howell1

str(df4)


## ----mod8, eval = TRUE, echo = TRUE, results = "hide"----------------------------------------------------------
priors <- c(
  prior(normal(178, 100), class = Intercept),
  prior(normal(0, 10), class = b),
  prior(exponential(0.01), class = sigma)
  )

mod8 <- brm(
  height ~ 1 + male,
  prior = priors,
  family = gaussian,
  data = df4
  )


## ----summary-mod8, eval = TRUE, echo = TRUE--------------------------------------------------------------------
fixef(mod8) # retrieves fixed effects


## ----summary-female, eval = TRUE, echo = TRUE------------------------------------------------------------------
post <- posterior_samples(mod8)
mu.male <- post$b_Intercept + post$b_male
quantile(x = mu.male, probs = c(0.025, 0.5, 0.975) )


## ----mod9, eval = TRUE, echo = TRUE, results = "hide"----------------------------------------------------------
# on crée une nouvelle colonne pour les femmes
df4 <- df4 %>% mutate(female = 1 - male)

priors <- c(
  # il n'y a plus d'intercept dans ce modèle
  # prior(normal(178, 100), class = Intercept),
  prior(normal(0, 10), class = b),
  prior(exponential(0.01), class = sigma)
  )

mod9 <- brm(
  height ~ 0 + female + male,
  prior = priors,
  family = gaussian,
  data = df4
  )


## ----summary-mod9, eval = TRUE, echo = TRUE--------------------------------------------------------------------
summary(mod9)


## ----r2, eval = FALSE, echo = FALSE, fig.width = 7.5, fig.height = 5-------------------------------------------
## library(effectsize)
## library(bayestestR)
## library(BEST
##         )
## r2 <- eta_squared_posterior(model = mod9, partial = F, type = 3, draws = 1e3, verbose = FALSE)
## # bayestestR::describe_posterior(r2, rope_range = c(0, 0.1), test = "rope")
## BEST::plotPost(r2$Marriage.s)


## ----cohen, eval = TRUE, echo = TRUE, fig.width = 7.5, fig.height = 5------------------------------------------
plotPost((post$b_male - post$b_female) / post$sigma, cex = 2, xlab = expression(delta) )


## ----milk-data, eval = TRUE, echo = TRUE-----------------------------------------------------------------------
data(milk)
df5 <- milk
str(df5)


## ----categories, eval = TRUE, echo = TRUE----------------------------------------------------------------------
df5$clade.NWM <- ifelse(df5$clade == "New World Monkey", 1, 0)
df5$clade.OWM <- ifelse(df5$clade == "Old World Monkey", 1, 0)
df5$clade.S <- ifelse(df5$clade == "Strepsirrhine", 1, 0)


## ----figure-table, eval = TRUE, echo = FALSE, out.width = "66%"------------------------------------------------
knitr::include_graphics("figures/table.png")


## ----mod10, eval = TRUE, echo = TRUE, results = "hide"---------------------------------------------------------
priors <- c(
  prior(normal(0.6, 10), class = Intercept),
  prior(normal(0, 1), class = b),
  prior(exponential(0.01), class = sigma)
  )

mod10 <- brm(
  kcal.per.g ~ 1 + clade.NWM + clade.OWM + clade.S,
  prior = priors,
  family = gaussian,
  data = df5
  )


## ----summary-mod10, eval = TRUE, echo = TRUE-------------------------------------------------------------------
summary(mod10)


## ----pairs-mod10, eval = TRUE, echo = TRUE---------------------------------------------------------------------
# retrieves posterior samples
post <- posterior_samples(mod10)

# retrieves posterior samples for each category
mu.ape <- post$b_Intercept
mu.NWM <- post$b_Intercept + post$b_clade.NWM
mu.OWM <- post$b_Intercept + post$b_clade.OWM
mu.S <- post$b_Intercept + post$b_clade.S


## ----precis-mod10, eval = TRUE, echo = TRUE--------------------------------------------------------------------
# displays a summary of the posterior samples
precis(data.frame(mu.ape, mu.NWM, mu.OWM, mu.S), prob = 0.95)


## ----quantiles-mod10, eval = TRUE, echo = TRUE-----------------------------------------------------------------
diff.NWM.OWM <- mu.NWM - mu.OWM
quantile(diff.NWM.OWM, probs = c(0.025, 0.5, 0.975) )


## ----plotpost-mod10, eval = TRUE, echo = TRUE, fig.width = 10, fig.height = 5----------------------------------
plotPost(diff.NWM.OWM, compVal = 0, ROPE = c(-0.1, 0.1) )


## ----mod11, eval = TRUE, echo = TRUE, results = "hide"---------------------------------------------------------
priors <- c(
  prior(normal(0.6, 10), class = b),
  prior(exponential(0.01), class = sigma)
  )

mod11 <- brm(
  # modèle sans intercept avec seulement un prédicteur catégoriel (facteur)
  kcal.per.g ~ 0 + clade,
  prior = priors,
  family = gaussian,
  data = df5
  )


## ----summary-mod11, eval = TRUE, echo = TRUE-------------------------------------------------------------------
summary(mod11)


## ----data-tulips, eval = TRUE, echo = TRUE---------------------------------------------------------------------
data(tulips)
df6 <- tulips

head(df6, 10)


## ----centering, eval = TRUE, echo = TRUE-----------------------------------------------------------------------
df6$shade.c <- df6$shade - mean(df6$shade)
df6$water.c <- df6$water - mean(df6$water)


## ----mod12, eval = TRUE, echo = TRUE, results = "hide"---------------------------------------------------------
priors <- c(
  prior(normal(130, 100), class = Intercept),
  prior(normal(0, 100), class = b),
  prior(exponential(0.01), class = sigma)
  )

mod12 <- brm(
  blooms ~ 1 + water.c + shade.c,
  prior = priors,
  family = gaussian,
  data = df6
  )


## ----mod13, eval = TRUE, echo = TRUE, results = "hide"---------------------------------------------------------
mod13 <- brm(
  blooms ~ 1 + water.c * shade.c,
  # equivalent to blooms ~ 1 + water.c + shade.c + water.c:shade.c
  prior = priors,
  family = gaussian,
  data = df6
  )


## ----model-comp, eval = TRUE, echo = FALSE---------------------------------------------------------------------
posterior_summary(mod12) %>%
  data.frame %>%
  rownames_to_column("term") %>%
  select(term, Estimate) %>%
  mutate(model = "mod12") %>%
  filter(term != "lp__") %>%
  bind_rows(
    posterior_summary(mod13) %>%
      data.frame %>%
      rownames_to_column("term") %>%
      select(term, Estimate) %>%
      mutate(model = "mod13") %>%
      filter(term != "lp__")
    ) %>%
  pivot_wider(names_from = model, values_from = Estimate) %>%
  data.frame


## ----plot-models, eval = TRUE, echo = FALSE, fig.height = 6, fig.width = 12------------------------------------
# `fitted()` for model b7.8
fitted(mod12) %>%
  as_tibble() %>%
  # add `fitted()` for model b7.9
  bind_rows(
    fitted(mod13) %>% 
      as_tibble()
  ) %>% 
  # we'll want to index the models
  mutate(fit = rep(c("mod12", "mod13"), each = 27) ) %>%
  # here we add the data, `d`
  bind_cols(bind_rows(df6, df6) ) %>%
  # these will come in handy for `ggplot2::facet_grid()`
  mutate(
    x_grid = paste("Water.c =", water.c),
    y_grid = paste("Modèle : ", fit)
    ) %>%
  # plot!
  ggplot(aes(x = shade.c) ) +
  geom_point(
    aes(y = blooms, group = x_grid), 
    shape = 21, color = "white", fill = "black", size = 3
    ) +
  geom_smooth(
    aes(y = Estimate, ymin = Q2.5, ymax = Q97.5),
    stat = "identity",
    color = "black",
    alpha = 0.25, size = 1
    ) +
  scale_x_continuous("Shade (centered)", breaks = c(-1, 0, 1) ) +
  ylab("Blooms") +
  facet_grid(y_grid ~ x_grid) +
  theme_bw(base_size = 20)


## ----plot-models2, eval = TRUE, echo = FALSE, fig.height = 6, fig.width = 12-----------------------------------
# `fitted()` for model b7.8
fitted(mod12) %>%
  as_tibble() %>%
  # add `fitted()` for model b7.9
  bind_rows(
    fitted(mod13) %>% 
      as_tibble()
  ) %>% 
  # we'll want to index the models
  mutate(fit = rep(c("mod12", "mod13"), each = 27) ) %>%
  # here we add the data, `d`
  bind_cols(bind_rows(df6, df6) ) %>%
  # these will come in handy for `ggplot2::facet_grid()`
  mutate(
    x_grid = paste("Water.c =", water.c),
    y_grid = paste("Modèle : ", fit)
    ) %>%
  # plot!
  ggplot(aes(x = shade.c) ) +
  geom_point(
    aes(y = blooms, group = x_grid), 
    shape = 21, color = "white", fill = "black", size = 3
    ) +
  geom_smooth(
    aes(y = Estimate, ymin = Q2.5, ymax = Q97.5),
    stat = "identity",
    color = "black",
    alpha = 0.25, size = 1
    ) +
  scale_x_continuous("Shade (centered)", breaks = c(-1, 0, 1) ) +
  ylab("Blooms") +
  facet_grid(y_grid ~ x_grid) +
  theme_bw(base_size = 20)


## ----mtcars-data, eval = TRUE, echo = TRUE---------------------------------------------------------------------
data(mtcars)
head(mtcars, 10)


## ----mtcars-plot, eval = TRUE, echo = FALSE, fig.width = 12, fig.height = 6------------------------------------
mtcars %>%
  ggplot(
    aes(
      x = disp, y = mpg, group = cyl,
      colour = as.factor(cyl), fill = as.factor(cyl)
      )
    ) +
  geom_point(pch = 21, size = 3, colour = "white") +
  geom_smooth(method = "lm") +
  theme_bw(base_size = 20)


## ----mtcars-lm, eval = TRUE, echo = TRUE-----------------------------------------------------------------------
mtcars$disp.s <- as.numeric(scale(mtcars$disp) )
mtcars$cyl.s <- as.numeric(scale(mtcars$cyl) )

m_cyl <- lm(mpg ~ disp.s * cyl.s, data = mtcars)
summary(m_cyl)


## ----mod14, eval = TRUE, echo = TRUE, results = "hide"---------------------------------------------------------
priors <- c(
  prior(normal(0, 100), class = Intercept),
  prior(normal(0, 10), class = b),
  prior(exponential(0.1), class = sigma)
  )

mod14 <- brm(
  mpg ~ 1 + disp.s * cyl.s,
  prior = priors,
  family = gaussian,
  data = mtcars
  )


## ----summary-mod14, eval = TRUE, echo = TRUE-------------------------------------------------------------------
summary(mod14)


## ----plot-mod14-1, eval = TRUE, echo = TRUE, fig.width = 12, fig.height = 6------------------------------------
plot(
  conditional_effects(mod14, effects = "disp.s:cyl.s"),
  points = TRUE,
  point_args = list(
    alpha = 0.8, shape = 21, size = 4,
    color = "white", fill = "black"
    ),
  theme = theme_bw(base_size = 20)
  )


## ----plot-mod14-2, eval = TRUE, echo = TRUE, fig.width = 12, fig.height = 6------------------------------------
plot(
  conditional_effects(mod14, effects = "disp.s:cyl.s", spaghetti = TRUE, nsamples = 1e2),
  points = TRUE, mean = FALSE,
  point_args = list(
    alpha = 0.8, shape = 21, size = 4,
    color = "white", fill = "black"
    ),
  theme = theme_bw(base_size = 20)
  )


## ----plot-mod14-3, eval = TRUE, echo = TRUE, fig.width = 12, fig.height = 6------------------------------------
plot(
  conditional_effects(
    mod14, effects = "disp.s:cyl.s",
    surface = TRUE, resolution = 1e2
    ),
  stype = "raster", # contour or raster
  surface_args = list(hjust = 0),
  theme = theme_bw(base_size = 20)
  )


## ----airquality-data, eval = TRUE, echo = TRUE-----------------------------------------------------------------
data(airquality)
df7 <- airquality[complete.cases(airquality), ] # removes NAs

head(df7, 10)


## ----mod15, eval = TRUE, echo = TRUE, results = "hide"---------------------------------------------------------
df7$Wind.s <- scale(df7$Wind)
df7$Temp.s <- scale(df7$Temp)

priors <- c(
  prior(normal(50, 10), class = Intercept),
  prior(normal(0, 10), class = b),
  prior(exponential(0.01), class = sigma)
  )

mod15 <- brm(
  Ozone ~ 1 + Wind.s + Temp.s,
  prior = priors,
  family = gaussian,
  data = df7
  )


## ----summary-mod15, eval = TRUE, echo = TRUE-------------------------------------------------------------------
summary(mod15)


## ----ppc-mod15-1, eval = TRUE, echo = TRUE, fig.width = 20, fig.height = 8-------------------------------------
pp_check(mod15, type = "intervals", nsamples = 1e2, prob = 0.5, prob_outer = 0.95) +
  theme_bw(base_size = 20) + labs(x = "Observations", y = "Ozone")


## ----ppc-mod15-2, eval = TRUE, echo = FALSE, fig.width = 20, fig.height = 10-----------------------------------
mod15 %>%
  # adds the prediction intervals
  predict(., probs = c(0.025, 0.975) ) %>%
  data.frame %>%
  transmute(ll = Q2.5, ul = Q97.5) %>%
  # adds the fitted intervals
  bind_cols(fitted(mod15, probs = c(0.025, 0.975) ) %>% data.frame() ) %>%
  # adds the data
  bind_cols(mod15$data) %>%
  mutate(case = 1:nrow(.) ) %>%
  # plotting it
  ggplot(aes(x = case) ) +
  geom_linerange(
    aes(ymin = ll, ymax = ul),
    size = 4, alpha = 0.2
    ) +
  geom_pointrange(
    aes(ymin = Q2.5, ymax = Q97.5, y = Estimate),
    size = 1, shape = 1
  ) +
  geom_point(
    aes(
      y = Ozone,
      color = ifelse(Ozone > ll & Ozone < ul, "black", "orangered") ),
    size = 4, show.legend = FALSE
    ) +
  # scale_x_continuous(breaks = 1:50, labels = df1$Loc, limits = c(1, 50) ) +
  scale_color_identity() +
  theme_bw(base_size = 20) +
  labs(x = "Observations", y = "Ozone")


## ----ppc-mod15-3, eval = TRUE, echo = TRUE, fig.width = 14, fig.height = 7-------------------------------------
pp_check(mod15, nsamples = 1e2) + theme_bw(base_size = 20) + labs(x = "Ozone", y = "Density")

