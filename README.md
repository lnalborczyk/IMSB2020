# Introduction à la modélisation statistique bayésienne : Un cours avec R, Stan, et brms <img src="https://github.com/lnalborczyk/IMSB2020/cover.png" align="right" width="300px">

[![GitHub repo size](https://img.shields.io/github/repo-size/lnalborczyk/IMSB2020?color=brightgreen&logo=github)](https://github.com/lnalborczyk/IMSB2020)
[![GitHub last commit](https://img.shields.io/github/last-commit/lnalborczyk/IMSB2020?color=orange&logo=github)](https://github.com/lnalborczyk/IMSB2020)

## Programme

L’objectif de cette formation est de vous faire découvrir la modélisation statistique bayésienne. Les concepts et outils qui seront introduits tout au long de la formation seront illustrés par des cas concrets d’analyse de données. Ce cours est construit autour du langage `R` et de l’utilisation du package `brms`, une interface au langage probabiliste `Stan`. Par conséquent, il est indispensable d’avoir quelques connaissances élémentaires du langage `R`.

La formation est proposée sous une double étiquette Collège doctoral / MaiMoSiNE (Maison de la Modélisation et de la Simulation) avec une priorité d’accès aux étudiant.e.s du collège doctoral de Grenoble.

## Pré-requis

Pour cette formation, vous pourrez utiliser votre propre ordinateur portable sur lequel vous aurez installé `R` (version >= 3.5.3 souhaitée). Nous avons fait le choix d'utiliser l'interface utilisateur RStudio. Pour faciliter nos interactions, nous vous conseillons donc de l’installer : https://www.rstudio.com/products/rstudio/.

Nous utiliserons également `Stan`, à partir de RStudio, grâce au package `rstan`. Vous trouverez toutes les infos pour installer rstan selon votre plateforme ici : https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started.

Nous utiliserons les packages listés ci-dessous, que vous pouvez installer avec une seule commande (à partir de RStudio) :

`install.packages(c("data.table", "coda", "mvtnorm", "devtools", "MASS", "ellipse", "rstan", "BayesFactor", "BEST", "coda", "LearnBayes", "markdown", "mcmc", "MCMCpack", "MuMIn", "reshape2", "rmarkdown", "brms", "tidyverse", "tidybayes", "bayesplot", "shinystan", "lme4", "patchwork"), dependencies = TRUE)`

Nous utiliserons également un package non publié sur le CRAN, que vous pouvez télécharger directement depuis Github, avec la commande suivante en R (après avoir installé "devtools") : `devtools::install_github("rmcelreath/rethinking")`. Si cette commande ne fonctionne pas, se référer aux instructions d'installation détaillées sur le répertoire Github associé au package : https://github.com/rmcelreath/rethinking.

Bien qu'une bonne connaissance de `R` soit un pré-requis d'inscription à la formation, vous trouverez [ici](https://cran.r-project.org/doc/contrib/Torfs+Brauer-Short-R-Intro.pdf) une courte introduction aux bases du langage, qui seront nécessaires afin de bien comprendre les exemples traités lors de la formation.

NB : Si la commande d'installation des packages ne fonctionne pas, il s'agit probablement d'un problème de guillemets...

## Comment lire les slides ?

### Option 1

Télécharger le répertoire entier (en cliquant sur le bouton vert) puis double-cliquer sur le fichier "CoursXX/CoursXX.html". Cela devrait ouvrir les slides dans une nouvelle fenêtre de votre navigateur par défaut, et fonctionner avec (au moins) Safari et Chrome.

### Option 2

Suivre les liens ci-dessous pour consulter les slides en ligne.

| Cours | Calendrier | Matériel |
|-------|:----------:|:--------:|
| Cours n°01 : Introduction à l'inférence bayésienne | Mardi 6 Octobre | <[slides](https://www.barelysignificant.com/IMSB2020/slides/Cours01)> |
| Cours n°02 : Modèle Beta-Binomial | Vendredi 9 Octobre | <[slides](https://www.barelysignificant.com/IMSB2020/slides/Cours02)> |
| Cours n°03 : Introduction à brms, modèle de régression linéaire | Mardi 13 Octobre | <[slides](https://www.barelysignificant.com/IMSB2020/slides/Cours03)> |
| Cours n°04 : Modèle de régression linéaire (suite) | Vendredi 16 Octobre | <[slides](https://www.barelysignificant.com/IMSB2020/slides/Cours04)> |
| Cours n°05 : Markov Chain Monte Carlo | Mardi 20 Octobre | <[slides](https://www.barelysignificant.com/IMSB2020/slides/Cours05)> |
| Cours n°06 : Modèle linéaire généralisé | Vendredi 23 Octobre | <[slides](https://www.barelysignificant.com/IMSB2020/slides/Cours06)> |
| Cours n°07 : Comparaison de modèles | Mardi 27 Octobre | <[slides](https://www.barelysignificant.com/IMSB2020/slides/Cours07)> |
| Cours n°08 : Modèles multi-niveaux | Vendredi 30 Octobre | <[slides](https://www.barelysignificant.com/IMSB2020/slides/Cours08)> |
| Cours n°09 : Modèles multi-niveaux généralisés | Mardi 3 Novembre | <[slides](https://www.barelysignificant.com/IMSB2020/slides/Cours09)> <[Zoom](https://univ-grenoble-alpes-fr.zoom.us/j/97569015383?pwd=ai9vVlJCODVvMEdNMUpFTElOdlNtUT09)> |
| Cours n°10 : Data Hackaton | Vendredi 6 Novembre | <[slides](https://www.barelysignificant.com/IMSB2020/slides/Cours10)> <[Zoom](https://univ-grenoble-alpes-fr.zoom.us/j/94061357264?pwd=cWJiZkVKejRiVWZaaEJaSS96LzNBZz09)> |

## Pour aller plus loin

### Livres

- Lambert, B. (2018). *A Student’s Guide to Bayesian Statistics*. SAGE Publications Ltd.
- McElreath, R. (2020). *Statistical Rethinking: A Bayesian Course with Examples in R and Stan*. Second Edition. CRC Press.
- Kurz, S. (2020). *Statistical Rethinking with brms, ggplot2, and the tidyverse*. Available [online](https://bookdown.org/ajkurz/Statistical_Rethinking_recoded/).
- Kruschke, J. K. (2015). *Doing Bayesian Data Analysis, Second Edition: A Tutorial with R, JAGS, and Stan*. Academic Press / Elsevier.
- Gelman, A., Carlin, J. B., Stern, H. S., Dunson, D. B., Vehtari, A., Rubin, D. B. (2013). *Bayesian Data Analysis, third edition*. London: CRC Press.

### Vidéos

- Bayes theorem. 3Blue1Brown, 22 December 2019. https://www.youtube.com/watch?v=HZGCoVF3YvM
- Binomial distributions. Probabilities of probabilities, part 1. 3Blue1Brown, 15 March 2020. https://www.youtube.com/watch?v=8idr1WZ1A7Q
- Why "probability of 0" does not mean "impossible". Probabilities of probabilities, part 2. 3Blue1Brown, 12 April 2020. https://www.youtube.com/watch?v=ZA4JkHKZM50&feature=youtu.be

## Contact

[Ladislas Nalborczyk](https://www.barelysignificant.com), prenom.nom@gmail.com
