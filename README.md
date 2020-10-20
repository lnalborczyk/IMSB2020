# Introduction à la modélisation statistique bayésienne : Un cours avec R, Stan, et brms

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

NB : si la commande d'installation des packages ne fonctionne pas, il s'agit probablement d'un problème de guillemets...

## Comment lire les slides ?

### Option 1

Télécharger le répertoire entier (en cliquant sur le bouton vert) puis double-cliquer sur le fichier "CoursXX/CoursXX.html". Cela devrait ouvrir les slides dans une nouvelle fenêtre de votre navigateur par défaut, et fonctionner avec (au moins) Safari et Chrome.

### Option 2

Suivre les liens ci-dessous pour consulter les slides en ligne (MDP Zoom : IMSB2020).

Cours n°01 : Introduction à l'inférence bayésienne <br>
> Mardi 6 Octobre de 14h à 16h. Lien vers [Zoom](https://univ-grenoble-alpes-fr.zoom.us/j/93467180046?pwd=Si9OSnl6SzdTUVg4U1UwSTFyOUVEdz09), [Slides](https://www.barelysignificant.com/IMSB2020/slides/Cours01).

Cours n°02 : Modèle beta-binomial <br>
> Vendredi 9 Octobre de 14h à 16h. Lien vers [Zoom](https://univ-grenoble-alpes-fr.zoom.us/j/96169760384?pwd=cmFqUEJ0TTdEWjYwN29Ya0lPUFF2Zz09), [Slides](https://www.barelysignificant.com/IMSB2020/slides/Cours02).

Cours n°03 : Introduction à brms, modèle de régression linéaire <br>
> Mardi 13 Octobre de 14h à 16h. Lien vers [Zoom](https://univ-grenoble-alpes-fr.zoom.us/j/97299937306?pwd=ZDhPZmI2N3EyK3lQRjl2SkhaTEF6Zz09), [Slides](https://www.barelysignificant.com/IMSB2020/slides/Cours03).

Cours n°04 : Modèle de régression linéaire (suite) <br>
> Vendredi 16 Octobre de 14h à 16h. Lien vers [Zoom](https://univ-grenoble-alpes-fr.zoom.us/j/98781324900?pwd=S1dLbjhPK3VrQnRZNmN0a2kzQ0t0dz09), [Slides](https://www.barelysignificant.com/IMSB2020/slides/Cours04).

Cours n°05 : Markov Chain Monte Carlo <br>
> Mardi 20 Octobre de 14h à 16h. Lien vers [Zoom](https://univ-grenoble-alpes-fr.zoom.us/j/98264501064?pwd=VFlEU1lVQmNWNlArTEp6MmZaeFdzdz09), [Slides](https://www.barelysignificant.com/IMSB2020/slides/Cours05).

Cours n°06 : Modèle linéaire généralisé <br>
> Vendredi 23 Octobre de 14h à 16h. Lien vers [Zoom](https://univ-grenoble-alpes-fr.zoom.us/j/93336809011?pwd=TFFLUElkbHdjbkx4OFFuSnFSVENsdz09), [Slides](https://www.barelysignificant.com/IMSB2020/slides/Cours06).

Cours n°07 : Comparaison de modèles <br>
> Mardi 27 Octobre de 14h à 16h. Lien vers [Zoom](https://univ-grenoble-alpes-fr.zoom.us/j/92690924574?pwd=MFlHell2eHFReHl5R3JkRnpOWTlzQT09), [Slides](https://www.barelysignificant.com/IMSB2020/slides/Cours07).

Cours n°08 : Modèles multi-niveaux <br>
> Vendredi 30 Octobre de 14h à 16h. Lien vers [Zoom](https://univ-grenoble-alpes-fr.zoom.us/j/94624686231?pwd=aWVFdzZOZ2VSKy8xaUdnUWtnamlBdz09), [Slides](https://www.barelysignificant.com/IMSB2020/slides/Cours08).

Cours n°09 : Modèles multi-niveaux généralisés <br>
> Mardi 3 Novembre de 14h à 16h. Lien vers [Zoom](https://univ-grenoble-alpes-fr.zoom.us/j/97569015383?pwd=ai9vVlJCODVvMEdNMUpFTElOdlNtUT09), [Slides](https://www.barelysignificant.com/IMSB2020/slides/Cours09).

Cours n°10 : Data Hackaton <br>
> Vendredi 6 Novembre de 14h à 16h. Lien vers [Zoom](https://univ-grenoble-alpes-fr.zoom.us/j/94061357264?pwd=cWJiZkVKejRiVWZaaEJaSS96LzNBZz09), [Slides](https://www.barelysignificant.com/IMSB2020/slides/Cours10).

| Cours | Calendrier | Contenu |
| ----- | -----------| ------- |
| Cours n°01 | Mardi 6 Octobre de 14h à 16h | Introduction à l'inférence bayésienne <[slides](https://www.barelysignificant.com/IMSB2020/slides/Cours01)> |
| Cours n°02 | Vendredi 9 Octobre de 14h à 16h | Modèle beta-binomial <[slides](https://www.barelysignificant.com/IMSB2020/slides/Cours02)> |
| Cours n°03 | Mardi 13 Octobre de 14h à 16h | Introduction à brms, modèle de régression linéaire <[slides](https://www.barelysignificant.com/IMSB2020/slides/Cours03)> |
| Cours n°04 | Vendredi 16 Octobre de 14h à 16h | Modèle de régression linéaire (suite) <[slides](https://www.barelysignificant.com/IMSB2020/slides/Cours04)> |
| Cours n°05 | Mardi 20 Octobre de 14h à 16h | Markov Chain Monte Carlo <[slides](https://www.barelysignificant.com/IMSB2020/slides/Cours05)> |
| Cours n°06 | Vendredi 23 Octobre de 14h à 16h | Modèle linéaire généralisé <[slides](https://www.barelysignificant.com/IMSB2020/slides/Cours06)> |
| Cours n°07 | Mardi 27 Octobre de 14h à 16h | Comparaison de modèles <[slides](https://www.barelysignificant.com/IMSB2020/slides/Cours07)> |
| Cours n°08 | Vendredi 30 Octobre de 14h à 16h | Modèles multi-niveaux  <[slides](https://www.barelysignificant.com/IMSB2020/slides/Cours08)> |
| Cours n°09 | Mardi 3 Novembre de 14h à 16h | Modèles multi-niveaux généralisés <[slides](https://www.barelysignificant.com/IMSB2020/slides/Cours09)> |
| Cours n°10 | Vendredi 6 Novembre de 14h à 16h | Data Hackaton <[slides](https://www.barelysignificant.com/IMSB2020/slides/Cours10)> |

## Pour aller plus loin

### Livres

- Lambert, B. (2018). *A Student’s Guide to Bayesian Statistics*. SAGE Publications Ltd.
- McElreath, R. (2015). *Statistical Rethinking: A Bayesian Course with Examples in R and Stan*. CRC Press.
- McElreath, R. (2020). *Statistical Rethinking: A Bayesian Course with Examples in R and Stan*. Second Edition. CRC Press.
- Kurz, S. (2019). *Statistical Rethinking with brms, ggplot2, and the tidyverse*. Available [online](https://bookdown.org/ajkurz/Statistical_Rethinking_recoded/).
- Kruschke, J. K. (2015). *Doing Bayesian Data Analysis, Second Edition: A Tutorial with R, JAGS, and Stan*. Academic Press / Elsevier.
- Gelman, A., Carlin, J. B., Stern, H. S., Dunson, D. B., Vehtari, A., Rubin, D. B. (2013). *Bayesian Data Analysis, third edition*. London: CRC Press.

### Articles, blogs

- Visualisation des algorithmes MCMC : https://chi-feng.github.io/mcmc-demo/
- Stein's paradox in statistics (Efron & Morris, 1977) : http://statweb.stanford.edu/~ckirby/brad/other/Article1977.pdf

### Vidéos

- Bayes theorem. 3Blue1Brown, 22 December 2019. https://www.youtube.com/watch?v=HZGCoVF3YvM
- Binomial distributions. Probabilities of probabilities, part 1. 3Blue1Brown, 15 March 2020. https://www.youtube.com/watch?v=8idr1WZ1A7Q
- Why "probability of 0" does not mean "impossible". Probabilities of probabilities, part 2. 3Blue1Brown, 12 April 2020. https://www.youtube.com/watch?v=ZA4JkHKZM50&feature=youtu.be

## Contact

Ladislas Nalborczyk, prenom.nom@gmail.com
