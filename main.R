#~############################################################################~#
# Preparation ----
#~############################################################################~#

#~=======================================================~=
## Libraries ----
#~=======================================================~=

library(tidyverse)
library(scales)

#~=======================================================~=
## Elements ----
#~=======================================================~=

age_retraite <- 65
# https://ourworldindata.org/life-expectancy
esperance_de_vie <- 84

bonifications_avant <- data.frame(
  age = 25:age_retraite
) %>% mutate(
  bonifications = case_when(
    age >= 25 & age <= 34 ~ 7 / 100,
    age >= 35 & age <= 44 ~ 10 / 100,
    age >= 45 & age <= 54 ~ 15 / 100,
    age >= 55 & age <= 65 ~ 18 / 100
  )
)

bonifications_apres <- data.frame(
  age = 25:age_retraite
) %>% mutate(
  bonifications = case_when(
    age >= 25 & age <= 44 ~ 9 / 100,
    age >= 45 & age <= 65 ~ 14 / 100
  )
)

taux_conversion_avant <- 6.8 / 100
taux_conversion_apres <- 6.0 / 100

#~=======================================================~=
## Functions ----
#~=======================================================~=

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Small functions ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
get_salaire_assure_avant <- function(salaire_brut_annuel, franchise = 25725) {
  return(max(0, salaire_brut_annuel - franchise))
}

get_salaire_assure_apres <- function(salaire_brut_annuel, deduction = 20 / 100) {
  return(max(0, salaire_brut_annuel - deduction * salaire_brut_annuel))
}

get_capital_acumule <- function(salaire_assure, bonifications) {
  return(sum(bonifications * salaire_assure))
}

rente_percue <- function(capital_accumule, taux_conversion) {
  return(capital_accumule * taux_conversion)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Big comparison ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
compare_reformes <- function(
    salaire_brut_annuel, 
    duree_carriere = age_retraite-25, 
    duree_retraite = esperance_de_vie-age_retraite
) {
  
  # Avant la réforme
  salaire_assure_avant <- get_salaire_assure_avant(salaire_brut_annuel)
  cotisations_avant <- sum(bonifications_avant$bonifications * salaire_assure_avant)
  capital_accumule_avant <- get_capital_acumule(salaire_assure_avant, bonifications_avant$bonifications)
  rente_annuelle_avant <- rente_percue(capital_accumule_avant, taux_conversion_avant)
  total_rente_avant <- rente_annuelle_avant * duree_retraite
  difference_nette_avant <- total_rente_avant - cotisations_avant
  
  # Après la réforme
  salaire_assure_apres <- get_salaire_assure_apres(salaire_brut_annuel)
  cotisations_apres <- sum(bonifications_apres$bonifications * salaire_assure_apres)
  capital_accumule_apres <- get_capital_acumule(salaire_assure_apres, bonifications_apres$bonifications)
  rente_annuelle_apres <- rente_percue(capital_accumule_apres, taux_conversion_apres)
  total_rente_apres <- rente_annuelle_apres * duree_retraite
  difference_nette_apres <- total_rente_apres - cotisations_apres
  
  # Résultat
  result <- data.frame(
    "Scénario" = c("Avant réforme", "Après réforme"),
    "Salaire brut mensuel" = c(salaire_brut_annuel/12, salaire_brut_annuel/12),
    "Salaire brut annuel" = c(salaire_brut_annuel, salaire_brut_annuel),
    "Salaire assuré" = c(salaire_assure_avant, salaire_assure_apres),
    "Cotisations totales" = c(cotisations_avant, cotisations_apres),
    "Capital accumulé" = c(capital_accumule_avant, capital_accumule_apres),
    "Rente annuelle" = c(rente_annuelle_avant, rente_annuelle_apres),
    "Total des rentes sur esperance de vie" = c(total_rente_avant, total_rente_apres),
    "Différence nette" = c(difference_nette_avant, difference_nette_apres)
  )
  
  return(result)
}

#~############################################################################~#
# Utilisation ----
#~############################################################################~#

#~=======================================================~=
## Calculs ----
#~=======================================================~=
# Exemple d'utilisation
salaire_brut_annuel <- 4000*12  # 4 000 CHF par mois
compare_reformes(salaire_brut_annuel) %>% glimpse()

# Comparer des salaires
salaires_bruts_annuels <- seq(23000/12, 10000, by = 500) * 12
resultats_comparer_salaire <- map_dfr(salaires_bruts_annuels, compare_reformes)

#~=======================================================~=
## Graphiques ----
#~=======================================================~=

# Différence nette en fonction des salaires
resultats_comparer_salaire %>% 
  ggplot(aes(x = Salaire.brut.mensuel, y = Différence.nette, color = `Scénario`)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Différence nette en fonction du salaire brut mensuel",
    x = "Salaire brut annuel",
    y = "Différence nette"
  ) +
  theme_bw() +
  scale_y_continuous(labels = dollar_format(prefix = "", suffix = " CHF"))

ggsave("diff_nette.png", width = 8, height = 4, dpi = 1000)

# Rente annuelle en fonction des salaires
resultats_comparer_salaire %>% 
  ggplot(aes(x = Salaire.brut.mensuel, y = `Rente.annuelle`, color = `Scénario`)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Rente annuelle en fonction du salaire brut mensuel",
    x = "Salaire brut annuel",
    y = "Rente annuelle"
  ) +
  theme_bw() +
  scale_y_continuous(labels = dollar_format(prefix = "", suffix = " CHF"))

ggsave("rente.png", width = 8, height = 4, dpi = 1000)
