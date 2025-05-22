Sys.which("make")

install.packages("pkgbuild")  # au cas où il n'est pas installé
library(pkgbuild)
pkgbuild::has_build_tools(debug = TRUE)


install.packages("tidyverse")
install.packages("haven")
install.packages("janitor")
install.packages("gt")
library(tidyverse)
library(haven)   # Pour importer les fichiers STATA (.dta)
library(janitor) # Pour nettoyer les noms de variables
library(gt) 
# base
data <- read_dta("C:/Users/HP/Desktop/S4/COURS R/EXPOSE/s01_me_SEN2018.dta") %>% 
  clean_names()
#exploration
glimpse(data)
#menages et individus
data <- data %>%
  mutate(id_menage = paste(vague, grappe, menage, sep = "_"))
data <- data %>%
  mutate(id_individu = paste(id_menage, s01q00a, sep = "_"))

n_menages <- n_distinct(data$id_menage)
n_individus <- nrow(data)
paste("Nombre total d'individus :", n_individus)
paste("Nombre total de ménages :", n_menages)
#Taille des menages
taille_menage <- data %>%
  group_by(id_menage) %>%
  summarise(taille = n()) %>%
  ungroup()
taille_menage %>%
  summarise(
    moyenne = mean(taille),
    mediane = median(taille),
    min = min(taille),
    max = max(taille)
  )
ggplot(taille_menage, aes(x = taille)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribution de la taille des ménages", x = "Nombre de membres", y = "Fréquence")
#Répartition par sexe
data %>%
  filter(!is.na(s01q01)) %>%
  mutate(sexe = case_when(
    s01q01 == 1 ~ "Homme",
    s01q01 == 2 ~ "Femme",
    TRUE ~ "Autre"
  )) %>%
  count(sexe) %>%
  mutate(pourcentage = round(n / sum(n) * 100, 1))
# Réparition selon l'age
# Calcul de l'âge et création des tranches
data <- data %>%
  distinct(id_individu, .keep_all = TRUE) %>%
  mutate(
    age = if_else(vague == 1, 2021 - s01q03c, 2022 - s01q03c),
    tranche_age = case_when(
      age < 5 ~ "0-4",
      age < 15 ~ "5-14",
      age < 25 ~ "15-24",
      age < 45 ~ "25-44",
      age < 65 ~ "45-64",
      TRUE ~ "65+"
    ),
    # Ordre des tranches
    tranche_age = factor(tranche_age, levels = c("0-4", "5-14", "15-24", "25-44", "45-64", "65+"))
  )

# Tableau de fréquences
table_tranche <- data %>%
  count(tranche_age) %>%
  mutate(pourcentage = round(n / sum(n) * 100, 1))

# Affichage du tableau
knitr::kable(table_tranche, caption = "Distribution des individus par tranche d'âge")

# Graphique en barres
ggplot(data, aes(x = tranche_age)) +
  geom_bar(fill = "#009E73") +
  labs(title = "Structure par tranche d’âge", x = "Tranche d’âge", y = "Effectif") +
  theme_minimal()
# Calcul de l'âge, tranches d’âge, sexe étiqueté avec pyramide des ages
data <- data %>%
  distinct(id_individu, .keep_all = TRUE) %>%
  mutate(
    age = if_else(vague == 1, 2021 - s01q03c, 2022 - s01q03c),
    tranche_age = case_when(
      age < 5 ~ "0-4",
      age < 15 ~ "5-14",
      age < 25 ~ "15-24",
      age < 45 ~ "25-44",
      age < 65 ~ "45-64",
      TRUE ~ "65+"
    ),
    tranche_age = factor(tranche_age, levels = c("0-4", "5-14", "15-24", "25-44", "45-64", "65+")),
    sexe_label = case_when(
      s01q01 == 1 ~ "Homme",
      s01q01 == 2 ~ "Femme",
      TRUE ~ NA_character_
    )
  )

# Agrégation par tranche d’âge et sexe
pyramide_data <- data %>%
  filter(!is.na(sexe_label)) %>%
  count(tranche_age, sexe_label) %>%
  mutate(effectif = if_else(sexe_label == "Homme", -n, n))

# Graphique pyramide des âges
ggplot(pyramide_data, aes(x = tranche_age, y = effectif, fill = sexe_label)) +
  geom_bar(stat = "identity", width = 0.7) +
  coord_flip() +
  scale_y_continuous(labels = abs) +
  scale_fill_manual(values = c("Homme" = "#0072B2", "Femme" = "#D55E00")) +
  labs(
    title = "Pyramide des âges",
    x = "Tranche d’âge",
    y = "Effectif",
    fill = "Sexe"
  ) +
  theme_minimal()
#ratio sexe
sexe_age <- data %>%
  filter(s01q01 %in% c(1, 2)) %>%  # On garde que les sexes valides
  count(tranche_age, sexe = s01q01) %>%
  pivot_wider(names_from = sexe, values_from = n, values_fill = 0) %>%
  rename(Homme = `1`, Femme = `2`) %>%
  mutate(sex_ratio = round(Homme / Femme, 2))

sexe_age %>% gt()
#Répartition situation matrimonial


data %>%
  filter(!is.na(s01q07)) %>%
  count(statut = s01q07) %>%
  mutate(
    libelle = case_when(
      statut == 1 ~ "Célibataire",
      statut == 2 ~ "Marié(e) monogame",
      statut == 3 ~ "Marié(e) polygame",
      statut == 4 ~ "Union libre",
      statut == 5 ~ "Veuf(ve)",
      statut == 6 ~ "Divorcé(e)",
      statut == 7 ~ "Séparé(e)",
      statut == 11 ~ ".A"
    ),
    pourcentage = round(n / sum(n) * 100, 1)
  ) %>%
  ggplot(aes(x = reorder(libelle, -n), y = n, fill = libelle)) +
  geom_bar(stat = "identity") +
  labs(title = "Répartition du statut matrimonial",
       x = "Statut matrimonial",
       y = "Effectif") +
  theme_minimal() +
  theme(legend.position = "none")

# TOP 10 ETHNIES
data <- data %>%
  mutate(
    ethnie_lib = case_when(
      s01q16 == 1 ~ "Wolof/Lébou",
      s01q16 == 2 ~ "Sérère",
      s01q16 == 3 ~ "Poular",
      s01q16 == 4 ~ "Soninké",
      s01q16 == 5 ~ "Diola",
      s01q16 == 6 ~ "Mandingue/Socé",
      s01q16 == 7 ~ "Balante",
      s01q16 == 8 ~ "Bambara",
      s01q16 == 9 ~ "Malinké",
      s01q16 == 10 ~ "Autres ethnies",
      s01q16 == 11 ~ "Naturalisé",
      s01q16 == 12 ~ "Mandiack/Mankagne",
      s01q16 == 13 ~ "Maure",
      s01q16 == 101 ~ ".A"
    )
  )

ethnie_table <- data %>%
  filter(!is.na(ethnie_lib)) %>%
  count(ethnie = ethnie_lib, sort = TRUE) %>%
  mutate(pourcentage = round(n / sum(n) * 100, 1)) %>%
  head(10)

# Affichage tableau
print(ethnie_table, caption = "Top 10 des ethnies les plus fréquentes")

# Graphique barres ETHNIE
ggplot(ethnie_table, aes(x = reorder(ethnie, n), y = n)) +
  geom_bar(stat = "identity", fill = "#56B4E9") +
  coord_flip() +
  labs(title = "Top 10 des ethnies", x = "Ethnie", y = "Effectif") +
  theme_minimal()

# RELIGION
data <- data %>%
  mutate(
    religion_lib = case_when(
      s01q14 == 1 ~ "Musulman",
      s01q14 == 2 ~ "Chrétien",
      s01q14 == 3 ~ "Animiste",
      s01q14 == 4 ~ "Autre religion",
      s01q14 == 5 ~ "Sans religion"
    )
  )

religion_table <- data %>%
  filter(!is.na(religion_lib)) %>%
  count(religion = religion_lib, sort = TRUE) %>%
  mutate(pourcentage = round(n / sum(n) * 100, 1))

# Affichage tableau
print(religion_table, caption = "Répartition selon la religion")

# Graphique barres RELIGION
ggplot(religion_table, aes(x = reorder(religion, n), y = n)) +
  geom_bar(stat = "identity", fill = "#F0E442") +
  coord_flip() +
  labs(title = "Répartition selon la religion", x = "Religion", y = "Effectif") +
  theme_minimal()
#Possession de téléphone
data <- data %>%
  mutate(
    tel_possede = case_when(
      s01q36 == 1 ~ "Oui",
      s01q36 == 2 ~ "Non",
      TRUE ~ "Non renseigné"
    )
  )
tel_table <- data %>%
  filter(!is.na(tel_possede)) %>%
  count(tel_possede, sort = TRUE) %>%
  mutate(pourcentage = round(n / sum(n) * 100, 1))

knitr::kable(tel_table, caption = "Distribution de la possession de téléphone")
ggplot(tel_table, aes(x = reorder(tel_possede, n), y = n)) +
  geom_bar(stat = "identity", fill = "#D55E00") +
  coord_flip() +
  labs(
    title = "Possession de téléphone",
    x = "Possède un téléphone",
    y = "Effectif"
  ) +
  theme_minimal()
# Croisement sexe et possession téléphone
tel_sexe <- data %>%
  filter(!is.na(tel_possede), s01q01 %in% c(1, 2)) %>%
  mutate(sexe = if_else(s01q01 == 1, "Homme", "Femme")) %>%
  count(sexe, tel_possede) %>%
  group_by(sexe) %>%
  mutate(pourcentage = round(n / sum(n) * 100, 1))

# Tableau
knitr::kable(tel_sexe, caption = "Possession de téléphone selon le sexe")

# Graphique
ggplot(tel_sexe, aes(x = tel_possede, y = pourcentage, fill = sexe)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Possession de téléphone selon le sexe",
    x = "Possession de téléphone",
    y = "Pourcentage"
  ) +
  theme_minimal()
