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
data <- data %>%
  filter(!is.na(s01q01)) %>%
  mutate(sexe = case_when(
    s01q01 == 1 ~ "Homme",
    s01q01 == 2 ~ "Femme",
    TRUE ~ "Autre"
  )) 
sexe_tab <- data %>%
  count(sexe) %>%
  mutate(pourcentage = round(n / sum(n) * 100, 1))

gt(sexe_tab)

# Réparition selon l'age
data <- data %>%
  mutate(
    s01q03c = na_if(s01q03c, 9999)
  ) %>%
  distinct(id_individu, .keep_all = TRUE) %>%
  mutate(
    age = if_else(vague == 1, 2018 - s01q03c, 2019 - s01q03c),
    tranche_age = case_when(
      age < 5 ~ "0-4",
      age < 15 ~ "5-14",
      age < 25 ~ "15-24",
      age < 45 ~ "25-44",
      age < 65 ~ "45-64",
      age >= 65 ~ "65+",
      TRUE ~ NA_character_  # pour les âges NA
    ),
    tranche_age = factor(tranche_age, levels = c("0-4", "5-14", "15-24", "25-44", "45-64", "65+"))
  )

table_tranche <- data %>%
  count(tranche_age) %>%
  mutate(pourcentage = round(n / sum(n) * 100, 1))

gt(table_tranche)

ggplot(data, aes(x = tranche_age)) +
  geom_bar(fill = "#009E73") +
  labs(title = "Structure par tranche d’âge", x = "Tranche d’âge", y = "Effectif") +
  theme_minimal()

structure_menage <- data %>%
  group_by(id_menage) %>%
  summarise(
    enfants = sum(age < 15, na.rm = TRUE),
    vieux = sum(age >= 65, na.rm = TRUE),
    actifs = sum(age >= 15 & age < 65, na.rm = TRUE)
  ) %>%
  mutate(
    dep_ratio = round((enfants + vieux) / ifelse(actifs == 0, NA, actifs), 2)
  )

summary(structure_menage$dep_ratio)
data <- data %>%
  mutate(
    sexe_label = case_when(
      s01q01 == 1 ~ "Homme",
      s01q01 == 2 ~ "Femme",
      TRUE ~ NA_character_
    )
  )
#Pyramide des ages
pyramide_data <- data %>%
  filter(!is.na(sexe_label)) %>%
  count(tranche_age, sexe_label) %>%
  mutate(effectif = if_else(sexe_label == "Homme", -n, n))

ggplot(pyramide_data, aes(x = tranche_age, y = effectif, fill = sexe_label)) +
  geom_bar(stat = "identity", width = 0.7) +
  coord_flip() +
  scale_y_continuous(labels = abs) +
  scale_fill_manual(values = c("Homme" = "#0072B2", "Femme" = "#D55E00")) +
  labs(title = "Pyramide des âges", x = "Tranche d’âge", y = "Effectif", fill = "Sexe") +
  theme_minimal()
#ratio sexe
sexe_age <- data %>%
  filter(s01q01 %in% c(1, 2)) %>%
  count(tranche_age, sexe = s01q01) %>%
  pivot_wider(names_from = sexe, values_from = n, values_fill = 0) %>%
  rename(Homme = `1`, Femme = `2`) %>%
  mutate(sex_ratio = round(Homme / Femme, 2))

gt(sexe_age)
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
  labs(title = "Répartition du statut matrimonial", x = "Statut matrimonial", y = "Effectif") +
  theme_minimal() +
  theme(legend.position = "none")

# TOP 10 ETHNIES
data <- data %>%
  mutate(ethnie_lib = case_when(
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
  ))

ethnie_table <- data %>%
  filter(!is.na(ethnie_lib)) %>%
  count(ethnie = ethnie_lib, sort = TRUE) %>%
  mutate(pourcentage = round(n / sum(n) * 100, 1)) %>%
  head(10)

gt(ethnie_table)

ggplot(ethnie_table, aes(x = reorder(ethnie, n), y = n)) +
  geom_bar(stat = "identity", fill = "#56B4E9") +
  coord_flip() +
  labs(title = "Top 10 des ethnies", x = "Ethnie", y = "Effectif") +
  theme_minimal()

# RELIGION
data <- data %>%
  mutate(religion_lib = case_when(
    s01q14 == 1 ~ "Musulman",
    s01q14 == 2 ~ "Chrétien",
    s01q14 == 3 ~ "Animiste",
    s01q14 == 4 ~ "Autre religion",
    s01q14 == 5 ~ "Sans religion"
  ))

religion_table <- data %>%
  filter(!is.na(religion_lib)) %>%
  count(religion = religion_lib, sort = TRUE) %>%
  mutate(pourcentage = round(n / sum(n) * 100, 1))

gt(religion_table)

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
data <- data %>%
  mutate(tel_possede = case_when(
    s01q36 == 1 ~ "Oui",
    s01q36 == 2 ~ "Non",
    TRUE ~ "Non renseigné"
  ))

tel_table <- data %>%
  filter(!is.na(tel_possede)) %>%
  count(tel_possede, sort = TRUE) %>%
  mutate(pourcentage = round(n / sum(n) * 100, 1))

knitr::kable(tel_table, caption = "Distribution de la possession de téléphone")

ggplot(tel_table, aes(x = reorder(tel_possede, n), y = n)) +
  geom_bar(stat = "identity", fill = "#D55E00") +
  coord_flip() +
  labs(title = "Possession de téléphone", x = "Possède un téléphone", y = "Effectif") +
  theme_minimal()
tel_par_age <- data %>%
  filter(!is.na(tel_possede)) %>%
  count(tranche_age, tel_possede) %>%
  group_by(tranche_age) %>%
  mutate(pct = round(n / sum(n) * 100, 1))
gt(tel_par_age)
ggplot(tel_par_age, aes(x = tranche_age, y = pct, fill = tel_possede)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Possession de téléphone selon l’âge", y = "Pourcentage") +
  theme_minimal()

