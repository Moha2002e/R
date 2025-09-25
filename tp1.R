# -------------------------------
# État de Batracie – Analyse ANOVA
# -------------------------------

# 1. Charger le fichier CSV
# ⚠ Attention aux chemins Windows : utiliser / ou bien \\ pour éviter l’erreur "\U"
data <- read.csv("C:/Users/moham/Documents/Cours/Machine_Learning/Exercices/Datasets/sante_batracie_2.csv",
                 sep = ",", header = TRUE)

# 2. Échantillonnage aléatoire de 150 individus (reproductible)
set.seed(123)  
sample_data <- data[sample(1:nrow(data), 150), ]

# 3. Vérification de la répartition des niveaux d’études
table(sample_data$edlevel)   # très déséquilibré : beaucoup niveau 1, peu dans les autres

# 4. Statistiques descriptives sur le nombre de consultations
summary(sample_data$docvis)  # moyenne ≈ 3.4, médiane = 2, beaucoup de zéros, max = 37

# 5. Visualisation par boxplot
boxplot(docvis ~ edlevel, data = sample_data,
        xlab = "Niveau d'études",
        ylab = "Nombre de consultations",
        main = "Consultations médicales selon le niveau d'études")
# → Distributions asymétriques, présence d’outliers

# 6. ANOVA : comparaison des moyennes de consultations selon edlevel
anova_model <- aov(docvis ~ as.factor(edlevel), data = sample_data)
summary(anova_model)
# Résultat : F(3,146) = 1.434, p = 0.235
# → Pas de différence significative entre niveaux d’études

# 7. Test post-hoc de Tukey pour comparer les paires de groupes
TukeyHSD(anova_model)
# Tous les p-values > 0.05 → aucune paire de niveaux n’est significativement différente

# 8. Conclusion (à écrire dans le rapport/examen)
# Sur un échantillon de 150 individus, on ne met pas en évidence de différence
# significative du nombre moyen de consultations médicales entre niveaux d’études (p=0.235).
# Limites : effectifs très déséquilibrés + variable docvis asymétrique (beaucoup de zéros, outliers).
# Une alternative plus robuste serait un test non paramétrique (Kruskal-Wallis)
# ou un modèle de régression de type Poisson.

# -----------------------
# Lecture du second fichier
# -----------------------# ============================================================
# RÉGRESSION ET CORRÉLATION MULTIPLE – Énoncé "Fromage"
# ============================================================

# Objectif :
# Étudier si le rendement fromager (RFESC, kg/100L lait) dépend
# de caractéristiques physico-chimiques du lait.
# Construire un modèle prédictif fiable à partir des mesures disponibles.

# -----------------------
# Lecture du fichier
# -----------------------
fromage <- read.table("C:/Users/moham/Documents/Cours/Machine_Learning/Exercices/Datasets/RdtFromage.txt",
                      header = TRUE, sep = "\t")

# Aperçu et corrélations
str(fromage)        # Vérification structure (41 obs., 17 variables)
cor(fromage)        # Matrice de corrélations pour repérer multicolinéarités
pairs(fromage)      # Matrice de dispersion (relations + outliers)

# -----------------------
# Régressions
# -----------------------

# 1. Modèle complet : RFESC expliqué par toutes les autres variables
model_full <- lm(RFESC ~ ., data = fromage)
summary(model_full)

# Résultats modèle complet :
# - R² ajusté ≈ 0.81 → modèle globalement significatif
# - Coeffs notables : CNE (+), ES (−), VRG (+), MAT (tendance −)
# - Attention : multicolinéarités possibles (ex. DMM–D10–D90 très corrélées)

# 2. Sélection pas à pas (AIC, les deux sens)
model_step <- step(model_full, direction = "both")
summary(model_step)

# Résultats modèle final :
# - Variables retenues : MAT (−), CNE (+), NPN, CAT (+), CAS, ES (−), VRG (+), FMG
# - R² ajusté ≈ 0.84, F(8,32)=27.67, p < 0.001
# - Coeffs significatifs (p<0.05) : MAT, CNE, CAT, ES, VRG
# - Conclusion : RFESC ↑ avec CNE, CAT, VRG ; ↓ avec MAT et ES

# 3. Diagnostics graphiques
par(mfrow = c(2, 2))
plot(model_step)

# Interprétation finale (type examen) :
# - Le rendement fromager dépend principalement de la matière sèche (MAT),
#   de la caséine (CNE), de certaines charges azotées (CAT), de l’extrait sec (ES)
#   et des propriétés mécaniques (VRG).
# - Le modèle final est significatif et prédictif (R² ajusté > 0.80).
# - Mais attention : taille d’échantillon faible (n=41) et corrélations fortes
#   entre certaines variables → prudence dans la généralisation.
