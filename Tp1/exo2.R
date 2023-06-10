# Lecture des données
data <- read.table("data2TP1.txt", header = TRUE)

# Calcul de la moyenne et de l'écart-type
m <- mean(data$Marseille)
s <- sd(data$Marseille)

# Taille de l'échantillon
n <- length(data$Marseille)

# Moyenne théorique (coût de la vie à Marseille en 2010)
u <- 19

# Calcul du score t
t <- abs(m - u) / (s / sqrt(n))

# Affichage du score t
print(t)

# Comparaison du score t avec la valeur critique
alpha <- 0.05
df <- n - 1
critical_value <- qt(1 - alpha, df)

if (t > critical_value) {
  print("L'inflation 2010-2019 a affecté le coût de la vie à Marseille.")
} else {
  print("L'inflation 2010-2019 n'a pas affecté significativement le coût de la vie à Marseille.")
}

# Nous avons obtenu que l'inflation a affecté le coût de la vie à Marseille.

