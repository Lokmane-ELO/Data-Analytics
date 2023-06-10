##Exercice 6

# Calcul des moyennes et des écarts-types
m1 <- mean(data$Marseille)
s1 <- sd(data$Marseille)
n1 <- length(data$Marseille)

m2 <- mean(data$Aix)
s2 <- sd(data$Aix)
n2 <- length(data$Aix)

# Calcul de la statistique de test t
t <- abs(m1 - m2) / sqrt((s1^2/n1) + (s2^2/n2))

# Calcul des degrés de liberté
df <- n1 + n2 - 2

# Comparaison de la statistique de test t avec la valeur critique pour alpha = 5%
alpha <- 0.05
critical_value <- qt(1 - alpha/2, df)

if (t > critical_value) {
  print("Il existe une dépendance significative entre Marseille et Aix-en-Provence à 5%.")
} else {
  print("Il n'existe pas de dépendance significative entre Marseille et Aix-en-Provence à 5%.")
}

#Nous obtenons Il existe une dépendance significative entre Marseille et Aix-en-Provence à 5%

# Comparaison de la statistique de test t avec la valeur critique pour alpha = 2%
alpha <- 0.02
critical_value <- qt(1 - alpha/2, df)

if (t > critical_value) {
  print("Il existe une dépendance significative entre Marseille et Aix-en-Provence à 2%.")
} else {
  print("Il n'existe pas de dépendance significative entre Marseille et Aix-en-Provence à 2%.")
  
  #Nous obtenons Il n'existe pas de dépendance significative entre Marseille et Aix-en-Provence à 2%.
}