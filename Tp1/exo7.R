# a)  Calcul des valeurs théoriques 
nombre_phenotype = 4 
ratio_generiques = c(9, 3, 3, 1) # ratio génétiques pour chaque phénotype
valeur_observee = c(1528, 106, 117, 381) # valeurs observées pour chaque phénotype de plante
nombre_plante_observee = sum(valeur_observee) # nombre total de plantes observées
valeur_theorique = c(0, 0, 0, 0) # initialisation du vecteur des valeurs théoriques

for(i in seq(1, nombre_phenotype)) {
  valeur_theorique[i] = nombre_plante_observee * (ratio_generiques[i]/sum(ratio_generiques))
}
#print(fonction_khi_deux(nombre_phenotype, valeur_observee, valeur_theorique))


# b) Fonction du khi deux
fonction_khi_deux = function(nombre_classe_distinct, valeur_observee, valeur_theorique) {
  somme = 0
  for (i in seq(1, nombre_classe_distinct)) {
    Oi = valeur_observee[i]
    Ei = valeur_theorique[i]
    calcul = ((Oi - Ei)^2)/Ei
    somme = somme + calcul
  }
  somme
}
print(fonction_khi_deux(nombre_phenotype, valeur_observee, valeur_theorique))

# c) Application du test du Khi deux
score_khi_deux = fonction_khi_deux(nombre_phenotype, valeur_observee, valeur_theorique)
print(score_khi_deux)
