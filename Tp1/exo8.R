# Données observées
form = rbind(c(29, 5, 46), c(40, 32, 8), c(18, 22, 0))
color = rbind(c(20, 60),  c(29, 51), c(12, 28))

n_form = sum(form)  # total des observations pour la forme
n_color = sum(color)  # total des observations pour la couleur
matrice_theorique = function(donnes_observer) {
  theorique = matrix(nrow = nrow(donnes_observer), ncol = ncol(donnes_observer))
  for(i in seq(1, nrow(donnes_observer))) {
    for(j in seq(1, ncol(donnes_observer))) {
      theorique[i,j] = ( sum(donnes_observer[i,]) * sum(donnes_observer[,j]) ) / n
    }
  }
  theorique
}
# Calcul des valeurs théoriques
val_theo_form = matrice_theorique(form)
val_theo_color = matrice_theorique(color)
fonction_khi_deux_matrice = function(nombre_classe_distinct, nombre_different_param, valeur_observee, valeur_theorique) {
  # sum(((form - val_theo_form)^2)/val_theo_form)
  somme = 0
  for (i in seq(1, nombre_classe_distinct)) {
    for(j in seq(1, nombre_different_param)) {
      Oi = valeur_observee[i, j]
      Ei = valeur_theorique[i, j]
      calcul = ((Oi - Ei)^2)/Ei
      somme = somme + calcul
    }
  }
  somme
}
# Calcul du Khi-deux pour la forme et la couleur
khi_deux_form = fonction_khi_deux_matrice(nrow(form), ncol(form), form, val_theo_form)
khi_deux_color = fonction_khi_deux_matrice(nrow(color), ncol(color), color, val_theo_color)

print(khi_deux_form)
print(khi_deux_color)