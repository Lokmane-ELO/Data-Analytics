df <- read.table('data1TP1.txt', header=TRUE)

# Remplacez "A" par "B", "C", "D", "E" pour visualiser les autres variables.
plot(df$A,df$Y, main="Nuage de points de A par rapport à Y", xlab="A", ylab="Y")
plot(df$B,df$Y, main="Nuage de points de B par rapport à Y", xlab="B", ylab="Y")
plot(df$C,df$Y, main="Nuage de points de C par rapport à Y", xlab="C", ylab="Y")
plot(df$D,df$Y, main="Nuage de points de D par rapport à Y", xlab="D", ylab="Y")
plot(df$E,df$Y, main="Nuage de points de E par rapport à Y", xlab="E", ylab="Y")



# # Fonction pour calculer le coefficient de corrélation de Pearson
# pearson_correlation <- function(X, Y) {
#   covariance <- cov(X, Y)
#   std_dev_X <- sd(X)
#   std_dev_Y <- sd(Y)
#   
#   correlation <- covariance / (std_dev_X * std_dev_Y)
#   return(correlation)
# }
# 
# cor(df$A, df$Y, method="pearson")
# cor(df$B, df$Y, method="pearson")
# cor(df$C, df$Y, method="pearson")
# cor(df$D, df$Y, method="pearson")
# cor(df$E, df$Y, method="pearson")


# Utilisation de la fonction
# correlation = pearson_correlation(df$C, df$Y)
# print(correlation)
# 
# correlation = pearson_correlation(df$D, df$Y)
# print(correlation)
# 
# correlation = pearson_correlation(df$E, df$Y)
# print(correlation)

spearman_correlation <- function(X, Y) {
  N <- length(X)
  rank_X <- rank(X)
  rank_Y <- rank(Y)
  
  sum_square_diff_ranks <- sum((rank_X - rank_Y)^2)
  
  correlation <- 1 - ((6 * sum_square_diff_ranks) / (N^3 - N))
  
  return(correlation)
}

# Utilisation de la fonction
# correlation_spearman = spearman_correlation(df$B, df$Y)
# print(correlation_spearman)

correlation_spearman_builtin = cor(df$E, df$Y, method = "spearman")
print(correlation_spearman_builtin)
