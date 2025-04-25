#1 Fonction pour tester si un nombre est premier
Est_premier <- function(n) {
  if (n <= 1) {
    return(FALSE)
  }
  for (i in 2:sqrt(n)) {
    if (n %% i == 0) {
      return(FALSE)
    }
  }
  return(TRUE)
}

# Fonction pour trouver les nombres premiers dans un vecteur
trouve_premier <- function(vect) {
  premier <- vect[which(sapply(vect, Est_premier))]
  cat("Voici les nombres premiers :\n")
  return(premier)
}

# Exemple d'utilisation
x <- c(11, 12, 14, 15, 16, 17, 19)
trouve_premier(x)




#2 Fonction pour résoudre une équation de second degré
resou_equa2 <- function(a, b, c) {
  delta <- b^2 - 4 * a * c
  if (delta > 0) {
    s1 <- (-b + sqrt(delta)) / (2 * a)
    s2 <- (-b - sqrt(delta)) / (2 * a)
    cat("Les deux solutions sont :\n")
    return(c(s1, s2))
  } else if (delta == 0) {
    s <- -b / (2 * a)
    cat("L'unique solution est :\n")
    return(s)
  } else {
    return("Pas de solution réelle")
  }
}

# Exemple d'utilisation
resou_equa2(1, 2,1) # x^2 + 2x + 1 = 0   une solution 
resou_equa2(1, -3, 2)   # x^2 - 3x + 2 = 0 → deux solutions
resou_equa2(1, -2, 1)   # x^2 - 2x + 1 = 0 → une solution
resou_equa2(1, 1, 1)    # x^2 + x + 1 = 0 → pas de solution réelle




#3Résolution du système d'équation linéaire à 2 équation et 2 variables
resoudre_sys2x2 <- function(a1, b1, c1, a2, b2, c2) {
  # Création de la matrice des coefficients
  A <- matrix(c(a1, b1, a2, b2), nrow = 2, byrow = TRUE)
  # Vecteur des constantes
  B <- c(c1, c2)
  
  # Vérification de l'existence de solution unique
  if (det(A) == 0) {
    return("Le système n'a pas de solution unique (déterminant nul).")
  }
  
  # Résolution du système
  solution <- solve(A, B)
  names(solution) <- c("x", "y")
  return(solution)
}
resoudre_sys2x2(1,1,3,1,-1,1)




#4 Génère f(x), calcul gradient et hessien pour une fonction du type : f(x) = sum(ai * xi^ni)

# f(x)
f_gen <- function(x, a, n) {
  sum(a * x^n)
}

# Gradient de f(x)
# Fonction pour calculer le gradient de f(x) = a(x1)^p + b(x2)^q
gradient_gen <- function(x, a, p, b, q) {
  # Calcul des dérivées partielles par rapport à x1 et x2
  grad_x1 <- a * p * x[1]^(p - 1)
  grad_x2 <- b * q * x[2]^(q - 1)
  
  # Retourner le vecteur du gradient
  return(c(grad_x1, grad_x2))
}
#exemple
# Définir les valeurs pour x1 et x2
x <- c(1, 2)   # x1 = 1, x2 = 2

# Définir les coefficients a, b et les puissances p, q
a <- 3  # Coefficient pour x1^p
p <- 2  # Exposant pour x1

b <- 4  # Coefficient pour x2^q
q <- 3  # Exposant pour x2

# Calcul du gradient
gradient_gen(x, a, p, b, q)


# Hessien de f(x)
hessian_gen <- function(x, a, n) {
  d <- length(x)  # Nombre de variables
  H <- matrix(0, nrow = d, ncol = d)  # Matrice du hessien (diagonale)
  
  # Remplir la diagonale du hessien
  H[1, 1] <- a[1] * n[1] * (n[1] - 1) * x[1]^(n[1] - 2)
  H[2, 2] <- a[2] * n[2] * (n[2] - 1) * x[2]^(n[2] - 2)
  
  return(H)
}

# Exemple d'utilisation :
hessian_gen(c(2, 4), c(1, 3), c(2, 2))




#5 Fonction pour inverser une matrice
inversion_matrice <- function(A) {
  if (det(A) == 0) {
    return("La matrice n'est pas inversible.")
  }
  return(solve(A))
}

# Exemple d'utilisation
A <- matrix(c(1, 2, 3, 4), nrow = 2)
inversion_matrice(A)
