# Source : https://en.wikipedia.org/wiki/Line–line_intersection
# L'algo se base sur les "parametres de bezier" t et u
# t et u sont des fractions
# Il y a intersection SSi 0<=t<=1 ET 0<=u<=1
intersect <- function(x1, y1, x2, y2, x3, y3, x4, y4) {

  t_num <- (x1 - x3)*(y3 - y4) - (y1 - y3)*(x3 - x4)
  t_den <- (x1 - x2)*(y3 - y4) - (y1 - y2)*(x3 - x4)
  
  u_num <- -((x1 - x2)*(y1 - y3) - (y1 - y2)*(x1 - x3))
  u_den <- (x1 - x2)*(y3 - y4) - (y1 - y2)*(x3 - x4)
  
  if(t_den == 0 || u_den == 0) return(NULL) # Lignes parallèles...
  
  t <- t_num / t_den
  u <- u_num / u_den
  
  if ((t > 0 && t <= 1) && (u > 0 && u <= 1)) { # Intersection !
    p_x <- x1 + t*(x2 - x1)
    p_y <- y1 + t*(y2 - y1)
    return(c(p_x, p_y))
  } else {
    return(NULL)
  }
}

# Retourne le nombre de recouvrement
# d'un point parmi deux polygones
beta <- function(point, polygon1, polygon2) {
  x <- point[1]
  y <- point[2]
  b <- if (polygon1$inside(x, y)) 1 else 0
  b <- b + (if (polygon2$inside(x, y))  1 else 0)
  return(b)
}

# Points -> segments
points_to_segments <- function(points) {
  segments <- lapply(seq_len(length(points) - 1), function(i) {
    list(c(points[[i]][1], points[[i]][2]), 
      c(points[[i + 1]][1], points[[i + 1]][2]))
  })
  
  return(segments)
}

# Teste si un segment
# est un inclus dans un autre segment
is_subline <- function(A, B, C, D) {
  if(!collinear(C, D, A)) return(FALSE)
  if(!collinear(C, D, B)) return(FALSE)
  
  return(is_on_line(C, D, A) && is_on_line(C, D, B))
}

# Crée un vecteur entre 2 points
vect <- function(A, B) {
  return(c(B[1] - A[1], B[2] - A[2]))
}

# Produit vectoriel 2D
cross <- function(v1, v2) {
  return((v1[1] * v2[2]) - (v1[2] * v2[1]))
}

# Vérifie si trois points sont alignés
collinear <- function(A, B, C) {
  v_AB <- vect(A, B)
  v_AC <- vect(A, C)
  ABxAC <- cross(v_AB, v_AC)
  if(is.na(ABxAC == 0)) stop("Collinear")
  return(ABxAC == 0)
}

# Si le point C est dans le segment [AB]
is_on_line <- function(A, B, C) {
  v_AB <- vect(A, B)
  v_AC <- vect(A, C)
  
  k_AC <- v_AB[1]*v_AC[1] + v_AB[2]*v_AC[2]
  k_AB <- v_AB[1]*v_AB[1] + v_AB[2]*v_AB[2]
  
  if (k_AC < 0) return(FALSE)
  if (k_AC > k_AB) return(FALSE)
  
  return(TRUE)
}

# Vérifier l'égalité à epsilon près
epsilon_equal <- function(a, b, epsilon) {
  return((b >= a - epsilon) && (b <= a + epsilon))
}

# Vérifier f(a,b) à epsilon près
epsilon_check <- function(f, a, b, epsilon) {
  return(f(a, b)
         || f(a + epsilon, b)
         || (if (a-epsilon >= 0) f(a - epsilon, b) else FALSE)
         || f(a, b + epsilon)
         || (if (b-epsilon >= 0) f(a, b - epsilon) else FALSE))
}
