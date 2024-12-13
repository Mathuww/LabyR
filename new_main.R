library(TurtleGraphics)
source("classes/Loggerhead.r")
source("classes/Path.r")
source("classes/Polygon.r")


draw_irregular_polygon <- function(path, num_sides, max_length, max_angle) {
  # Initialiser l'angle total pour assurer la fermeture
  total_angle <- 0
  
  for (i in 1:num_sides) {
    # Longueur aléatoire pour chaque côté
    side_length <- runif(1, 20, max_length)
    path$forward(side_length)
    
    # Angle aléatoire pour chaque tournant
    angle <- runif(1, 10, max_angle)
    path$turn(angle)
    
    # Ajouter l'angle au total
    total_angle <- total_angle + angle
  }
  # Pour fermer le polygone, ajuster le dernier angle
  # Le dernier angle doit annuler la somme des rotations précédentes
  closing_angle <- -total_angle %% 360
  path$turn(closing_angle)  # Tourner pour fermer le polygone
}

turtle <- Loggerhead$new("Ultimaker_S3") # 0.5 = rayon filament 

path1 <- Path$new()
path2 <- Path$new()


# Premier polygone irrégulier (par exemple, 7 côtés)
draw_irregular_polygon(path1, num_sides = 7, max_length = 80, max_angle = 180)
path1$move(0,0)

# Deuxième polygone irrégulier (par exemple, 8 côtés), légèrement déplacé pour superposition
draw_irregular_polygon(path2, num_sides = 8, max_length = 60, max_angle = 160)
path2$move(0,0)
poly1 <- Polygon$new(path1)
poly2 <- Polygon$new(path1, origin=c(10,10))

new_poly <- poly1$merge(poly2)




turtle$buildShapes(list(
  new_poly
))

# Afficher le dernier calque
turtle$display()
#turtle$genFile()
