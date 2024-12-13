library(TurtleGraphics)

cheminRelatif <- ""

source(paste0(cheminRelatif, 'classes/Loggerhead.r'))
source(paste0(cheminRelatif, 'classes/Path.r'))



turtle <- Loggerhead$new("Ultimaker_S3") # 0.5 = rayon filament 

# Définir un carré de côté 5O
path <- Path$new()

for (i in 1:4) {
  path$forward(50)
  path$turn(90) 
}


source(paste0(cheminRelatif, 'classes/Polygon.r'))
square <- Polygon$new(path, fill_step = 1)
square$inside(50,0)
square2 <- Polygon$new(path, origin=c(3,3))
square$merge(square2)
# Ajouter un calque contenant le carré
turtle$addLayer()

turtle$buildShapes(list(
 square
))

turtle$addLayer()

turtle$buildShapes(list(
  square
))

# Afficher le dernier calque
turtle$display()
turtle$genFile()

