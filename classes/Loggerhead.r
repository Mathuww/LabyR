library(R6)
library(TurtleGraphics)
library(jsonlite)

Loggerhead <- R6Class("Loggerhead",
  class = TRUE,
 
  public = list(
    layers = NULL,
    activeLayer = NULL,
    delta = NULL,
    flow_rate = NULL,
    printer = NULL,
    printer_data = NULL,
    fil_radius = NULL,
    # Valeurs "génériques" (si l'imprimante n'existe pas dans printers.json)
    nozzle_diam = 0.2,
    print_speed = 300,
    travel_speed = 9000,

    initialize = function(printer, flow_rate = 1, delta = 0.2) {
      self$layers <- list()
      
      self$printer <- printer
      self$printer_data = read_json(paste0(cheminRelatif, "printers.json"))[[printer]]
      if (is.null(self$printer_data)) {
        print("WARNING : Data needed to generate GCode start/end sequences for your printer does not exist (check printers.json)")
      } else {
        self$nozzle_diam = 0.1
        self$print_speed = as.numeric(self$printer_data$print_speed)
        self$travel_speed = as.numeric(self$printer_data$travel_speed)
        self$fil_radius = as.numeric(self$printer_data$fil_radius)
      }
      
      self$flow_rate <- flow_rate
      self$delta <- delta
      
      turtle_init() # Initialise turtle graphics
      turtle_hide()
    },

    # Méthode publique pour ajouter un nouveau calque
    addLayer = function() {
      layer <- Path$new()
      self$layers <- append(self$layers, list(layer))
      layerIndex <- length(self$layers)
      self$selectLayer(layerIndex)
    },

    # Méthode publique pour changer le calque actif
    selectLayer = function(layerIndex) {
      if (layerIndex < 1 || layerIndex > length(self$layers)) {
        stop("Layer index out of bounds")
      } else {
        self$activeLayer <- layerIndex
      }
    },

    # Méthode publique pour ajouter des polygones au calque
    buildShapes = function(polygons) {
      if(is.null(self$activeLayer)) stop("No active layer selected")
      
      layer <- self$layers[[self$activeLayer]]
      
      for (polygon in polygons) {
        if (!inherits(polygon, "Polygon")) {
          stop("All shapes must be of class 'Polygon'")
        } else {
          polygonPath <- polygon$toPrintPath(self$activeLayer)
          layer <- Path$new()$fusion(layer, polygonPath)
        }
      }
      
      self$layers[[self$activeLayer]] <- layer
    },

    # Méthode publique pour tracer librement
    freeDraw = function(moves) {
      if(!self$activeLayer) stop("No layer registered")
      # free draw
      self$layers[[self$activeLayer]] <- moves
    },
    
    # Méthode publique
    # Affiche la couche actuelle
    # (Appelle juste display() de la couche)
    display = function() {
      self$layers[[self$activeLayer]]$display()
    },
    
    # Méthode publique
    # Pour générer le GCode correspondant à toutes les couches
    genFile = function(filename = paste0(cheminRelatif, "out.gcode")) {
      # En-tête spécifique à l'imprimante
      gcode_str <- paste("; START SEQUENCE (in printers.json)\n", self$printer_data$start_code, sep='')

      for (i in 1:length(self$layers)) {
        # Coordonnée Z absolue (i-1 fois la hauteur d'une couche, 
        # la première couche étant à Z = 0)
        z <- i*self$delta
        gcode_str <- paste(gcode_str, sprintf("\n; LAYER %d\n", i), sep = '')
  
        movs <- self$layers[[i]]$movements
        
        if (length(movs) > 0) {
          curr_p <- movs[[1]] 
          # Aller vers le premier point de la couche !
          # J'ignore la possiblilité où le premier point de la couche aurait FILL
          # car ça n'a aucune utilité d'extruder en montant simplement sur Z (je pense ?)
          curr_str <- sprintf("G0 X%f Y%f Z%f F%f E0.000\n", curr_p[1], curr_p[2], z, self$travel_speed/6)
          gcode_str <- paste(gcode_str, curr_str, sep='')
          
          for (j in 1:(length(movs) - 1)) {
            next_p <- movs[[j+1]]
            # Mention FILL
            if (next_p[3] != 0) {
              ## Toute cette partie sert à calculer le taux d'extrusion pour un vecteur donné
              # Première étape : savoir la longueur de la ligne qu'on trace
              curr_line_l <- sqrt((next_p[1] - curr_p[1])**2 + (next_p[2] - curr_p[2])**2)
              #cat('CURR P\n');print(curr_p); cat('NEXT P\n');print(next_p)
              # Volume de fil à extruder 
              extr_v <- self$delta * self$flow_rate * self$nozzle_diam * curr_line_l
              # On divise par la surface du filament (qui dépend de son rayon)
              # et on obtient la longueur
              extr_l <- extr_v / (pi * (self$fil_radius)**2)
              # Commande G1 : Mouvement de travail (avec extrusion)
              curr_str <- sprintf("G1 X%f Y%f Z%f F%f E%f\n", next_p[1], next_p[2], z, self$print_speed, extr_l)
            } else {
              # Si FILL = 0 : mouvement rapide G0 (mouvement rapide sans extrusion)
              curr_str <- sprintf("G0 X%f Y%f Z%f F%f E0.000\n", next_p[1], next_p[2], z, self$travel_speed)
            }
            gcode_str <- paste(gcode_str, curr_str, sep='')
            curr_p <- next_p
          }
          gcode_str <- paste(gcode_str, "\n")
        }
      }
      # Code de fin spécifique à l'imprimante
      gcode_str <- paste(gcode_str, "\n\n; END SEQUENCE\n", sep='')
      gcode_str <- paste(gcode_str, self$printer_data$end_code, sep='')
      write(gcode_str, filename)
    }
    
  )
)