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
        self$nozzle_diam = self$printer_data$extruder$diameterNozzle
        self$print_speed = self$printer_data$speed$printing
        self$travel_speed = self$printer_data$speed$travel
        self$fil_radius = (self$printer_data$fil_diam)/2
      }
      
      self$flow_rate <- flow_rate
      self$delta <- delta
      
      turtle_init() # Initialise turtle graphics
      turtle_hide()
    },
    
    #Méthode publique pour personnaliser le début du gcode, spécialement les commentaires pour l'imprimante
    commentStart = function (volume) {
      target <- paste0(";TARGET_MACHINE.NAME:", self$printer_data$printerName, "\n")
      
      #personalisation des commentaires du début du gcode (toutes les descriptions approfondies sont dans la documentation GCODE.md)
      base <- ";START_OF_HEADER\n"
      headerVersion <- paste0(";HEADER_VERSION:", self$printer_data$headerVersion, "\n")
      flavor <- paste0(";FLAVOR:", self$printer_data$flavor, "\n")
      
      #Les commentaires destinés au générateur comme Cura
      if (self$printer_data$"generator"[["dateInstallation"]] == "") {
        generator <- paste0(";GENERATOR.NAME:", self$printer_data$generator$name, "\n", ";GENERATOR.VERSION:", self$printer_data$generator$version, "\n", ";GENERATOR.BUILD_DATE:", Sys.Date(), "\n")
      } else {
        generator <- paste0(";GENERATOR.NAME:", self$printer_data$generator$name, "\n", ";GENERATOR.VERSION:", self$printer_data$generator$version, "\n", ";GENERATOR.BUILD_DATE:", self$printer_data$"generator"[["dateInstallation"]], "\n")
      }
      extruder <- ""
      nbBuse <- length(self$printer_data$extruder)
      #Les commmentaires destinés à chaque buse
      for (i in 1:nbBuse) {
        numBuse <- as.character(i-1)
        buse <- self$printer_data$"extruder"[[numBuse]]
        extruderTemp <- paste0(";EXTRUDER_TRAIN.", numBuse, ".INITIAL_TEMPERATURE:", buse$temperature, "\n")
        extruderMaterialVolume <- paste0(";EXTRUDER_TRAIN.", numBuse, ".MATERIAL.VOLUME_USED:", volume, "\n")
        extruderMaterialGUID <- paste0(";EXTRUDER_TRAIN.", numBuse, ".MATERIAL.GUID:", buse$materialGUID, "\n")
        extruderNozzleDiameter <- paste0(";EXTRUDER_TRAIN.", numBuse, ".NOZZLE.DIAMETER:", as.character(buse$diameterNozzle), "\n")
        extruderNozzleName <- paste0(";EXTRUDER_TRAIN.", numBuse, ".NOZZLE.NAME:", as.character(buse$nameNozzle), "\n")
        extruder <- paste0(extruder, extruderTemp, extruderMaterialVolume, extruderMaterialGUID, extruderNozzleDiameter,extruderNozzleName)
      }
       #Les commentaires destinés aux différentes température extérieur et du plateau
      build <- paste0(";BUILD_PLATE.INITIAL_TEMPERATURE:", as.character(self$printer_data$temperature_build$plate), "\n", ";BUILD_VOLUME.TEMPERATURE:", as.character(self$printer_data$temperature_build$volume) ,"\n")
      
      #Les commentaires destinés au dimension de l'impression, qui est pour l'instant la dimension de l'imprimante
      dimensionMin <- paste0(";PRINT.SIZE.MIN.X:", "0", "\n", ";PRINT.SIZE.MIN.Y:", "0", "\n", ";PRINT.SIZE.MIN.Z:", "0", "\n" )
      dimensionMax <- paste0(";PRINT.SIZE.MAX.X:", as.character(self$printer_data$dimensionMax$x), "\n", ";PRINT.SIZE.MAX.Y:", as.character(self$printer_data$dimensionMax$y), "\n", ";PRINT.SIZE.MAX.Z:", as.character(self$printer_data$dimensionMax$z), "\n" )
      
      endComments <- paste0(";END_OF_HEADER\n;Generated with ", self$printer_data$generator$name, " ", self$printer_data$generator$version, "\n")
      
      comments <- paste0(base, headerVersion, flavor, generator, target, extruder, build, dimensionMin, dimensionMax, endComments)
      return(comments)
    },

    #Méthode publique pour personaliser le gcode
    commandStart = function ()
    {
      nbBuse <- length(self$printer_data$extruder)
      
      if (nbBuse == 1) {
        base <- "T0\n"
      } else {
        base <- ""
      }
      checkupPosition <- paste0(base, "G90 ; absolute pos\nM83 ; relative extrusion\n")
      
      bed <- paste0("M190 S", as.character(self$printer_data$temperature_build$plate)," ; heat bed\n")
      
      nozzle <- ""
      for (i in 1:nbBuse) {
        numBuse <- as.character(i-1)
        buse <- self$printer_data$"extruder"[[numBuse]]
        nozzle <- paste0(nozzle, "M109 S", buse$temperature, " T", numBuse," ; heat nozzle\n")
      }
      checkupZ <- "G29 ; leveling\nM420 S1 \nM500 ; mesh leveling\nG0 Z20.001\n"
      
      fan <- paste0("M106 S", as.character(self$printer_data$speed$fan)," ; fan full speed\n")
      generalAcceleration <- paste0("M204 S", as.character(self$printer_data$speed$generalAcceleration)," ; general acceleration\n")
      
      checkupFinal <- "\nM205 X30 Y30 ; start position\nG92 E0 ; extr = 0\nG1 F200 E6 ; 6 mm of extra to compensate end sequence retraction\n"
        
      command <- paste0(checkupPosition, bed, nozzle, checkupZ, fan, generalAcceleration, checkupFinal)
      return(command)
    },
    
    #Méthode publique pour personaliser le gcode
    commandStart = function ()
    {
      nbBuse <- length(self$printer_data$extruder)
      
      if (nbBuse == 1) {
        base <- "T0\n"
      } else {
        base <- ""
      }
      checkupPosition <- paste0(base, "G90 ; absolute pos\nM83 ; relative extrusion\n")
      
      bed <- paste0("M190 S", as.character(self$printer_data$temperature_build$plate)," ; heat bed\n")
      
      nozzle <- ""
      for (i in 1:nbBuse) {
        numBuse <- as.character(i-1)
        buse <- self$printer_data$"extruder"[[numBuse]]
        nozzle <- paste0(nozzle, "M109 S", buse$temperature, " T", numBuse," ; heat nozzle\n")
      }
      checkupZ <- "G29 ; leveling\nM420 S1 \nM500 ; mesh leveling\nG0 Z20.001\n"
      
      fan <- paste0("M106 S", as.character(self$printer_data$speed$fan)," ; fan full speed\n")
      generalAcceleration <- paste0("M204 S", as.character(self$printer_data$speed$generalAcceleration)," ; general acceleration\n")
      
      checkupFinal <- "\nM205 X30 Y30 ; start position\nG92 E0 ; extr = 0\nG1 F200 E6 ; 6 mm of extra to compensate end sequence retraction\n"
        
      command <- paste0(checkupPosition, bed, nozzle, checkupZ, fan, generalAcceleration, checkupFinal)
      return(command)
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
      
      merged_polygons <- c()
      for (polygon in polygons) {
        if (!inherits(polygon, "Polygon")) {
          stop("All shapes must be of class 'Polygon'")
        }
      }
      
      for (polygon in polygons) {
          polygonPath <- polygon$toPrintPath(self$activeLayer)
          layer <- Path$new()$fusion(layer, polygonPath)
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
      gcode_str <- paste("; START SEQUENCE (in printers.json)\n", self$commentStart(5), "\n", self$commandStart(), sep='')

      for (i in 1:length(self$layers)) {
        # Coordonnée Z absolue (i-1 fois la hauteur d'une couche, 
        # la première couche étant à Z = 0)
        z <- i*self$delta
        gcode_str <- paste(gcode_str, sprintf("\n; LAYER %d\n", i), sep = '')
  
        movs <- self$layers[[i]]$movements

        total_vol <- 0
        
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
              total_vol <- total_vol + (extr_v/1000)
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
      gcode_str <- paste(gcode_str, self$printer_data$end_gcode, sep='')
      write(gcode_str, filename)
    }
    
  )
)
