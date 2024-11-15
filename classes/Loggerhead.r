library(R6)
library(TurtleGraphics)

Loggerhead <- R6Class("Loggerhead",
  class = TRUE,
 
  public = list(
    layers = NULL,
    activeLayer = NULL,

    initialize = function() {
      self$layers <- list()
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
          polygonPath <- polygon$toPrintPath()
          layer <- Path$new()$fusion(layer, polygonPath)
        }
      }

      self$layers[[self$activeLayer]] <- layer
    },

    # Méthode publique pour tracer librement
    freeDraw = function(moves) {
      if(!self$activeLayer) stop("No layer registered")
      # free draw
    }
    
  )
)