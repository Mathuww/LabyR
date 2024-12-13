source("vec_utils.R")

#Classe définissant des polygones d'après un tracé (ou des sommets) et permettant de les remplir
Polygon <- R6Class(
  "Polygon",
  class = TRUE,
  
  public = list(
    vertices = NULL,
    segments = NULL,
    fill_step = 1,
    
    # Transforme les tracés en liste des sommets du polygone
    initialize = function(path, origin = c(0,0), fill_step = 1) {
      if (!inherits(path, "Path")) {
        stop("Object passed as first parameter isn't of type path")
      }
      
      movements <- path$movements
      if (length(movements) < 3) {
        stop("No polygon can be built with less than 3 vertices")
      }
      
      first_point <- movements[[1]][1:2]
      last_point <- movements[[length(movements)]][1:2]
      
      if (!all(first_point == last_point)) {
        stop("No polygon can be built from an open path")
      }
      
      self$vertices <- lapply(movements, function(m) c(m[[1]]+origin[[1]], m[[2]]+origin[[2]]))
      
      self$fill_step <- fill_step
    },
    
    # Méthode publique pour renvoyer un tracé optimisé
    toPrintPath = function(layer) {
      vertices <- self$vertices
      path <- Path$new(vertices[[1]][1], vertices[[1]][2])
      t <- 2 # insérer taille du trait 
      
      for (i in 2:length(vertices)) {
        point <- vertices[[i]]
        path$move(point[1], point[2])
      }
      
      path$move(vertices[[1]][1], vertices[[1]][2])
      
      inside_line <- function(x1, y1, x2, y2) {
        h <- (x2-x1)**2 + (y2-y1)**2
        if (h > 2) {
          cos_a <- (x2-x1)/h
          sin_a <- (y2-y1)/h
          
          x_int1 <- cos_a*t + x1
          y_int1 <- sin_a*t + y1
          
          x_int2 <- cos_a * (h-1) + x1
          y_int2 <- sin_a * (h-1) + y1
          
          path$move(x_int1, y_int1, 0.0)
          path$move(x_int2, y_int2)
          path$move(x2, y2, 0.0)
        } else {
          path$move(x2, y2, 0.0)
        }
      }
      
      v_line = function(posx, posy, x1, y1, x2, y2) { 
        tan_a <- (y2-y1) / (x2-x1)
        inside_line(posx, posy, posx, tan_a*(posx-x1)+y1)
      }
      
      h_line = function(posx, posy, x1, y1, x2, y2) {
        tan_a <- (y2-y1) / (x2-x1)
        if (tan_a == Inf) {
          inside_line(posx, posy, x1, posy)
        } else {
          inside_line(posx, posy, (posy-y1)/tan_a + x1, posy)
        }
      }
      
      h_follow = function(add, x1, y1, x2, y2) {
        if (add > 0) {if (y1+add > y2) {path$move(x2, y2, 0.0) ; return(y1+add-y2)}} # Si on dépasse le segment
        else if (add < 0) {if (y1+add < y2) {path$move(x2, y2, 0.0) ; return(y1+add-y2)}} # idem
        
        tan_a <- (y2-y1) / (x2-x1)
        if (tan_a == Inf) {
          path$move(x1, add + y1, 0.0)
        } else {
          path$move(add/tan_a + x1, add + y1, 0.0)
        }
        return(0)
      }
      
      follow = function(add, x1, y1, x2, y2) {
        if (add > 0) {
          if (x1 + add > x2) {
            path$move(x2, y2, 0.0)
            return(x1 + add - x2)
          }
        } # Si on dépasse le segment
        else if (add < 0) {
          if (x1 + add < x2) {
            path$move(x2, y2, 0.0)
            return(x1 + add - x2)
          }
        } # idem
        
        tan_a <- (y2 - y1) / (x2 - x1)
        path$move(add + x1, tan_a * add + y1, 0.0)
        return(0)
      }
      
      sum_p = function(c) {
        # A vérifier rigoureusement
        return(c[1] * 1000 + c[2] + c[3] * 0.001 + c[3] *
                 0.000001)
      }
      
      sum_h = function(c) { # A vérifier rigoureusement
        return(c[2]*1000 + c[1] + c[4]*0.001 + c[3]*0.000001)
      }
      
      v_remplissage = function(sommets) {
        
        # Création d'une liste ordonnée contenant les segments
        segments <- list()
        for (i in 1:length(sommets)) {
          i2 <- i+1
          if (i2>length(sommets)) {
            i2 <- 1
          }
          
          if (sommets[[i2]][1] < sommets[[i]][1]) {
            segments <- append(segments, list( c(sommets[[i2]][1], sommets[[i2]][2], sommets[[i]][1], sommets[[i]][2]) ))
          } else { 
            segments <- append(segments, list( c(sommets[[i]][1], sommets[[i]][2], sommets[[i2]][1], sommets[[i2]][2]) ))
          }
        }
        segments <- segments[order(sapply(segments, sum_p))]
        
        # Création des couples de segments à relier
        couples <- list()
        while (length(segments) != 0) {
          seg <- segments[[1]]
          segments <- segments[-1]
          cat("\n", seg, " ->")
          if (length(segments) == 0) {
            cat("Erreur, segment sans couple")
          }
          
          new_segments <- list()
          for (i in segments) {
            cat("\n (",i, ") : ")
            if ( !((seg[1] >= i[3]) || (i[1]>=seg[3])) ) {
              # Les 2 segments sont compatibles
              
              debut <- seg[1]
              fin <- seg[3]
              
              if (debut > i[1]) { 
                # Le debut du 1er segment est après celui du 2ème -> raccourcit le 2ème
                new_i <- i
                tan_a <- (i[4]-i[2]) / (i[3]-i[1])
                new_i[3] <- debut
                new_i[4] <- i[2] + (debut - i[1])*tan_a
                new_segments <- append(new_segments, list(new_i))
                cat(new_i, " | ")
                i[1] <- new_i[3]
                i[2] <- new_i[4]
              } else if (debut < i[1]) { 
                # Le debut du 1er segment est avant celui du 2ème -> impossible
                cat("Cas immpossible")
              }
              if (fin > i[3]) {
                # La fin du 1er segment est après celle du 2ème -> raccourcit le 1er
                fin <- i[3]
              } else if (fin < i[3]) {
                # La fin du 1er est avant celle du 2ème -> raccourcit le 2ème
                new_i <- i
                tan_a <- (i[4]-i[2]) / (i[3]-i[1])
                new_i[1] <- fin
                new_i[2] <- i[2] + (fin - i[1])*tan_a
                new_segments <- append(new_segments, list(new_i))
                cat(new_i)
                i[3] <- new_i[1]
                i[4] <- new_i[2]
              }
              
              couples <- append(couples, list(c(seg, i)))
              if (fin == seg[3]) {
                seg <- c(0,0,0,0)
              } else {
                # On continue le parcours avec la suite du segment
                tan_a <- (seg[4]-seg[2]) / (seg[3]-seg[1])
                seg[2] <- tan_a*(fin-seg[1]) + seg[2]
                seg[1] <- fin
                cat(".  ", seg)
              }
            } else {
              new_segments <- append(new_segments, list(i))
            }
          }
          segments <- new_segments
        }
        
        # Passage tortues
        step_value <- 2 # La valeur dépend du diamètre de la buse
        for (c in couples) { 
          path$move(c[1], c[2], 0.0)
          up <- TRUE
          while (TRUE) {
            if (up) {
              if (follow(step_value, path$x, path$y, c[3], c[4]) != 0) {
                break
              }
              v_line(path$x, path$y, c[5], c[6], c[7], c[8])
            } else {
              if (follow(step_value, path$x, path$y, c[7], c[8]) != 0) {
                break
              }
              v_line(path$x, path$y, c[1], c[2], c[3], c[4])
            }
            up <- !up
          }
        }
        
        return(path)
      }
      
      h_remplissage = function(sommets) {
        
        # Création d'une liste ordonnée contenant les segments
        segments <- list()
        for (i in 1:length(sommets)) {
          i2 <- i+1
          if (i2>length(sommets)) {
            i2 <- 1
          }
          
          if (sommets[[i2]][2] > sommets[[i]][2]) {
            segments <- append(segments, list( c(sommets[[i2]][1], sommets[[i2]][2], sommets[[i]][1], sommets[[i]][2]) ))
          } else { 
            segments <- append(segments, list( c(sommets[[i]][1], sommets[[i]][2], sommets[[i2]][1], sommets[[i2]][2]) ))
          }
        }
        segments <- segments[order(sapply(segments, sum_h), decreasing = TRUE)]
        
        # Création des couples de segments à relier
        couples <- list()
        while (length(segments) != 0) {
          seg <- segments[[1]]
          segments <- segments[-1]
          cat("\n", seg, " ->")
          if (length(segments) == 0) {
            cat(" segment sans couple")
          }
          
          new_segments <- list()
          for (i in segments) {
            cat("\n (",i, ") : ")
            if ( !((seg[4] >= i[2]) || (i[4]>=seg[2])) ) {
              # Les 2 segments sont compatibles
              
              debut <- seg[2]
              fin <- seg[4]
              
              if (debut < i[2]) { 
                # Le debut du 1er segment est après celui du 2ème -> raccourcit le 2ème
                new_i <- i
                
                tan_a <- (i[4]-i[2]) / (i[3]-i[1])
                if (!(tan_a == Inf)) {
                  new_i[3] <- (debut-i[2])/tan_a + i[1]
                }
                new_i[4] <- debut
                new_segments <- append(new_segments, list(new_i))
                cat(new_i, " | ")
                i[1] <- new_i[3]
                i[2] <- new_i[4]
              } else if (debut > i[2]) { 
                # Le debut du 1er segment est avant celui du 2ème -> impossible
                cat("Cas immpossible")
              }
              if (fin < i[4]) {
                # La fin du 1er segment est après celle du 2ème -> raccourcit le 1er
                fin <- i[4]
              } else if (fin > i[4]) {
                # La fin du 1er est avant celle du 2ème -> raccourcit le 2ème
                new_i <- i
                tan_a <- (i[4]-i[2]) / (i[3]-i[1])
                if (tan_a != Inf) {
                  new_i[1] <- (fin - i[2])/tan_a +i[1]
                }
                new_i[2] <- fin
                new_segments <- append(new_segments, list(new_i))
                cat(new_i)
                i[3] <- new_i[1]
                i[4] <- new_i[2]
              }
              
              couples <- append(couples, list(c(seg, i)))
              if (fin == seg[4]) {
                seg <- c(0,0,0,0)
              } else {
                # On continue le parcours avec la suite du segment
                tan_a <- (seg[4]-seg[2]) / (seg[3]-seg[1])
                if (tan_a != Inf) {
                  seg[1] <- (fin - seg[2])/tan_a + seg[1] ########### ERREUR #########
                }
                seg[2] <- fin
                cat(".  ", seg)
              }
            } else {
              new_segments <- append(new_segments, list(i))
            }
          }
          segments <- new_segments
        }
        
        # Passage tortues
        step_value <- 2 # La valeur dépend du diamètre de la buse
        for (c in couples) {
          path$move(c[1], c[2], 0.0)
          pos <- turtle_getpos()
          up <- TRUE
          while (TRUE) {
            if (up) {
              if (h_follow(-step_value, path$x, path$y, c[3], c[4]) != 0) {
                break
              }
              h_line(path$x, path$y, c[5], c[6], c[7], c[8])
            } else {
              if (h_follow(-step_value, path$x, path$y, c[7], c[8]) != 0) {
                break
              }
              h_line(path$x, path$y, c[1], c[2], c[3], c[4])
            }
            up <- !up
          }
        }
        
        return(path)
      }
      
      if (layer %% 2 == 0) return(h_remplissage(vertices))
      else return(v_remplissage(vertices))
    },
    
    # Surcharge de la fonction d'affichage
    print = function() {
      origin <- self$vertices[[1]]
      
      cat(
        "<Polygon>",
        "\n\tVertices: ",
        length(self$vertices) - 1,
        " (+ 1)",
        "\n\tOrigin: (",
        origin[1],
        ', ',
        origin[2],
        ")",
        "\n",
        sep = ""
      )
    },
    
    # ray-casting
    inside = function(x, y, eps = 0.0001) {
      n <- length(self$vertices)
      
      f <- function(e_x, e_y) {
        intersections <- 0
        for (i in 1:n) {
          x1 <- self$vertices[[i]][1]
          y1 <- self$vertices[[i]][2]
          x2 <- self$vertices[[(i %% n) + 1]][1]
          y2 <- self$vertices[[(i %% n) + 1]][2]
          
          if ((e_y > min(y1, y2)) && (e_y <= max(y1, y2)) && (e_x <= max(x1, x2))) {
            if (x1 != x2) xinters <- (e_y - y1) * (x2 - x1) / (y2 - y1) + x1
            else xinters <- x1
            
            # coté / bords de la forme
            if (x1 == x2 || e_x <= xinters) intersections <- intersections + 1
          }
        }
        # intersections est impair ? TRUE : FALSE
        return(intersections %% 2 == 1)
      }
      
      return(f(x, y)
             || f(x + eps, y)
             || (if (x-eps >= 0) f(x - eps, y) else FALSE)
             || f(x, y + eps)
             || (if (y-eps >= 0) f(x, y - eps) else FALSE))

    },
    
    
    # méthode publique pour fusionner ce polygone
    # avec un autre polygone passé en paramètres
    # @return: (list of Polygon) 
    merge = function(polygon, eps = 0.001) {
      intersections <- private$intersections(polygon) # type@list(list<2>)
      if(is.null(intersections)) return(c(self, polygon))
      
      self$segments <- points_to_segments(self$vertices)
      polygon$segments <- points_to_segments(polygon$vertices)
      
      skeleton <- append(intersections, self$vertices)
      skeleton <- append(skeleton, polygon$vertices)
      
      segments <- list()
      n <- length(skeleton)

      for(i in 1:n) {
        for(j in 1:n) {
          if(i == j) next
          
          point1 <- skeleton[[i]]
          point2 <- skeleton[[j]]
          
          is_in_p1 <- FALSE
          for(p1_seg in self$segments) {
            is_in_p1 <- is_in_p1 || is_subline(point1, point2, p1_seg[[1]], p1_seg[[2]])
          }
          
          is_in_p2 <- FALSE
          for(p2_seg in polygon$segments) {
            is_in_p2 <- is_in_p2 || is_subline(point1, point2, p2_seg[[1]], p2_seg[[2]])
          }
          
          if(!is_in_p1 && !is_in_p2) next
          
          # Si la somme des nombres de recouvrement des 2 points
          # pour les polygones 1 et 2 est = à 4
          # Alors ce segment est à l'intérieur des deux polygones
          # et donc on ne l'inclut pas
          beta1 <- beta(point1, self, polygon)
          beta2 <- beta(point2, self, polygon)
          print(point1)
          print(beta1)
          print(point2)
          print(beta2)
          #print(beta2)
          if(beta1 + beta2 == 4) next
          

          
          is_duplicate <- FALSE
          for (s in segments) {
            is_duplicate <- is_duplicate || (identical(c(point1, point2), s))
            is_duplicate <- is_duplicate || (identical(c(point2, point1), s)) 
          }
          if(is_duplicate) next
          
          if(identical(point1, point2)) next
          
          segments <- append(segments, list(c(point1, point2)))
        }
      }
      
      source("poubelle.R")
      
      #segments <- remove_duplicates(segments)
      
      print(segments)
      print("NB SEG : ")
      print(length(segments))
      

      displaySegments(segments)
    }
  ),
  
  private = list(
    
    # méthode privée pour obtenir les coordonnées
    # des points d'intersection entre deux polygones
    intersections = function(polygon) {
      coords <- list()
      
      x1 <- self$vertices[[1]][1]
      y1 <- self$vertices[[1]][2]
      
      for(i in 2:length(self$vertices)) {
        x2 <- self$vertices[[i]][1]
        y2 <- self$vertices[[i]][2]
        
        x3 <- polygon$vertices[[1]][1]
        y3 <- polygon$vertices[[1]][2]
        for(j in 2:length(polygon$vertices)) {
          x4 <- polygon$vertices[[j]][1]
          y4 <- polygon$vertices[[j]][2]
          
          ints <- intersect(
            x1, y1, 
            x2, y2,
            x3, y3,
            x4, y4
          )
          
          if(!is.null(ints)) {
            coords <- append(coords, list(ints))
          }
          
          x3 <- x4
          y3 <- y4
        }
        
        x1 <- x2
        y1 <- y2
      }
      
      if(length(coords) == 0) return(NULL)
      return(coords)
    }
  )
)
