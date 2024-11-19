#### REMPLISSAGE VERSION TORTUE ###

library(TurtleGraphics)

Line <- function(x1,y1, x2, y2) {
  turtle_setpos(x1,y1)
  turtle_goto(x2,y2)
}

v_line = function(posx, posy, x1, y1, x2, y2) { 
  tan_a <- (y2-y1) / (x2-x1)
  Line(posx, posy, posx, tan_a*(posx-x1)+y1)
}

follow = function(add, x1, y1, x2, y2) {
  if (add > 0) {if (x1+add > x2) {Line(x1, y1, x2, y2) ; return(x1+add-x2)}} # Si on dépasse le segment
  else if (add < 0) {if (x1+add < x2) {Line(x1, y1, x2, y2) ; return(x1+add-x2)}} # idem
  
  tan_a <- (y2-y1) / (x2-x1)
  Line(x1, y1, add+x1, tan_a*add + y1)
  return(0)
}

sum_p = function(c) { # A vérifier rigoureusement
  return(c[1]*1000 + c[2] + c[3]*0.001 + c[3]*0.000001)
}

contour = function(sommets) {
  turtle_setpos(sommets[[1]][1], sommets[[1]][2])
  pos <- c(sommets[[1]][1], sommets[[1]][2])
  for (s in sommets[-1]) {
    Line(pos[1], pos[2], s[1], s[2])
    pos <- turtle_getpos()
  }
  Line(pos[1], pos[2], sommets[[1]][1], sommets[[1]][2])
}

remplissage = function(sommets) {
  
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
          tan_a <- (i[4]-i[2]) / (i[3]-i[1])
          seg[2] <- tan_a*(fin-seg[1]) + seg[2]
          seg[1] <- fin
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
    turtle_setpos(c[1], c[2])
    pos <- turtle_getpos()
    up <- TRUE
    while (TRUE) {
      if (up) {
        if (follow(step_value, pos[1], pos[2], c[3], c[4]) != 0) {
          break
        }
        pos <- turtle_getpos()
        v_line(pos[1], pos[2], c[5], c[6], c[7], c[8])
      } else {
        if (follow(step_value, pos[1], pos[2], c[7], c[8]) != 0) {
          break
        }
        pos <- turtle_getpos()
        v_line(pos[1], pos[2], c[1], c[2], c[3], c[4])
      }
      up <- !up
      pos <- turtle_getpos()
    }
  }
}


l <- list( c(100,200), c(300, 400), c(600, 500), c(700,300), c(500, 100))
turtle_init(1000,1000)
contour(l)
turtle_hide()
remplissage(l)
