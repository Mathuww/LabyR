library(TurtleGraphics)

cheminRelatif <- ""

source(paste0(cheminRelatif, 'classes/Loggerhead.r'))
source(paste0(cheminRelatif, 'classes/Path.r'))
source(paste0(cheminRelatif, 'classes/Polygon.r'))


turtle <- Loggerhead$new("Ultimaker_S3") # 0.5 = rayon filament 


segments <- list()


epsilon_check <- function(a, b, epsilon = 1) {
  return((b >= a - epsilon) && (b <= a + epsilon))
}

seg_epsilon_check <- function(A, B, C, D) {
  return(epsilon_check(A[1], C[1])
         && epsilon_check(A[2], C[2])
         && epsilon_check(B[1], D[1])
         && epsilon_check(B[2], D[2]))
}

f <- function(n, len, path) {
  if (n==0) {
    curr_pos_x <- path$x
    curr_pos_y <- path$y
    next_pos_x <- path$x + len*path$facing[[1]]
    next_pos_y <- path$y + len*path$facing[[2]]
    #print(path$facing)
    seg_exist <- FALSE
    for (s in segments) {
      #print(s)
      seg_exist <- seg_exist || seg_epsilon_check(
        c(curr_pos_x, curr_pos_y),
        c(next_pos_x, next_pos_y),
        c(s[[1]], s[[2]]),
        c(s[[3]], s[[4]])
      )
    }
    if (!seg_exist) {
      next_seg <- c(curr_pos_x, curr_pos_y,
                    next_pos_x, next_pos_y)
      print(next_seg)
      segments <<- append(segments, list(c(next_seg[[1]],next_seg[[2]],next_seg[[3]],next_seg[[4]])))
      #print(segments)
      path$move(next_pos_x, next_pos_y)
    } else {
      print("SAME SEGMENT !!!!!")
      path$move(next_pos_x, next_pos_y, fill=0.0)
    }
  } else {
    f(n-1, len, path)
    f(n-1, len, path)
  }
}

x <- function(n, len, path) {
  if (n>0) {
    x(n-1,len, path)
    f(n-1,len, path)
    path$turn(120)
    x(n-1,len, path)
    f(n-1,len, path)
    path$turn(120)
    x(n-1,len, path)
    f(n-1,len, path)
    path$turn(120)
  }
}

Serpinsky <- function(n, len) {
  path <- Path$new()
  x(n, len, path)
  return(path)
}

serp_path <- Serpinsky(3, 10)

turtle <- Loggerhead$new("Ultimaker S3")
turtle$addLayer()
turtle$freeDraw(serp_path)
turtle$display()
