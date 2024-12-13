displaySegments <- function(seglist) {
  turtle_init(width = 100, height = 100)
  turtle_hide()
  
  for(seg in seglist) {
    turtle_goto(seg[[1]], seg[[2]])
    turtle_down()
    turtle_goto(seg[[3]], seg[[4]])
    turtle_up()
    
    Sys.sleep(0.125)
  }
}

