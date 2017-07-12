locateGlobe <- function(coord, angle.y = 0, angle.x =0, c =0, size=3, alpha=0.9, color='orange'){
  #' Plot location or extent on the globe.
  #'
  #' @description Plot to locate an area on the globe in polar coordinates. 
  #'
  #' @template xyParam
  #' @param coord a data frame with a x and y column (for points) or extent (for extent)
  #' @param angle.x the x viewing angle
  #' @param angle.y the y viewing angle
  #' @param c the z viewing angle
  #' @param size a numeric indicating the size of the point (for locations)
  #' @param alpha a numeric indicating the level of transparency
  #' @param color the color code for the location
  #'
  #' @examples
  #' df.point <- data.frame(x=30,y=30)
  #' locateGlobe(df.point, 40, 30, c =0, size=10, alpha=0.9)
  #'
  #' @return a ggplot2 object
  require(rworldmap)
  require(dplyr)
  require(ggplot2)
  require(geosphere)
  
  # World map
  worldMap <- getMap()
  world.points <- fortify(worldMap)
  world.points$region <- world.points$id
  
  world.df <- world.points[,c("long","lat","group", "region")]
  
  
  r.world.co <- raster(extent(c(-180, 180, -90, 90)),1000)
  r.world.co[]<- 0
  x <- sampleRegular(r.world.co, size=1000, asRaster = TRUE)
  dat <- as.data.frame(x, xy=TRUE)
  
  theme_opts <- list(theme(panel.grid.minor = element_blank(),
                           panel.grid.major = element_blank(),
                           panel.background = element_blank(),
                           plot.background = element_rect(fill = "transparent",colour = NA),
                           panel.border = element_blank(),
                           axis.line = element_blank(),
                           axis.text.x = element_blank(),
                           axis.text.y = element_blank(),
                           axis.ticks = element_blank(),
                           axis.title.x = element_blank(),
                           axis.title.y = element_blank(),
                           plot.title = element_text(size=22)))
  
  if( is.data.frame(coord)){
    df.point <- coord
    worldmap <- ggplot() + 
      geom_tile(data = na.omit(dat),aes(x, y),fill="#EBF4FA")+
      geom_polygon(data = world.df, aes(x = long, y = lat, group = group)) +
      geom_point(data=df.point,aes(x=x, y=y),color=color,alpha=alpha,size=size)+
      scale_y_continuous(breaks = (-2:2) * 30) +
      scale_x_continuous(breaks = (-4:4) * 45) +
      theme_opts+
      coord_map("ortho", orientation=c(angle.y, angle.x, 0))
    worldmap
  } else if (is(coord) == 'Extent') {
    p <- as(coord, 'SpatialPolygons') 
    p_df<- fortify(p)
    worldmap <- ggplot() + 
      geom_tile(data = na.omit(dat),aes(x, y),fill="#EBF4FA")+
      geom_polygon(data = world.df, aes(x = long, y = lat, group = group)) +
      geom_polygon(data=p_df,aes(x=long, y=lat),color=color,fill=color,alpha=alpha)+
      scale_y_continuous(breaks = (-2:2) * 30) +
      scale_x_continuous(breaks = (-4:4) * 45) +
      theme_opts+
      coord_map("ortho", orientation=c(angle.y, angle.x, 0))
    worldmap
  }
  return(worldmap)
}