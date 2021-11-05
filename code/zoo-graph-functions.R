deg2rad = function(degree){
  radian = 2 * pi * degree / 360
  return(radian)
}

get_images = function(path_files){
  images_out = c()
  for (path in path_files) {
    image = magick::image_read(path)
    image_raster = as.raster(image)
    images_out = c(images_out, list(image_raster))
  }
  return(images_out)
}

coord_circle_point = function(radius, angle_radian) {
  # get coordinates on a circle based on the angle and the radius
  x = radius * sin(angle_radian)
  y = radius * cos(angle_radian)
  output = list(x = x, y = y)
  return(output)
}

add_badges = function(fig, images_raster, dt_nodes) {
  fig = fig + 
    annotation_raster(raster = images_raster[[1]],
                      xmin = dt_nodes[1]$xmin,
                      xmax = dt_nodes[1]$xmax,
                      ymin = dt_nodes[1]$ymin,
                      ymax = dt_nodes[1]$ymax) +
    annotation_raster(raster = images_raster[[2]],
                      xmin = dt_nodes[2]$xmin,
                      xmax = dt_nodes[2]$xmax,
                      ymin = dt_nodes[2]$ymin,
                      ymax = dt_nodes[2]$ymax) +
    annotation_raster(raster = images_raster[[3]],
                      xmin = dt_nodes[3]$xmin,
                      xmax = dt_nodes[3]$xmax,
                      ymin = dt_nodes[3]$ymin,
                      ymax = dt_nodes[3]$ymax) +
    annotation_raster(raster = images_raster[[4]],
                      xmin = dt_nodes[4]$xmin,
                      xmax = dt_nodes[4]$xmax,
                      ymin = dt_nodes[4]$ymin,
                      ymax = dt_nodes[4]$ymax) +
    annotation_raster(raster = images_raster[[5]],
                      xmin = dt_nodes[5]$xmin,
                      xmax = dt_nodes[5]$xmax,
                      ymin = dt_nodes[5]$ymin,
                      ymax = dt_nodes[5]$ymax) +
    annotation_raster(raster = images_raster[[6]],
                      xmin = dt_nodes[6]$xmin,
                      xmax = dt_nodes[6]$xmax,
                      ymin = dt_nodes[6]$ymin,
                      ymax = dt_nodes[6]$ymax)
  return(fig)
}
