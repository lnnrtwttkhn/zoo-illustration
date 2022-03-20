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

draw_circle = function(figure, cfg) {
  figure = figure +
    geom_circle(
      mapping = aes(x0 = 0, y0 = 0, r = cfg$circle_radius),
      color = cfg$circle_color,
      linetype = cfg$circle_linetype,
      size = cfg$circle_size,
    ) +
    theme(panel.border = element_blank()) +
    theme(panel.background = element_blank()) +
    theme(panel.grid.major = element_blank()) +
    theme(panel.grid.minor = element_blank()) +
    theme(axis.line = element_blank()) +
    theme(axis.text = element_blank()) +
    theme(axis.title = element_blank()) +
    theme(axis.ticks = element_blank()) +
    coord_equal()
  return(figure)
}

draw_letters = function(figure, dt_nodes) {
  figure = figure + 
    geom_text(data = dt_nodes, aes(x = x_letter_center, y = y_letter_center, label = node_letter),
              size = rel(5), color = "black", inherit.aes = FALSE)
  return(figure)
}

draw_badge_highlight = function(figure, dt, cfg_guide = "none") {
  figure = figure + 
    geom_circle(data = dt, aes(
      x0 = x_badge_center, y0 = y_badge_center, r = highlight_radius,
      fill = highlight_color, color = highlight_color)) +
    scale_colour_identity("Node distance\n(uni | bi)", labels = dt$node_distance,
                          breaks = dt$highlight_color, guide = cfg_guide,
                          limits = dt$highlight_color[2:length(dt$highlight_color)]) + 
    scale_fill_identity("Node distance\n(uni | bi)", labels = dt$node_distance,
                        breaks = dt$highlight_color, guide = cfg_guide,
                        limits = dt$highlight_color[2:length(dt$highlight_color)])
  return(figure)
}

draw_node_point = function(figure, dt, cfg) {
  figure = figure + 
    geom_point(data = dt, aes(x = x_node_center, y = y_node_center), pch = 21,
      fill = "white", color = cfg$highlight_color,
      size = cfg$highlight_point_size, stroke = 1)
  return(figure)
}

draw_curved_arrows = function(figure, dt, cfg) {
  figure = figure + 
    lapply(split(dt, 1:nrow(dt)), function(dat) {
      geom_curve(data = dat, aes(
        x = x_node_center, y = y_node_center, xend = xend, yend = yend),
        arrow = arrow(length = unit(0.03, "npc"), ends = cfg$arrow_ends, type = "closed"),
        curvature = dat$curvature, angle = 90, color = cfg$arrow_curved_color, linetype = "solid",
        size = cfg$arrow_curved_size)
    })
}

draw_edges = function(figure, dt, cfg) {
  figure = figure + 
    geom_segment(data = dt, mapping = aes(
      x = x_node_center, y = y_node_center, xend = xend, yend = yend),
      size = rel(0.5), inherit.aes = FALSE, color = cfg$edges_color,
      linetype = cfg$edges_linetype)
  return(figure)
}

draw_straight_arrows = function(figure, dt, cfg) {
  figure = figure + 
    geom_segment(data = dt, aes(
      x = x_node_center, y = y_node_center, xend = xend, yend = yend),
      size = rel(1), inherit.aes = FALSE, color = cfg$arrow_straight_color,
      arrow = arrow(length = unit(0.03, "npc"), ends = "last", type = "closed"))
  return(figure)
}

draw_badges = function(figure, images_raster, dt_nodes) {
  figure = figure + 
    annotation_raster(raster = images_raster[[1]],
                      xmin = dt_nodes[1]$badge_xmin,
                      xmax = dt_nodes[1]$badge_xmax,
                      ymin = dt_nodes[1]$badge_ymin,
                      ymax = dt_nodes[1]$badge_ymax) +
    annotation_raster(raster = images_raster[[2]],
                      xmin = dt_nodes[2]$badge_xmin,
                      xmax = dt_nodes[2]$badge_xmax,
                      ymin = dt_nodes[2]$badge_ymin,
                      ymax = dt_nodes[2]$badge_ymax) +
    annotation_raster(raster = images_raster[[3]],
                      xmin = dt_nodes[3]$badge_xmin,
                      xmax = dt_nodes[3]$badge_xmax,
                      ymin = dt_nodes[3]$badge_ymin,
                      ymax = dt_nodes[3]$badge_ymax) +
    annotation_raster(raster = images_raster[[4]],
                      xmin = dt_nodes[4]$badge_xmin,
                      xmax = dt_nodes[4]$badge_xmax,
                      ymin = dt_nodes[4]$badge_ymin,
                      ymax = dt_nodes[4]$badge_ymax) +
    annotation_raster(raster = images_raster[[5]],
                      xmin = dt_nodes[5]$badge_xmin,
                      xmax = dt_nodes[5]$badge_xmax,
                      ymin = dt_nodes[5]$badge_ymin,
                      ymax = dt_nodes[5]$badge_ymax) +
    annotation_raster(raster = images_raster[[6]],
                      xmin = dt_nodes[6]$badge_xmin,
                      xmax = dt_nodes[6]$badge_xmax,
                      ymin = dt_nodes[6]$badge_ymin,
                      ymax = dt_nodes[6]$badge_ymax)
  return(figure)
}

