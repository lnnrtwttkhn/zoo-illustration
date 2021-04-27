if (!requireNamespace("pacman")) install.packages("pacman")
packages_cran <- c("here", "tidyverse", "data.table", "magic", "patchwork", "ggforce")
#"ggforce", "grid", "png" cowplot
pacman::p_load(char = packages_cran)
path_badges = here::here("output", "badges", "*.png")
path_output = here::here("output", "graph_structure")
dir.create(path_output, showWarnings = FALSE)
files_badges = Sys.glob(path_badges)

radius = 2
dat = data.table(
  x = c(0),
  y = c(0),
  size = c(radius)
)

path_files = files_badges[1:6]
get_images = function(path_files){
  images_out = c()
  for (path in path_files) {
    image = magick::image_read(path)
    image_raster = as.raster(image)
    images_out = c(images_out, list(image_raster))
  }
  return(images_out)
}

images_raster = get_images(path_files=path_files)

deg2rad = function(degree){
  radian = 2 * pi * degree / 360
  return(radian)
}

coord_circle_point = function(radius, angle_radian) {
  x = radius * sin(angle_radian)
  y = radius * cos(angle_radian)
  output = list(x = x, y = y)
  return(output)
}

badge_size = 1
angles_degree = head(seq(0, 360, by = 60))
angles_radians = deg2rad(angles_degree)

dt_badge = data.table(
  badge = seq(1, 6),
  angle = angles_degree,
  x = coord_circle_point(radius = radius, angle_radian = angles_radians)$x,
  y = coord_circle_point(radius = radius, angle_radian = angles_radians)$y
) %>%
  .[, ":="(
    xmin = x - badge_size * 0.5,
    xmax = x + badge_size * 0.5,
    ymin = y - badge_size * 0.5,
    ymax = y + badge_size * 0.5
  )]

dt_lines = dt_badge %>%
  .[,c("badge", "x", "y")] %>%
  .[rep(seq_len(.N), 6), ] %>%
  setorder(badge) %>%
  .[, xend := rep(unique(x), 6)] %>%
  .[, yend := rep(unique(y), 6)] %>%
  .[, badge_next := rep(unique(badge), 6)] %>%
  # remove duplication of nearest neighbor:
  .[!(abs(badge_next - badge) %in% c(1, 5)),] %>%
  .[, ":="(probability = ifelse(
    abs(badge_next - badge) %in% c(1), "0.7", "0.1")
  )]
  
  
  
ggplot(dat, aes(x0 = x, y0 = y)) + 
  geom_segment(data = dt_lines, mapping = aes(
    x = x, y = y, xend = xend, yend = yend, color = probability)) +
  geom_circle(aes(r = size)) +
  coord_equal() +
  xlim(c(-3, 3)) +
  ylim(c(-3, 3)) +
  theme_classic() +
  annotation_raster(raster = images_raster[[1]],
                    xmin = dt_badge[1]$xmin,
                    xmax = dt_badge[1]$xmax,
                    ymin = dt_badge[1]$ymin,
                    ymax = dt_badge[1]$ymax) +
  annotation_raster(raster = images_raster[[2]],
                    xmin = dt_badge[2]$xmin,
                    xmax = dt_badge[2]$xmax,
                    ymin = dt_badge[2]$ymin,
                    ymax = dt_badge[2]$ymax) +
  annotation_raster(raster = images_raster[[3]],
                    xmin = dt_badge[3]$xmin,
                    xmax = dt_badge[3]$xmax,
                    ymin = dt_badge[3]$ymin,
                    ymax = dt_badge[3]$ymax) +
  annotation_raster(raster = images_raster[[4]],
                    xmin = dt_badge[4]$xmin,
                    xmax = dt_badge[4]$xmax,
                    ymin = dt_badge[4]$ymin,
                    ymax = dt_badge[4]$ymax) +
  annotation_raster(raster = images_raster[[5]],
                    xmin = dt_badge[5]$xmin,
                    xmax = dt_badge[5]$xmax,
                    ymin = dt_badge[5]$ymin,
                    ymax = dt_badge[5]$ymax) +
  annotation_raster(raster = images_raster[[6]],
                    xmin = dt_badge[6]$xmin,
                    xmax = dt_badge[6]$xmax,
                    ymin = dt_badge[6]$ymin,
                    ymax = dt_badge[6]$ymax)

ggsave(file.path(path_output, "zoo_graph_structure.pdf"),
       device = "pdf", dpi = "retina",
       last_plot(), width = 4, height = 4)
ggsave(file.path(path_output, "zoo_graph_structure.png"),
       device = "png", dpi = "retina",
       last_plot(), width = 4, height = 4)