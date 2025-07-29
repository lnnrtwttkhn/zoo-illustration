load_packages <- function() {
  library("here")
  library("tidyverse")
  library("data.table")
  library("assertr")
  library("viridis")
  library("patchwork")
  library("lemon")
  library("cowplot")
  library("magic")
  library("ggforce")
}
load_packages()

load_config <- function() {
  cfg <- c()
  cfg$probability_colors = hcl.colors(4, "Dark Mint")
  cfg$graph_colors = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")[c(6,7)]
  return(cfg)
}
cfg = load_config()

save_figure <- function(plot, filename, path, width, height) {
  ggsave(filename = paste0("zoo_figure_", filename, ".pdf"),
         plot = plot, device = cairo_pdf, path = path,
         scale = 1, dpi = "retina", width = width, height = height)
  ggsave(filename = paste0("zoo_figure_", filename, ".png"),
         plot = plot, device = "png", path = path,
         scale = 1, dpi = "retina", width = width, height = height)
  return(plot)
}

make_figure = function(path) {
  figure = ggdraw() +
    draw_image(path) +
    theme(plot.margin = unit(c(0, 0, 0, 0), "pt"))
  return(figure)
}
