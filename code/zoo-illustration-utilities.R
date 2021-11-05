if (!requireNamespace("pacman")) install.packages("pacman")
packages_cran <- c(
  "here", "tidyverse", "data.table", "assertr", "viridis", "patchwork", "lemon",
  "cowplot"
)
pacman::p_load(char = packages_cran)

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
  fig = ggdraw() +
    draw_image(path)
  return(fig)
}
