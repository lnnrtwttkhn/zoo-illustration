if (!requireNamespace("pacman")) install.packages("pacman")
packages_cran <- c("here", "magick", "cowplot", "ggplot2")
pacman::p_load(char = packages_cran)
path_input = here::here("output", "badges")
path_output = here::here("output", "stimuli")
dir.create(path_output, showWarnings = FALSE)
path_input_files = Sys.glob(file.path(path_input, "*png"))

filenames = c()
for (path_file in path_input_files) {
  file_name = base::strsplit(base::basename(path_file), "\\.")[[1]][1]
  image = magick::image_read(path_file)
  plot_tmp = cowplot::ggdraw() +
    draw_image(path_file)
  assign(file_name, plot_tmp)
  filenames = c(filenames, file_name)
}
plot_overview = plot_grid(
  plotlist = mget(filenames), ncol = 4, nrow = 6) +
  theme(panel.grid = element_blank()) +
  theme(panel.border = element_blank())
ggsave(file.path(path_output, "zoo_badges_overview.png"),
       device = "png", dpi = "retina",
       plot_overview, width = 8, height = 6)
