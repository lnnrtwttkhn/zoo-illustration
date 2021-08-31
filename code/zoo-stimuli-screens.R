if (!requireNamespace("pacman")) install.packages("pacman")
packages_cran <- c("here", "magick", "cowplot", "ggplot2")
pacman::p_load(char = packages_cran)
path_output_screens = here::here("output", "screens")
path_output_gif = here::here("output", "animation")
dir.create(path_output_screens, showWarnings = FALSE)
dir.create(path_output_gif, showWarnings = FALSE)
path_input_files = Sys.glob(here::here("input", "stimuli", "*png"))

plot_rectangle = ggplot() +
  geom_rect(aes(xmin = 1, xmax = 4, ymin = 0, ymax = 1), fill = "white", color = "black") +
  theme(panel.grid = element_blank()) +
  theme(panel.border = element_blank()) +
  theme(panel.background = element_blank()) +
  theme(plot.background = element_blank()) +
  theme(axis.line = element_blank()) +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.title = element_blank()) +
  theme(legend.position = "none")

save_screen = function(plot, file_name) {
  path_out = file.path(path_output_screens, paste0(file_name, "_screen.png"))
  ggsave(plot = plot,
         filename = path_out,
         device = "png",
         dpi = "retina",
         width = 4,
         height = 3,
         scale = 1)
  return(path_out)
}

paths_out = c()
for (path_file in path_input_files) {
  file_name = base::strsplit(base::basename(path_file), "\\.")[[1]][1]
  image = magick::image_read(path_file)
  image_info <- magick::image_info(image)
  image_height = image_info$height
  image_width = image_info$width
  # TODO: this is just a lazy way to adjust the size to horizontal / vertical images:
  if (image_height > image_width) {
    scale = 1.3
  } else if (image_height < image_width) {
    scale = 2
  }
  plot_screen = plot_rectangle +
    cowplot::draw_image(path_file, x = 2.5, y = 0.5, hjust = 0.5, vjust = 0.5, scale = scale)
  path_out = save_screen(plot = plot_screen, file_name = file_name)
  paths_out = c(paths_out, path_out)
}

plot_fixation = plot_rectangle +
  geom_text(aes(label = "+", x = 2.5, y = 0.5), size = 10)
path_fixation = save_screen(plot = plot_fixation, file_name = "fixation")

# make gif
delay_stimuli = 80
delay_iti_short = 75
delay_iti_long = 1000

list_stimuli = paste("-delay", delay_stimuli, paths_out[1:6])
list_fixation_short = rep(paste("-delay", delay_iti_short, path_fixation), 5)
list_fixation_long = rep(paste("-delay", delay_iti_long, path_fixation), 1)
list_fixation = c(list_fixation_short, list_fixation_long)
image_command = paste(c(rbind(list_stimuli, list_fixation)), collapse = " ")

file_out_gif = "zoo_main_long_inter_trial_interval.gif"
gif_arguments = paste(
  c(image_command, "-loop 0", file.path(path_output_gif, file_out_gif)),
  collapse = " ")
system2(command = "convert", args = gif_arguments, stderr = TRUE, stdout = TRUE)
system2(command = "identify", args = paste('-format "%f %T\n"', file.path(path_output_gif, file_out_gif)))
