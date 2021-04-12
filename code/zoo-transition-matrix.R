if (!requireNamespace("pacman")) install.packages("pacman")
packages_cran <- c("here", "magick")
pacman::p_load(char = packages_cran)
path_input = here::here("stimuli", "*png")
path_output = here::here("output")
path_files = Sys.glob(path_input)

for (path_file in path_files) {
  file_name = strsplit(base::basename(path_file), "\\.")[[1]][1]
  image = magick::image_read(path_file)
  image_info <- magick::image_info(image)
  image_height = image_info$height
  image_width = image_info$width
  image_size_diff = abs(image_height - image_width)
  image_size_max <- max(c(image_height, image_width))
  background_square = magick::image_blank(image_size_max, image_size_max, "white")
  if (image_height > image_width) {
    offset = paste0("+", image_size_diff / 2, "+", 0)
  } else if (image_height < image_width) {
    offset = paste0("+", 0, "+", image_size_diff / 2)
  }
  image_square = image_composite(background_square, image, offset = offset)
  fig <- magick::image_draw(image_blank(image_size_max, image_size_max))
  circle = symbols(
    x = image_size_max / 2,
    y = image_size_max / 2,
    circles = image_size_max / 2,
    bg = "black",
    fg = "black",
    inches = FALSE,
    add = TRUE)
  dev.off()
  image_circle = magick::image_composite(
    image_square, fig, operator = "copyopacity", gravity = "center")
  image_circle = magick::image_background(image_circle, "transparent")
  path_out = here::here("output", paste0(file_name, "_badge.png"))
  magick::image_write(image_circle, path = path_out, format = "png")
}

