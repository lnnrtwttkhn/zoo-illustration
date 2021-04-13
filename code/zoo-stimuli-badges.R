if (!requireNamespace("pacman")) install.packages("pacman")
packages_cran <- c("here", "magick")
pacman::p_load(char = packages_cran)
path_output = here::here("output", "badges")
dir.create(path_output, showWarnings = FALSE)
path_input_files = Sys.glob(here::here("stimuli", "*png"))

for (path_file in path_input_files) {
  file_name = base::strsplit(base::basename(path_file), "\\.")[[1]][1]
  image = magick::image_read(path_file)
  image_info <- magick::image_info(image)
  image_height = image_info$height
  image_width = image_info$width
  image_size_diff = abs(image_height - image_width)
  image_size_max <- max(c(image_height, image_width))
  circle_border_size = 10
  background_square = magick::image_blank(
    width = image_size_max + circle_border_size,
    height = image_size_max + circle_border_size,
    color = "white")
  if (image_height > image_width) {
    xoffset = (image_size_diff + circle_border_size) / 2
    yoffset = (0 + circle_border_size) / 2
  } else if (image_height < image_width) {
    xoffset = (0 + circle_border_size) / 2
    yoffset = (image_size_diff + circle_border_size) / 2
  }
  offset = paste0("+", xoffset, "+", yoffset)
  image_square = image_composite(
    image = background_square,
    composite_image = image,
    offset = offset)
  # create one circle figure for the inner circle:
  fig1 <- magick::image_draw(
    image = image_blank(
      width = image_size_max,
      height = image_size_max
      )
    )
  circle1 = symbols(
    x = image_size_max / 2,
    y = image_size_max / 2,
    circles = image_size_max / 2,
    bg = "black",
    fg = "black",
    inches = FALSE,
    add = TRUE)
  dev.off()
  # create a second circle for the outer circle:
  fig2 <- magick::image_draw(
    image = image_blank(
      width = image_size_max + circle_border_size,
      height = image_size_max + circle_border_size
      )
    )
  circle2 = symbols(
    x = (image_size_max + circle_border_size) / 2,
    y = (image_size_max + circle_border_size) / 2,
    circles = (image_size_max + circle_border_size) / 2,
    bg = "black",
    fg = "black",
    inches = FALSE,
    add = TRUE)
  dev.off()
  # combine the square image with the inner circle:
  image_circle = magick::image_composite(
    image = image_square,
    composite_image = fig1,
    operator = "copyopacity",
    gravity = "center")
  # set the background of the circle image to transparent:
  image_circle = magick::image_background(
    image = image_circle, "transparent")
  # add the circle image on top of the large black circle:
  image_circle = magick::image_composite(
    image = fig2,
    composite_image = image_circle,
    operator = "atop",
    gravity = "center")
  image_circle
  path_out = file.path(path_output, paste0(file_name, "_badge.png"))
  magick::image_write(image_circle, path = path_out, format = "png")
}

