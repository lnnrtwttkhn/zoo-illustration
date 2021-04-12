if (!requireNamespace("pacman")) install.packages("pacman")
packages_cran <- c("here", "tidyverse", "ggridges", "cowplot")
pacman::p_load(char = packages_cran)
path_badges = here::here("output", "*.png")
files_badges = Sys.glob(path_badges)

states = c("A", "B", "C", "D", "E", "F")
num_states = length(states)
df <- data.frame(
  t0 = rep(states, each = num_states),
  t1 = rep(states, num_states),
  probability = c(
    sample(c(0, 0.7, 0.1, 0.1, 0.1, 0)),
    sample(c(0, 0.7, 0.1, 0.1, 0.1, 0)),
    sample(c(0, 0.7, 0.1, 0.1, 0.1, 0)),
    sample(c(0, 0.7, 0.1, 0.1, 0.1, 0)),
    sample(c(0, 0.7, 0.1, 0.1, 0.1, 0)),
    sample(c(0, 0.7, 0.1, 0.1, 0.1, 0))
  )
)


transition_matrix = ggplot(data = df, aes(x = t0, y = t1)) +
  geom_tile(aes(fill = probability)) +
  theme(axis.ticks = element_blank()) +
  theme(axis.text = element_text(size = 16)) +
  xlab("Stimulus at t") +
  ylab("Stimulus at t + 1")
  

pimage_y <- axis_canvas(transition_matrix, axis = 'y') + 
  draw_image(files_badges[1], y = 0.5) +
  draw_image(files_badges[2], y = 1.5) +
  draw_image(files_badges[3], y = 2.5) +
  draw_image(files_badges[4], y = 3.5) +
  draw_image(files_badges[5], y = 4.5) +
  draw_image(files_badges[6], y = 5.5)
pimage_x <- axis_canvas(transition_matrix, axis = 'x') + 
  draw_image(files_badges[1], x = 0.5) +
  draw_image(files_badges[2], x = 1.5) +
  draw_image(files_badges[3], x = 2.5) +
  draw_image(files_badges[4], x = 3.5) +
  draw_image(files_badges[5], x = 4.5) +
  draw_image(files_badges[6], x = 5.5)
a = insert_yaxis_grob(transition_matrix, pimage_y, position = "left")
b = insert_xaxis_grob(a, pimage_x, position = "bottom")
ggdraw(b)
