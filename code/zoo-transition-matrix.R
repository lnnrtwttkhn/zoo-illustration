if (!requireNamespace("here")) install.packages("here")
source(here::here("code", "zoo-illustration-utilities.R"))
path_badges = here::here("output", "badges", "*.png")
path_output = here::here("output", "transition_matrix")
dir.create(path_output, showWarnings = FALSE)
files_badges = Sys.glob(path_badges)

states = c("A", "B", "C", "D", "E", "F")
num_states = length(states)
probabilities_uni = c(0, 0.7, 0.1, 0.1, 0.1, 0)
df_uni = data.table(
  t0 = rep(states, each = num_states),
  t1 = rep(states, num_states),
  probability = unlist(lapply(seq(0, 5), function(x) magic::shift(probabilities_uni, i = x)))
) %>%
  .[, "probability" := as.factor(probability)]

probabilities_bi = c(0, 0.35, 0.1, 0.1, 0.1, 0.35)
df_bi = data.table(
  t0 = rep(states, each = num_states),
  t1 = rep(states, num_states),
  probability = unlist(lapply(seq(0, 5), function(x) magic::shift(probabilities_bi, i = x)))
) %>%
  .[, "probability" := as.factor(probability)]


plot_trans_mat = function(df, title_name, colors){
  
  transition_matrix = ggplot(data = df, aes(x = t0, y = t1)) +
    geom_tile(aes(fill = probability)) +
    theme(axis.ticks = element_blank()) +
    theme(axis.text = element_text(size = 16)) +
    xlab("Stimulus at t - 1") +
    ylab("Stimulus at trial t") +
    scale_fill_manual(values = colors, name = "Transition\nprobability") +
    scale_y_discrete(limits = rev) +
    theme(panel.grid.major = element_blank()) +
    theme(panel.grid.minor = element_blank()) +
    theme(panel.background = element_blank()) +
    theme(axis.line = element_blank()) +
    theme(axis.text = element_text(color = "black")) +
    theme(axis.title = element_text(color = "black")) +
    ggtitle(title_name) +
    theme(plot.title = element_text(hjust = 0.5))
  
  pimage_y <- axis_canvas(transition_matrix, axis = 'y') + 
    draw_image(files_badges[6], y = 0.5, scale = 0.9) +
    draw_image(files_badges[5], y = 1.5, scale = 0.9) +
    draw_image(files_badges[4], y = 2.5, scale = 0.9) +
    draw_image(files_badges[3], y = 3.5, scale = 0.9) +
    draw_image(files_badges[2], y = 4.5, scale = 0.9) +
    draw_image(files_badges[1], y = 5.5, scale = 0.9)
  pimage_x <- axis_canvas(transition_matrix, axis = 'x') + 
    draw_image(files_badges[1], x = 0.5, scale = 0.9) +
    draw_image(files_badges[2], x = 1.5, scale = 0.9) +
    draw_image(files_badges[3], x = 2.5, scale = 0.9) +
    draw_image(files_badges[4], x = 3.5, scale = 0.9) +
    draw_image(files_badges[5], x = 4.5, scale = 0.9) +
    draw_image(files_badges[6], x = 5.5, scale = 0.9)
  plot_update_y = insert_yaxis_grob(transition_matrix, pimage_y, position = "left")
  plot_update_xy = insert_xaxis_grob(plot_update_y, pimage_x, position = "bottom")
  bla = ggdraw(plot_update_xy)
  return(bla)
  
}

fig_trans_mat_uni = plot_trans_mat(
  df = df_uni,
  title_name = "Transition matrix\nUnidirectional graph",
  colors = cfg$probability_colors[c(1, 2, 4)]) &
  guides(fill = "none") +
  theme(plot.margin = unit(c(0, 0, 0, 0), "pt"))
fig_trans_mat_bi = plot_trans_mat(
  df = df_bi,
  title_name = "Transition matrix\nBidirectional graph",
  colors = cfg$probability_colors[c(1, 2, 3)])
fig_trans_mat_both = fig_trans_mat_uni + fig_trans_mat_bi +
  plot_layout(guides = "collect") +
  theme(plot.margin = unit(c(0, 0, 0, 0), "pt")) &
  # plot_annotation(tag_levels = c('1'), tag_prefix = '[', tag_suffix = ']') &
  theme(legend.position = "bottom", legend.box = "horizontal")
ggsave(file.path(path_output, "zoo_graphs_transition_matrix.pdf"),
       device = cairo_pdf(), dpi = "retina",
       fig_trans_mat_both, width = 8, height = 3.5)
ggsave(file.path(path_output, "zoo_graphs_transition_matrix.png"),
       device = "png", dpi = "retina",
       fig_trans_mat_both, width = 8, height = 3.5)




