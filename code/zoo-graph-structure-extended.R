if (!requireNamespace("here")) install.packages("here")
source(here::here("code", "zoo-illustration-utilities.R"))
path_badges = here::here("output", "badges", "*.png")
path_output = here::here("output", "graph_structure")
source(here::here("code", "zoo-graph-functions.R"))
dir.create(path_output, showWarnings = FALSE)
files_badges = Sys.glob(path_badges)
num_nodes = 6
path_files = files_badges[1:num_nodes]
images_raster = get_images(path_files = path_files)
angles_degree_nodes = head(seq(0, 360, by = 60))

cfg = list()
cfg$circle_radius = 2
cfg$badge_radius = 2.7
cfg$badge_size = 1
cfg$letter_radius = 3.7
colors_probabilities = load_config()$probability_colors

dt_nodes = data.table(
  node_number = seq(1, num_nodes),
  node_letter = LETTERS[seq(1, num_nodes)],
  angle = angles_degree_nodes,
  x_node_center = coord_circle_point(radius = cfg$circle_radius, angle_radian = deg2rad(angles_degree_nodes))$x,
  y_node_center = coord_circle_point(radius = cfg$circle_radius, angle_radian = deg2rad(angles_degree_nodes))$y,
  x_badge_center = coord_circle_point(radius = cfg$badge_radius, angle_radian = deg2rad(angles_degree_nodes))$x,
  y_badge_center = coord_circle_point(radius = cfg$badge_radius, angle_radian = deg2rad(angles_degree_nodes))$y,
  x_letter_center = coord_circle_point(radius = cfg$letter_radius, angle_radian = deg2rad(angles_degree_nodes))$x,
  y_letter_center = coord_circle_point(radius = cfg$letter_radius, angle_radian = deg2rad(angles_degree_nodes))$y
) %>%
  .[, ":="(
    badge_xmin = x_badge_center - cfg$badge_size * 0.5,
    badge_xmax = x_badge_center + cfg$badge_size * 0.5,
    badge_ymin = y_badge_center - cfg$badge_size * 0.5,
    badge_ymax = y_badge_center + cfg$badge_size * 0.5
  )]

dt_lines = dt_nodes %>%
  .[, c("node_number", "angle", "x_node_center", "y_node_center")] %>%
  .[rep(seq_len(.N), num_nodes), ] %>%
  setorder(node_number) %>%
  .[, xend := rep(dt_nodes$x_node_center, num_nodes)] %>%
  .[, yend := rep(dt_nodes$y_node_center, num_nodes)] %>%
  .[, node_number_next := rep(dt_nodes$node_number, num_nodes)] %>%
  # remove transitions to the same node and nearest neighbor:
  .[!(abs(node_number_next - node_number) %in% c(0)), ] %>%
  .[, ":="(probability = ifelse(abs(node_number_next - node_number) %in% c(1), "0.7", "0.1"))] %>%
  .[, by = .(node_number), node_next_index := seq(1, .N)] %>%
  .[, node_distance := node_number_next - node_number] %>%
  .[, curvature := -0.27] %>%
  setDT(.)

dt_lines_uni = dt_lines %>%
  .[node_distance %in% c(1, -5), ]

dt_nodes_single = dt_nodes %>%
  .[node_number == 1, ]
dt_lines_single = dt_lines %>%
  .[node_number == 1, ] %>%
  .[, curvature := ifelse(node_number == 1 & node_number_next == 6, 0.27, -0.27)]

cfg$arrow_curved_color = "black"
cfg$arrow_ends = "both"
cfg$circle_radius = 2
cfg$circle_linetype = "solid"
cfg$circle_color = "black"
cfg$edges_color = "black"
cfg$edges_linetype = "solid"

figure = ggplot()
figure = draw_circle(figure, cfg)
figure = draw_letters(figure, dt_nodes)
figure = draw_edges(figure, dt_lines %>% .[!(abs(node_distance) %in% c(1, 5)), ], cfg)
figure = draw_badges(figure, images_raster, dt_nodes)
figure = figure + ggtitle("Graph") + theme(plot.title = element_text(hjust = 0.5))
#figure = draw_curved_arrows(figure, dt_lines %>% .[node_distance %in% c(1, -5)], cfg)
figure_circle = figure

cfg$arrow_curved_color = colors_probabilities[4]
cfg$arrow_straight_color = colors_probabilities[2]
cfg$arrow_ends = "last"
cfg$highlight_color = "black"
cfg$highlight_radius = 0.55
cfg$circle_linetype = "dotted"
cfg$circle_color = "gray"
cfg$edges_color = "gray"
cfg$edges_linetype = "dotted"

figure = ggplot()
figure = draw_circle(figure, cfg)
figure = draw_letters(figure, dt_nodes)
figure = draw_edges(figure, dt_lines %>% .[!(abs(node_distance) %in% c(1, 5)), ], cfg)
figure = draw_badge_highlight(figure, dt_nodes_single, cfg)
figure = draw_badges(figure, images_raster, dt_nodes)
figure = draw_straight_arrows(figure, dt_lines_single %>% .[!(node_distance %in% c(1, 5))], cfg)
figure = draw_curved_arrows(figure, dt_lines_single %>% .[node_distance == 1], cfg)
figure = figure + ggtitle("Unidirectional") + theme(plot.title = element_text(hjust = 0.5))
figure_uni = figure

cfg$arrow_curved_color = colors_probabilities[3]
cfg$arrow_straight_color = colors_probabilities[2]
figure = ggplot()
figure = draw_circle(figure, cfg)
figure = draw_letters(figure, dt_nodes)
figure = draw_edges(figure, dt_lines %>% .[!(abs(node_distance) %in% c(1, 5)), ], cfg)
figure = draw_badge_highlight(figure, dt_nodes_single, cfg)
figure = draw_badges(figure, images_raster, dt_nodes)
figure = draw_straight_arrows(figure, dt_lines_single %>% .[!(node_distance %in% c(1, 5))], cfg)
figure = draw_curved_arrows(figure, dt_lines_single %>% .[node_distance %in% c(1, 5)], cfg)
figure = figure + ggtitle("Bidirectional") + theme(plot.title = element_text(hjust = 0.5))
figure_bi = figure

figure_graphs = figure_circle + figure_uni + figure_bi +
  # patchwork::plot_annotation(tag_levels = c('1'), tag_prefix = '[', tag_suffix = ']') &
  # theme(plot.tag = element_text(face = "bold")) & 
  theme(plot.margin = unit(c(0, 0, 0, 0), "pt"))

save_figure(plot = figure_graphs, filename = "graph_structure",
            path = path_output, width = 8, height = 3.5)
 