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
badge_size = 1
radius = 2
node_size = 20
angles_degree_nodes = head(seq(0, 360, by = 60))
angles_degree_side1 = head(seq(0, 360, by = 60)) - 15
angles_degree_side2 = head(seq(0, 360, by = 60)) + 15

dt_nodes = data.table(
  node_number = seq(1, num_nodes),
  node_letter = LETTERS[seq(1, num_nodes)],
  angle = angles_degree_nodes,
  x_node_center = coord_circle_point(radius = radius, angle_radian = deg2rad(angles_degree_nodes))$x,
  y_node_center = coord_circle_point(radius = radius, angle_radian = deg2rad(angles_degree_nodes))$y,
  x_node_inner = coord_circle_point(radius = radius, angle_radian = deg2rad(angles_degree_nodes))$x,
  y_node_inner = coord_circle_point(radius = radius, angle_radian = deg2rad(angles_degree_nodes))$y,
  x_node_side1 = coord_circle_point(radius = radius, angle_radian = deg2rad(angles_degree_side1))$x,
  y_node_side1 = coord_circle_point(radius = radius, angle_radian = deg2rad(angles_degree_side1))$y,
  x_node_side2 = coord_circle_point(radius = radius, angle_radian = deg2rad(angles_degree_side2))$x,
  y_node_side2 = coord_circle_point(radius = radius, angle_radian = deg2rad(angles_degree_side2))$y
) %>%
  .[, ":="(
    xmin = x_node_center - badge_size * 0.5,
    xmax = x_node_center + badge_size * 0.5,
    ymin = y_node_center - badge_size * 0.5,
    ymax = y_node_center + badge_size * 0.5
  )] %>%
  .[, ":="(
    arrow_x_side1 = shift(x_node_side1),
    arrow_y_side1 = shift(y_node_side1),
    arrow_x_side2 = shift(x_node_side2),
    arrow_y_side2 = shift(y_node_side2)
  )]


la = data.table(
  y = coord_circle_point(radius = 2, angle_radian = deg2rad(angles_degree_nodes))$y - coord_circle_point(radius = 0.5, angle_radian = deg2rad(angles_degree_nodes))$y,
  x = coord_circle_point(radius = 2, angle_radian = deg2rad(angles_degree_nodes))$x - coord_circle_point(radius = 0.5, angle_radian = deg2rad(angles_degree_nodes))$x
)

na = data.table(
  y = coord_circle_point(radius = 2, angle_radian = deg2rad(angles_degree_nodes))$y - coord_circle_point(radius = 0.5, angle_radian = deg2rad(head(seq(0, 360, by = 60)) + 30))$y,
  x = coord_circle_point(radius = 2, angle_radian = deg2rad(angles_degree_nodes))$x - coord_circle_point(radius = 0.5, angle_radian = deg2rad(head(seq(0, 360, by = 60)) + 30))$x
)

fa = data.table(
  y = coord_circle_point(radius = 2, angle_radian = deg2rad(angles_degree_nodes))$y - coord_circle_point(radius = 0.5, angle_radian = deg2rad(head(seq(0, 360, by = 60)) - 30))$y,
  x = coord_circle_point(radius = 2, angle_radian = deg2rad(angles_degree_nodes))$x - coord_circle_point(radius = 0.5, angle_radian = deg2rad(head(seq(0, 360, by = 60)) - 30))$x
)

dt_lines = dt_nodes %>%
  .[, c("node_number", "angle", "x_node_center", "y_node_center")] %>%
  .[rep(seq_len(.N), num_nodes), ] %>%
  setorder(node_number) %>%
  .[, xend := rep(dt_nodes$x_node_center, num_nodes)] %>%
  .[, yend := rep(dt_nodes$y_node_center, num_nodes)] %>%
  .[, node_number_next := rep(dt_nodes$node_number, num_nodes)] %>%
  # remove transitions to the same node and nearest neighbor:
  .[!(abs(node_number_next - node_number) %in% c(0, 1, 5)), ] %>%
  .[, by = .(node_number, node_number_next),
    node_combination := paste(sort(c(node_number, node_number_next)), collapse = "")] %>%
  dplyr::distinct(., node_combination, .keep_all = TRUE) %>%
  setDT(.) %>%
  .[, ":="(probability = ifelse(abs(node_number_next - node_number) %in% c(1), "0.7", "0.1"))] %>%
  .[, by = .(node_number), node_next_index := seq(1, .N)]

fig_graph = ggplot() +
  geom_circle(aes(x0 = 0, y0 = 0, r = 2), fill = "red") +
  geom_circle(data = dt_nodes, aes(x0 = x_node_center, y0 = y_node_center, r = 0.5), fill = "blue") +
  geom_segment(data = dt_lines, mapping = aes(
    x = x_node_center, y = y_node_center, xend = xend, yend = yend, color = probability),
    color = "black", size = 0.5, inherit.aes = FALSE,
    arrow = arrow(length = unit(0.03, "npc"), ends = "both", type = "closed")) +
  geom_text(data = dt_nodes, aes(x = x_node_center, y = y_node_center, label = node_letter),
            size = 12, color = "white", inherit.aes = FALSE) +
  geom_point(data = la, aes(x = x, y = y), color = "green") +
  geom_point(data = na, aes(x = x, y = y), color = "orange") +
  geom_point(data = fa, aes(x = x, y = y), color = "yellow") +
  geom_curve(
    data = dt_nodes,
    aes(x = x_node_side1, xend = arrow_x_side2,
        y = y_node_side1, yend = arrow_y_side2),
    arrow = arrow(length = unit(0.03, "npc"), ends = "first", type = "closed"),
    curvature = 0.15, # TODO: how to calculate the curvature
    angle = 90,
    color = "black",
    linetype = "solid",
    size = 1
  ) +
  theme(panel.border = element_blank()) +
  theme(panel.grid.major = element_blank()) +
  theme(panel.grid.minor = element_blank()) +
  coord_equal()

save_figure(plot = fig_graph , filename = "graph_structure",
            path = path_output, width = 4, height = 4)