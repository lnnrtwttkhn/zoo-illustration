if (!requireNamespace("pacman")) install.packages("pacman")
packages_cran <- c("here", "patchwork", "cowplot", "ggplot2")
pacman::p_load(char = packages_cran)
path_illustration = here::here("output", "illustration")
path_output = here::here("output", "task_procedure")
dir.create(path_output, showWarnings = FALSE)

make_fig = function(path) {
  fig = ggdraw() +
    draw_image(path)
    # draw_figure_label(label = "Figure 1", colour = "black", fontface = "bold",
    #                   position = "top.left")
  return(fig)
}

fig_recall = make_fig(Sys.glob(file.path(path_illustration, "*task*recall*.png")))
fig_graph = make_fig(Sys.glob(file.path(path_illustration, "*task*graph*.png")))
fig_task = fig_recall / fig_graph +
  patchwork::plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = "bold"))

ggsave(file.path(path_output, "zoo_task_procedure.pdf"),
       device = "pdf", dpi = "retina",
       fig_task, width = 6, height = 4)
ggsave(file.path(path_output, "zoo_task_procedure.png"),
       device = "png", dpi = "retina",
       fig_task, width = 6, height = 4)