if (!requireNamespace("here")) install.packages("here")
source(here::here("code", "zoo-illustration-utilities.R"))
path_illustration = here::here("output", "illustration")
path_output = here::here("output", "task_procedure")
dir.create(path_output, showWarnings = FALSE)

fig_response = make_figure(Sys.glob(file.path(path_illustration, "*response_buttons.png")))
fig_training = make_figure(Sys.glob(file.path(path_illustration, "*task*training*.png")))
fig_recall = make_figure(Sys.glob(file.path(path_illustration, "*task*recall*.png")))
fig_graph = make_figure(Sys.glob(file.path(path_illustration, "*task*graph*.png")))
fig_task = fig_response / fig_training / fig_recall +
  patchwork::plot_layout(widths = c(1, 1, 1)) +
  patchwork::plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = "bold"))

save_figure(plot = fig_task, filename = "task_procedure",
            path = path_output, width = 6, height = 6)