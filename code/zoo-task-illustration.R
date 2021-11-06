if (!requireNamespace("here")) install.packages("here")
source(here::here("code", "zoo-illustration-utilities.R"))
# create study procedure and transition matrix plots:
system2("make", args = "study-procedure")
system2("make", args = "transition-matrix")
system2("make", args = "graph-structure")
path_graphproc = Sys.glob(here::here("output", "study_procedure", "*graph_procedure.png"))
stopifnot(length(path_graphproc) == 1)
path_transmat = Sys.glob(here::here("output", "transition_matrix", "*transition_matrix.png"))
stopifnot(length(path_transmat) == 1)
path_graphstruct = Sys.glob(here::here("output", "graph_structure", "*graph_structure.png"))
stopifnot(length(path_graphstruct) == 1)
path_output = here::here("output", "task_illustration")
dir.create(path_output, showWarnings = FALSE)

fig_graphproc = make_figure(path_graphproc)
fig_transmat = make_figure(path_transmat)
fig_graphstruct = make_figure(path_graphstruct)

fig_task = fig_graphstruct / fig_transmat / (fig_graphproc + plot_spacer()) +
  patchwork::plot_layout(nrow = 3) + 
  patchwork::plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = "bold")) &
  theme(plot.margin = unit(c(2, 2, 2, 2), "pt"))

save_figure(plot = fig_task, filename = "task_procedure",
            path = path_output, width = 8, height = 10)
    