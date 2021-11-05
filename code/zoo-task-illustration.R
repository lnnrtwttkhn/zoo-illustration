if (!requireNamespace("here")) install.packages("here")
source(here::here("code", "zoo-illustration-utilities.R"))
# create study procedure and transition matrix plots:
system2("make", args = "study-procedure")
system2("make", args = "transition-matrix")
path_graphproc = Sys.glob(here::here("output", "study_procedure", "*graph_procedure.png"))
stopifnot(length(path_graphproc) == 1)
path_transmat = Sys.glob(here::here("output", "transition_matrix", "*transition_matrix.png"))
stopifnot(length(path_transmat) == 1)
path_output = here::here("output", "task_illustration")
dir.create(path_output, showWarnings = FALSE)

fig_graphproc = make_figure(path_graphproc)
fig_transmat = make_figure(path_transmat)

fig_task = fig_transmat / fig_graphproc +
  patchwork::plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = "bold"))

save_figure(plot = fig_task, filename = "task_procedure",
            path = path_output, width = 6, height = 6)