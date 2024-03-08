if (!requireNamespace("here")) install.packages("here")
source(here::here("code", "zoo-illustration-utilities.R"))
# create study procedure and transition matrix plots:
system2("make", args = "study-procedure")
system2("make", args = "transition-matrix")
system2("make", args = "graph-structure")
system2("make", args = "hypotheses")

path_task_single <- Sys.glob(here::here("output", "illustration", "*task*single*.png"))
stopifnot(length(path_task_single) == 1)
path_task_sequence <- Sys.glob(here::here("output", "illustration", "*task*sequence*.png"))
stopifnot(length(path_task_sequence) == 1)
path_graphproc = Sys.glob(here::here("output", "study_procedure", "*graph_procedure.png"))
stopifnot(length(path_graphproc) == 1)
path_transmat = Sys.glob(here::here("output", "transition_matrix", "*transition_matrix.png"))
stopifnot(length(path_transmat) == 1)
path_graphstruct = Sys.glob(here::here("output", "graph_structure", "*graph_structure.png"))
stopifnot(length(path_graphstruct) == 1)
path_graphtrial = Sys.glob(here::here("output", "illustration", "*task*sequence*.png"))
stopifnot(length(path_graphtrial) == 1)
path_hypotheses = Sys.glob(here::here("output", "hypotheses", "*hypotheses.png"))
stopifnot(length(path_hypotheses) == 1)
path_output = here::here("output", "graph_procedure")
dir.create(path_output, showWarnings = FALSE)

fig_task_single = make_figure(path_task_single)
fig_task_sequence = make_figure(path_task_sequence)
fig_graphproc = make_figure(path_graphproc)
fig_transmat = make_figure(path_transmat)
fig_graphstruct = make_figure(path_graphstruct)
fig_graphtrial = make_figure(path_graphtrial)
fig_hypotheses = make_figure(path_hypotheses)

fig_task = plot_grid(
    plot_grid(fig_task_single, fig_task_sequence,
              nrow = 1, ncol = 2, rel_widths = c(0.55, 0.45), labels = c("a", "b")),
    plot_grid(fig_graphstruct, labels = c("c"),
              nrow = 1, ncol = 1),
    plot_grid(fig_transmat, labels = c("d"),
              nrow = 1, ncol = 1),
    plot_grid(fig_graphproc, fig_hypotheses, labels = c("e", "f"),
              nrow = 1, ncol = 2, vjust = -0.1),
    nrow = 4, ncol = 1
  ) +
  theme(plot.margin = unit(c(0, 0, 0, 0), "pt"))

save_figure(plot = fig_task, filename = "graph_procedure",
            path = path_output, width = 8, height = 11.5)
    