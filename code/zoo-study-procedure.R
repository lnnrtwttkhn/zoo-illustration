if (!requireNamespace("pacman")) install.packages("pacman")
packages_cran <- c("here", "tidyverse", "data.table", "assertr",
                   "viridis", "patchwork", "lemon")
pacman::p_load(char = packages_cran)
path_output = here::here("output", "study_procedure")
dir.create(path_output, showWarnings = FALSE)

duration_s1 = c(2, 2, 4, 2, 2, 5, 2, 2, rep(6, 8), 5, 3)
events_s1 = c("Localizer", "T1-Orientation", "T1-MPRAGE", "MB4-Orientation",
           "Shimming", "Rest Run 1", "Instructions", "Training",
           paste(rep("Recall Run", 8), seq(8)),
           "Rest Run 2", "Fieldmaps")
t_start_s1 = c(0, head(cumsum(duration_s1), -1))
t_stop_s1 = cumsum(duration_s1)
session1 = data.table("events" = events_s1, "duration" = duration_s1,
                      "t_start" = t_start_s1,  "t_stop" = t_stop_s1,
                      "session" = rep("Session 1", length(events_s1)))


duration_s2 = c(2, 2, 4, 2, 2, 6, rep(c(3, 10), 5), 3, 3)
events_s2 = c("Localizer", "T1-Orientation", "T1-MPRAGE", "MB4-Orientation",
              "Shimming", "Recall Run 9",
              paste(rep(c("Rest Run", "Main Run"), 5), rep(seq(5), each = 2)),
              "Rest Run 6", "Fieldmaps")
t_start_s2 = c(0, head(cumsum(duration_s2), -1))
t_stop_s2 = cumsum(duration_s2)
session2 = data.table("events" = events_s2, "duration" = duration_s2,
                      "t_start" = t_start_s2,  "t_stop" = t_stop_s2,
                      "session" = rep("Session 2", length(events_s2)))

gap = 1
sessions = rbind(session1, session2) %>%
  verify(t_stop - t_start == duration) %>%
  .[, by = .(session), t_start_gap := t_start + (gap * seq(0, .N - 1))] %>%
  .[, by = .(session), t_stop_gap := t_stop + (gap * seq(0, .N - 1))] %>%
  .[, by = .(session), label_breaks := t_start_gap + duration / 2] %>%
  .[, by = .(session), task := ifelse(stringr::str_detect(events, "Recall"), "Task: Recall", "Eyes closed")] %>%
  .[, by = .(session), task := ifelse(stringr::str_detect(events, "Training"), "Task: Training", task)] %>%
  .[, by = .(session), task := ifelse(stringr::str_detect(events, "Instructions"), "Task: Instructions", task)] %>%
  .[, by = .(session), task := ifelse(stringr::str_detect(events, "Main"), "Task: Main", task)] %>%
  .[, by = .(session), task := ifelse(stringr::str_detect(events, "Rest"), "Rest (Fixation)", task)] %>%
  .[, task := as.factor(factor(task, levels = c("Task: Instructions", "Task: Training", "Task: Recall", "Task: Main", "Rest (Fixation)", "Eyes closed")))]

plot_session = function(df) {
  ggplot(data = df,
         aes(xmin = t_start_gap, xmax = t_stop_gap, ymin = 0, ymax = 1))  + 
    geom_rect(aes(fill = task), color = "black") +
    geom_text(aes(x = label_breaks, y = 0.5, label = duration)) +
    #facet_grid(rows = vars(session), scales = "free", switch = "y") +
    scale_y_continuous(name = "Duration\n(in min.)") +
    scale_x_continuous(
      breaks = df$label_breaks, labels = df$events, name = "Event",
      sec.axis = dup_axis(breaks = df$t_stop_gap, labels = df$t_stop_gap,
                          name = "Time in the MRI scanner (in min.)")) +
    coord_capped_cart(top = "right") +
    scale_fill_viridis_d(name = "Participants' task", drop = FALSE,
                         direction = 1, option = "viridis") +
    theme(axis.text.x.bottom = element_text(angle = 45, hjust = 1)) +
    theme(axis.text.x.top = element_text(angle = 45, hjust = 0.5, vjust = 0.5)) +
    theme(panel.grid.major = element_blank()) +
    theme(panel.grid.minor = element_blank()) +
    theme(panel.background = element_blank()) +
    theme(axis.line.y = element_blank()) +
    theme(axis.ticks = element_line(color = "white")) +
    theme(axis.title = element_text(color = "black")) +
    theme(axis.text.y = element_blank()) +
    theme(axis.text.x = element_text(color = "black")) +
    theme(axis.line.x.top = element_line(color = "black")) +
    theme(axis.ticks.x.top = element_line(color = "black")) +
    theme(plot.title = element_text(hjust = 0.5))
}

plot_s1 = plot_session(sessions %>% .[session == "Session 1"]) +
  ggtitle("Session 1")
plot_s2 = plot_session(sessions %>% .[session == "Session 2"]) +
  ggtitle("Session 2")
plot_s1_nolegend = plot_s1 + theme(legend.position = "none")
study_procedure = (plot_s1_nolegend  / plot_s2) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a") &
  theme(text = element_text("Helvetica"))
study_procedure

ggsave(file.path(path_output, "zoo_study_procedure_session1.pdf"),
       device = "pdf", dpi = "retina",
       plot_s1, width = 8, height = 3)
ggsave(file.path(path_output, "zoo_study_procedure_session1.png"),
       device = "png", dpi = "retina",
       plot_s1, width = 8, height = 3)

ggsave(file.path(path_output, "zoo_study_procedure_session2.pdf"),
       device = "pdf", dpi = "retina",
       plot_s2, width = 8, height = 3)
ggsave(file.path(path_output, "zoo_study_procedure_session2.png"),
       device = "png", dpi = "retina",
       plot_s2, width = 8, height = 3)

ggsave(file.path(path_output, "zoo_study_procedure.pdf"),
       device = "pdf", dpi = "retina",
       study_procedure, width = 8, height = 6)
ggsave(file.path(path_output, "zoo_study_procedure.png"),
       device = "png", dpi = "retina",
       study_procedure, width = 8, height = 6)

session2_main = session2 %>%
  .[stringr::str_detect(events, "Main")] %>%
  verify(t_stop - t_start == duration)

session2_main = session2_main %>%
  setDT(.) %>%
  setorder(events) %>%
  .[, by = .(session), t_start_gap := t_start + (gap * seq(0, .N - 1))] %>%
  .[, by = .(session), t_stop_gap := t_stop + (gap * seq(0, .N - 1))] %>%
  .[, by = .(session), label_breaks := t_start_gap + duration / 2] %>%
  .[, by = .(session), task := ifelse(stringr::str_detect(events, "Run 1"), "Graph 1", "Graph 2")] %>%
  .[, by = .(session), task := ifelse(stringr::str_detect(events, "Run 2"), "Graph 1", task)] %>%
  .[, by = .(session), task := ifelse(stringr::str_detect(events, "Run 3"), "Graph 1 & 2", task)] %>%
  .[, by = .(session), task := ifelse(stringr::str_detect(events, "Run 4"), "Graph 2", task)] %>%
  .[, by = .(session), task := ifelse(stringr::str_detect(events, "Run 4"), "Graph 2", task)] %>%
  .[, task := as.factor(factor(task, levels = c("Graph 1", "Graph 1 & 2", "Graph 2")))]

plot_main = function(df) {
  ggplot(data = df,
         aes(xmin = t_start_gap, xmax = t_stop_gap, ymin = 0, ymax = 1))  + 
    geom_rect(aes(fill = task), color = "black") +
    geom_text(aes(x = label_breaks, y = 0.5, label = duration)) +
    #facet_grid(rows = vars(session), scales = "free", switch = "y") +
    scale_y_continuous(name = "Duration\n(in min.)") +
    scale_x_continuous(
      breaks = df$label_breaks, labels = df$events, name = "Event",
      sec.axis = dup_axis(breaks = df$t_stop_gap, labels = df$t_stop_gap,
                          name = "Time in the MRI scanner (in min.)")) +
    coord_capped_cart(top = "right") +
    scale_fill_discrete(name = "Graph structure", drop = FALSE) +
    theme(axis.text.x.bottom = element_text(angle = 45, hjust = 1)) +
    theme(axis.text.x.top = element_text(angle = 45, hjust = 0.5, vjust = 0.5)) +
    theme(panel.grid.major = element_blank()) +
    theme(panel.grid.minor = element_blank()) +
    theme(panel.background = element_blank()) +
    theme(axis.line.y = element_blank()) +
    theme(axis.ticks = element_line(color = "white")) +
    theme(axis.title = element_text(color = "black")) +
    theme(axis.text.y = element_blank()) +
    theme(axis.text.x = element_text(color = "black")) +
    theme(axis.line.x.top = element_line(color = "black")) +
    theme(axis.ticks.x.top = element_line(color = "black")) +
    theme(plot.title = element_text(hjust = 0.5))
}

plot_graph_procedure = plot_main(session2_main %>% .[session == "Session 2"]) +
  ggtitle("Session 2 - Graph learning")
plot_graph_procedure

ggsave(file.path(path_output, "zoo_graph_procedure.pdf"),
       device = "pdf", dpi = "retina",
       plot_graph_procedure, width = 5, height = 3)
ggsave(file.path(path_output, "zoo_graph_procedure.png"),
       device = "png", dpi = "retina",
       plot_graph_procedure, width = 5, height = 3)

