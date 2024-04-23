if (!requireNamespace("here")) install.packages("here")
source(here::here("code", "zoo-illustration-utilities.R"))
path_output = here::here("output", "study_procedure")
dir.create(path_output, showWarnings = FALSE)

duration_s1 = c(2, 2, 4, 2, 2, 5, 2, 2, rep(6, 8), 5, 3)
events_s1 = c("Localizer", "T1-Orientation", "T1-MPRAGE", "MB4-Orientation",
           "Shimming", "Rest Run 1", "Instructions", "Training",
           paste(rep("Single Run", 8), seq(8)),
           "Rest Run 2", "Fieldmaps")
t_start_s1 = c(0, head(cumsum(duration_s1), -1))
t_stop_s1 = cumsum(duration_s1)
session1 = data.table("events" = events_s1, "duration" = duration_s1,
                      "t_start" = t_start_s1,  "t_stop" = t_stop_s1,
                      "session" = rep("Session 1", length(events_s1)))


duration_s2 = c(2, 2, 4, 2, 2, 6, rep(c(3, 10), 5), 3, 3)
events_s2 = c("Localizer", "T1-Orientation", "T1-MPRAGE", "MB4-Orientation",
              "Shimming", "Single Run 9",
              paste(rep(c("Rest Run", "Sequence Run"), 5), rep(seq(5), each = 2)),
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
  .[, by = .(session), task := ifelse(stringr::str_detect(events, "Single"), "Task: Single", "Eyes closed")] %>%
  .[, by = .(session), task := ifelse(stringr::str_detect(events, "Training"), "Task: Training", task)] %>%
  .[, by = .(session), task := ifelse(stringr::str_detect(events, "Instructions"), "Task: Instructions", task)] %>%
  .[, by = .(session), task := ifelse(stringr::str_detect(events, "Sequence"), "Task: Sequence", task)] %>%
  .[, by = .(session), task := ifelse(stringr::str_detect(events, "Rest"), "Rest (Fixation)", task)] %>%
  .[, task := as.factor(factor(task, levels = c("Task: Instructions", "Task: Training", "Task: Single", "Task: Sequence", "Rest (Fixation)", "Eyes closed")))]

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

save_figure(plot = plot_s1, filename = "study_procedure_session1",
            path = path_output, width = 8, height = 3)
save_figure(plot = plot_s2, filename = "study_procedure_session2",
            path = path_output, width = 8, height = 3)
save_figure(plot = study_procedure, filename = "study_procedure",
            path = path_output, width = 8, height = 6)

group_labels = c("uni - bi", "bi - uni")
session2_main = session2 %>%
  .[stringr::str_detect(events, "Sequence"), ] %>%
  verify(t_stop - t_start == duration) %>%
  rbind(., as.data.table(lapply(subset(., events == "Sequence Run 3"), rep, length(group_labels) - 1))) %>%
  setorder(events) %>%
  .[, num_events := .N] %>%
  .[rep(seq_len(nrow(.)), length(group_labels)), ] %>%
  .[, group := rep(group_labels, each = unique(.$num_events))] %>%
  .[, group := as.factor(factor(group, levels = group_labels))] %>%
  .[, by = .(group), graph_index := rep(seq(length(group_labels)), each = unique(.$num_events) / length(group_labels))] %>%
  .[, graph := unlist(mapply(function(x, y) unlist(strsplit(as.character(x), split = " - "))[y], group, graph_index))] %>%
  .[, graph := as.factor(factor(graph, levels = c("uni", "bi")))] %>%
  .[events == "Sequence Run 3", by = .(events, graph_index), ":="(
    duration = duration / 2,
    t_start = ifelse(graph_index == 1, t_start, t_start + (t_stop - t_start) / 2),
    t_stop = ifelse(graph_index == 1, t_stop - (t_stop - t_start) / 2, t_stop)
  )] %>%
  .[, by = .(group), t_start_gap := t_start + (gap * seq(0, .N - 1))] %>%
  .[, by = .(group), t_stop_gap := t_stop + (gap * seq(0, .N - 1))] %>%
  .[, by = .(group), label_breaks := t_start_gap + duration / 2] %>%
  .[, by = events, event_index := .GRP]

plot_main = function(df) {
  ggplot(data = df, aes(xmin = t_start_gap, xmax = t_stop_gap, ymin = 0, ymax = 1))  + 
    geom_rect(aes(fill = graph), color = "black", alpha = 0.7) +
    geom_text(aes(x = label_breaks, y = 0.5, label = duration)) +
    facet_grid(rows = vars(group), scales = "free", switch = "y") +
    scale_y_continuous(name = "Duration\n(in min.)") +
    scale_x_continuous(breaks = df$label_breaks, labels = df$event_index, name = "Run") +
    # scale_x_continuous(
    #   breaks = df$label_breaks, labels = df$event_index, name = "Run",
    #   sec.axis = dup_axis(breaks = df$t_stop_gap, labels = df$t_stop_gap,
    #                       name = "Time in the MRI scanner (in min.)")) +
    coord_capped_cart(top = "right") +
    # theme(axis.text.x.bottom = element_text(angle = 45, hjust = 1)) +
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
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(strip.text.y.left = element_text(angle = 0)) +
    scale_color_manual(values = cfg$graph_colors, name = "Graph") +
    scale_fill_manual(values = cfg$graph_colors, name = "Graph") +
    theme(plot.margin = unit(c(2, 2, 2, 2), "pt")) +
    theme(legend.position = "bottom", legend.box = "horizontal") +
    theme(legend.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
    theme(legend.box.margin = margin(t = -5, r = 0, b = 0, l = 0)) +
    ggtitle("Session 2 - Graph learning")
}

plot_graph_procedure = plot_main(session2_main)
save_figure(plot = plot_graph_procedure, filename = "study_graph_procedure",
            path = path_output, width = 5, height = 3)
