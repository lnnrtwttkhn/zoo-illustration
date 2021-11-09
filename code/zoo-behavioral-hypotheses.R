if (!requireNamespace("here")) install.packages("here")
source(here::here("code", "zoo-illustration-utilities.R"))
path_output = here::here("output", "hypotheses")
dir.create(path_output, showWarnings = FALSE)
colors = hcl.colors(5, "Viridis")[1:3]

dt = data.table(
  transition = rep(c("B", "C", "D", "E", "F"), 6),
  probability = rep(c(c(0.7, 0.1, 0.1, 0.1, 0.0), c(0.35, 0.1, 0.1, 0.1, 0.35)), 3),
  phase = rep(c(rep("Unidirectional", 5), rep("Bidirectional", 5)), 3),
  hypothesis = c(rep("(a) No effect", 10),
                 rep("(b) Effect of\ntransititon probabilities", 10),
                 rep("(c) Effect of\ngraph structure", 10)),
  reaction_time = c(
    0.3, 0.3, 0.3, 0.3, 0.0,
    0.3, 0.3, 0.3, 0.3, 0.3,
    0.2, 0.4, 0.4, 0.4, 0.0,
    0.2, 0.4, 0.4, 0.4, 0.2,
    0.2, 0.3, 0.4, 0.5, 0.0,
    0.2, 0.3, 0.4, 0.3, 0.2
  )
)

plot_hypotheses = ggplot(data = dt, mapping = aes(
  x = as.factor(transition), y = as.numeric(reaction_time), fill = as.factor(probability))) +
  geom_bar(stat = "identity") +
  facet_grid(vars(fct_rev(as.factor(phase))), vars(as.factor(hypothesis))) +
  xlab("Transition from A to ...") +
  ylab("Behavioral responses | Classifier probabilities") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.background = element_blank()) +
  scale_fill_viridis_d(name = "Transition\nProbability") +
  theme(plot.title = element_text(hjust = 0.5, size = 10)) +
  theme(strip.text = element_text(margin = margin(b = 3, t = 3, r = 3, l = 3))) +
  theme(axis.title = element_text(family = "Helvetica", face = "plain", color = "black")) +
  theme(axis.text = element_text(family = "Helvetica", face = "plain", color = "black")) +
  theme(axis.line.x = element_line(colour = "black", linetype = "solid", color = "black")) +
  coord_capped_cart(expand = TRUE, top = "both", bottom = "both", right = "both") +
  theme(axis.text.y = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(legend.position = "bottom")

filename = "behavioral_hypotheses"
save_figure(plot = plot_hypotheses, filename, path = path_output, width = 5, height = 4)