## Example 0. Illutrative example

library(ggplot2)

my_plot=ggplot()+
  scale_x_continuous(limits=c(-1, 0.25), name = latex2exp::TeX("$X\\hat{\\beta}_1$")) +
  scale_y_continuous(limits=c(-1.5, 1.5), latex2exp::TeX("$CSTE = g_1(X\\hat{\\beta}_1)$")) +
  coord_cartesian(expand = FALSE)+
  geom_function(fun = function(x) sin(-4*x)-0.2, aes(color ="CSTE"), linewidth=1)+
  geom_function(fun = function(x) sin(-4*x)+0.1, aes(color ="Simultaneous \nconfidence \nband"), linewidth=1)+
  geom_function(fun = function(x) sin(-4*x)-0.5, aes(color ="Simultaneous \nconfidence \nband"), linewidth=1) +
  geom_hline(yintercept=0, lty=2, alpha=0.5) +
  geom_segment(aes(x = c(0.025, -0.13, -0.655, -0.81), y = -1.5, xend = c(0.025, -0.13, -0.655, -0.81), yend = 0), lty=5) +
  annotate("text", x=-0.82, y=-1.45, label= "a")+
  annotate("text", x=-0.64, y=-1.45, label= "b")+
  annotate("text", x=-0.15, y=-1.45, label= "c")+
  annotate("text", x=0.035, y=-1.45, label= "d")+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey"),
        legend.key = element_rect (fill = "white"),
        legend.background = element_rect(fill = "white", color = "black"))+
  ggtitle("Illustrative CSTE curve") +
  scale_colour_manual("Curve",
                      breaks = c("CSTE", "Simultaneous \nconfidence \nband"),
                      values = c("#F8766D", "#87cefa"))


ggsave("eg.png", plot = my_plot, width = 12, height = 5, dpi = 300)