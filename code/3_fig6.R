
# -----------------------------------------------------------------------------
# Make figure #6
# -----------------------------------------------------------------------------

time.rating <- Effect(c("day","rating"), model, xlevels = list(day = seq(1, 366, by = 33)))

time.rating <- data.frame(time.rating$x, time.rating$prob, time.rating$lower.prob,
                          time.rating$upper.prob)
time.rating <- merge(time.rating, blog.data[,c("day","date")], 
                     by = "day") %>% distinct()

time.prob.plot.1 <- ggplot(time.rating,
                           aes(x=date, y=prob.X1, group=rating, color=rating)) +
  geom_line(aes(color=rating)) +
  geom_ribbon(aes(ymin=L.prob.X1, ymax=U.prob.X1, fill=rating), alpha=.2) +
  ylab("") + xlab("") +
  labs(title="Class #1") +
  #labs(title="Old Influential Men,\nYoung Moral Women") +
  ylim(c(min(time.rating$L.prob.X1, time.rating$L.prob.X2, time.rating$L.prob.X3, 
             time.rating$L.prob.X4, time.rating$L.prob.X5),
         max(time.rating$U.prob.X1, time.rating$U.prob.X2, time.rating$U.prob.X3, 
             time.rating$U.prob.X4, time.rating$U.prob.X5))) +
  theme_bw() +
  theme(axis.text.y=element_text(size=12),
        legend.title=element_text(face="bold", size=12),
        legend.position="right",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size=8, angle=45, vjust=.9, hjust=1),
        axis.title=element_text(size=12),
        plot.title=element_text(face="bold", size=12, hjust=.5)) +
  scale_color_manual(name="Ideology", values=c("#a51414","#003366"),
                     labels=c("Conservative", "Liberal")) +
  scale_fill_manual(name="Ideology", values=c("#a51414","#003366"),
                    labels=c("Conservative", "Liberal")) 

time.prob.plot.2 <- ggplot(time.rating,
                           aes(x=date, y=prob.X2, group=rating, color=rating)) +
  geom_line(aes(color=rating)) +
  geom_ribbon(aes(ymin=L.prob.X2, ymax=U.prob.X2, fill=rating), alpha=.2) +
  ylab("") + xlab("") +
  labs(title="Class #2") +
  #labs(title="(Not) Old White Men") +
  ylim(c(min(time.rating$L.prob.X1, time.rating$L.prob.X2, time.rating$L.prob.X3, 
             time.rating$L.prob.X4, time.rating$L.prob.X5),
         max(time.rating$U.prob.X1, time.rating$U.prob.X2, time.rating$U.prob.X3, 
             time.rating$U.prob.X4, time.rating$U.prob.X5))) +
  theme_bw() +
  theme(axis.text.y=element_text(size=12),
        legend.title=element_text(face="bold", size=12),
        legend.position="right",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size=8, angle=45, vjust=.9, hjust=1),
        axis.title=element_text(size=12),
        plot.title=element_text(face="bold", size=12, hjust=.5)) +
  scale_color_manual(name="Ideology", values=c("#a51414","#003366"),
                     labels=c("Conservative", "Liberal")) +
  scale_fill_manual(name="Ideology", values=c("#a51414","#003366"),
                    labels=c("Conservative", "Liberal"))

time.prob.plot.3 <- ggplot(time.rating,
                           aes(x=date, y=prob.X3, group=rating, color=rating)) +
  geom_line(aes(color=rating)) +
  geom_ribbon(aes(ymin=L.prob.X3, ymax=U.prob.X3, fill=rating), alpha=.2) +
  ylab("") + xlab("") +
  labs(title="Class #3") +
  #labs(title="Anything But Race") +
  ylim(c(min(time.rating$L.prob.X1, time.rating$L.prob.X2, time.rating$L.prob.X3, 
             time.rating$L.prob.X4, time.rating$L.prob.X5),
         max(time.rating$U.prob.X1, time.rating$U.prob.X2, time.rating$U.prob.X3, 
             time.rating$U.prob.X4, time.rating$U.prob.X5))) +
  theme_bw() +
  theme(axis.text.y=element_text(size=12),
        legend.title=element_text(face="bold", size=12),
        legend.position="right",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size=8, angle=45, vjust=.9, hjust=1),
        axis.title=element_text(size=12),
        plot.title=element_text(face="bold", size=12, hjust=.5)) +
  scale_color_manual(name="Ideology", values=c("#a51414","#003366"),
                     labels=c("Conservative", "Liberal")) +
  scale_fill_manual(name="Ideology", values=c("#a51414","#003366"),
                    labels=c("Conservative", "Liberal")) 

time.prob.plot.4 <- ggplot(time.rating,
                           aes(x=date, y=prob.X4, group=rating, color=rating)) +
  geom_line(aes(color=rating)) +
  geom_ribbon(aes(ymin=L.prob.X4, ymax=U.prob.X4, fill=rating), alpha=.2) +
  ylab("") + xlab("") +
  labs(title="Class #4") +
  #labs(title="Ascribed vs. Achieved") +
  ylim(c(min(time.rating$L.prob.X1, time.rating$L.prob.X2, time.rating$L.prob.X3, 
             time.rating$L.prob.X4, time.rating$L.prob.X5),
         max(time.rating$U.prob.X1, time.rating$U.prob.X2, time.rating$U.prob.X3, 
             time.rating$U.prob.X4, time.rating$U.prob.X5))) +
  theme_bw() +
  theme(axis.text.y=element_text(size=12),
        legend.title=element_text(face="bold", size=12),
        legend.position="right",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size=8, angle=45, vjust=.9, hjust=1),
        axis.title=element_text(size=12),
        plot.title=element_text(face="bold", size=12, hjust=.5)) +
  scale_color_manual(name="Ideology", values=c("#a51414","#003366"),
                     labels=c("Conservative", "Liberal")) +
  scale_fill_manual(name="Ideology", values=c("#a51414","#003366"),
                    labels=c("Conservative", "Liberal"))

time.prob.plot.5 <- ggplot(time.rating,
                           aes(x=date, y=prob.X5, group=rating, color=rating)) +
  geom_line(aes(color=rating)) +
  geom_ribbon(aes(ymin=L.prob.X5, ymax=U.prob.X5, fill=rating), alpha=.2) +
  ylab("") + xlab("") +
  labs(title="Class #5") +
  #labs(title="Anything But Gender") +
  ylim(c(min(time.rating$L.prob.X1, time.rating$L.prob.X2, time.rating$L.prob.X3, 
             time.rating$L.prob.X4, time.rating$L.prob.X5),
         max(time.rating$U.prob.X1, time.rating$U.prob.X2, time.rating$U.prob.X3, 
             time.rating$U.prob.X4, time.rating$U.prob.X5))) +
  theme_bw() +
  theme(axis.text.y=element_text(size=12),
        legend.title=element_text(face="bold", size=12),
        legend.position="right",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size=8, angle=45, vjust=.9, hjust=1),
        axis.title=element_text(size=12),
        plot.title=element_text(face="bold", size=12, hjust=.5)) +
  scale_color_manual(name="Ideology", values=c("#a51414","#003366"),
                     labels=c("Conservative", "Liberal")) +
  scale_fill_manual(name="Ideology", values=c("#a51414","#003366"),
                    labels=c("Conservative", "Liberal"))

g_legend <- function(a.gplot){ 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  return(legend)} 

legend <- g_legend(time.prob.plot.5) 
legend <- as_ggplot(legend)

png("figures/fig6.png", width = 8, height = 6, units = 'in', res = 350)
time.plots <- ggarrange(time.prob.plot.1, time.prob.plot.2, time.prob.plot.3, 
                        time.prob.plot.4, time.prob.plot.5, legend,
                        legend = "none", align = "hv", nrow=2, ncol=3)
annotate_figure(time.plots, left = text_grob("Predicted Probability", 
                                             size=12, rot=90))
dev.off()