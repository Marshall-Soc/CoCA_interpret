
# -----------------------------------------------------------------------------
# Make figure #4
# -----------------------------------------------------------------------------

fit.rating <- Effect("rating", model)

fit.rating <- data.frame(fit.rating$x, fit.rating$prob, fit.rating$lower.prob,
                         fit.rating$upper.prob)

rating.prob.plot <- ggplot(fit.rating, aes(x="1", y=prob.X1, group=rating, color=rating)) +
  geom_point(size=3, aes(x="1", color=rating), 
             position = position_dodge(width=.3)) +
  geom_errorbar(aes(x="1", ymin=L.prob.X1, ymax=U.prob.X1,
                    color=rating), 
                width=.1,
                position = position_dodge(width=.3)) +
  geom_point(size=3, aes(x="2",  y=prob.X2, color=rating), 
             position = position_dodge(width=.3)) +
  geom_errorbar(aes(x="2", ymin=L.prob.X2, ymax=U.prob.X2,
                    color=rating), 
                width=.1,
                position = position_dodge(width=.3)) +
  geom_point(size=3, aes(x="3",  y=prob.X3, color=rating), 
             position = position_dodge(width=.3)) +
  geom_errorbar(aes(x="3", ymin=L.prob.X3, ymax=U.prob.X3,
                    color=rating), 
                width=.1,
                position = position_dodge(width=.3)) +
  geom_point(size=3, aes(x="4",  y=prob.X4, color=rating), 
             position = position_dodge(width=.3)) +
  geom_errorbar(aes(x="4", ymin=L.prob.X4, ymax=U.prob.X4,
                    color=rating), 
                width=.1,
                position = position_dodge(width=.3)) +
  geom_point(size=3, aes(x="5",  y=prob.X5, color=rating), 
             position = position_dodge(width=.3)) +
  geom_errorbar(aes(x="5", ymin=L.prob.X5, ymax=U.prob.X5,
                    color=rating), 
                width=.1,
                position = position_dodge(width=.3)) +
  ylab("Predicted Probability") + xlab("") +
  #ylim(c(.12,.47)) +
  theme_bw() +
  theme(axis.text.y=element_text(size=12),
        legend.title=element_text(face="bold", size=12),
        legend.position="bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size=12),
        axis.title.y=element_text(size=12),
        plot.title=element_text(face="bold", size=12)) +
  scale_color_manual(name="Ideology", values=c("#a51414","#003366"),
                     labels=c("Conservative","Liberal")) +
  guides(color = guide_legend(title.position = "top",
                              title.hjust = 0.5)) +
  scale_x_discrete(breaks=c("1","2","3","4","5"), labels=c("Class #1","Class #2",
                                                           "Class #3","Class #4","Class #5"))
# scale_x_discrete(breaks=c("1","2","3","4","5"),
#                  labels=c("Old\nInfluential Men,\nYoung\nMoral Women", "(Not) Old\nWhite Men",
#                           "Anything But\nRace", "Ascribed vs.\nAchieved",
#                           "Anything But\nGender"))

png("figures/fig4.png", width = 6, height = 4, units = 'in', res = 350)
rating.prob.plot
dev.off()
