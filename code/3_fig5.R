
# -----------------------------------------------------------------------------
# Make figure #5
# -----------------------------------------------------------------------------

# Left panel
female.rating <- Effect("female", model2, xlevels = list(day = 201))

female.rating <- data.frame(female.rating$x, female.rating$prob, female.rating$lower.prob,
                            female.rating$upper.prob)

female.prob.plot <- ggplot(female.rating, aes(x="1", y=prob.X1, group=female, color=female)) +
  geom_point(size=3, aes(x="1", color=female), 
             position = position_dodge(width=.3)) +
  geom_errorbar(aes(x="1", ymin=L.prob.X1, ymax=U.prob.X1,
                    color=female), 
                width=.1,
                position = position_dodge(width=.3)) +
  geom_point(size=3, aes(x="2",  y=prob.X2, color=female), 
             position = position_dodge(width=.3)) +
  geom_errorbar(aes(x="2", ymin=L.prob.X2, ymax=U.prob.X2,
                    color=female), 
                width=.1,
                position = position_dodge(width=.3)) +
  geom_point(size=3, aes(x="3",  y=prob.X3, color=female), 
             position = position_dodge(width=.3)) +
  geom_errorbar(aes(x="3", ymin=L.prob.X3, ymax=U.prob.X3,
                    color=female), 
                width=.1,
                position = position_dodge(width=.3)) +
  geom_point(size=3, aes(x="4",  y=prob.X4, color=female), 
             position = position_dodge(width=.3)) +
  geom_errorbar(aes(x="4", ymin=L.prob.X4, ymax=U.prob.X4,
                    color=female), 
                width=.1,
                position = position_dodge(width=.3)) +
  geom_point(size=3, aes(x="5",  y=prob.X5, color=female), 
             position = position_dodge(width=.3)) +
  geom_errorbar(aes(x="5", ymin=L.prob.X5, ymax=U.prob.X5,
                    color=female), 
                width=.1,
                position = position_dodge(width=.3)) +
  ylim(.1,.5) +
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
  scale_color_manual(name="Gender", values=c("#55b748","#1696d2"),
                     labels=c("Man (Obama, McCain, Biden, and/or Bush)",
                              "Woman (Clinton and/or Palin)")) +
  guides(color = guide_legend(title.position = "top",
                              title.hjust = 0.5)) +
  scale_x_discrete(breaks=c("1","2","3","4","5"), labels=c("Class #1","Class #2",
                                                           "Class #3","Class #4","Class #5"))
# scale_x_discrete(breaks=c("1","2","3","4","5"),
#                  labels=c("Old\nInfluential Men,\nYoung\nMoral Women", "(Not) Old\nWhite Men",
#                           "Anything But\nRace", "Ascribed vs.\nAchieved",
#                           "Anything But\nGender"))


# Right panel
obama.rating <- lsmeans(model, pairwise ~ rating*obama | group, mode = "prob", at = list(
  day = median(blog.data$day), n_ppl_article = mean(blog.data$n_ppl_article),
  biden = F, mccain = F, palin = F, clinton = F, biden = F, bush = F))

obama.rating <- data.frame(obama.rating$lsmeans)[,c(1,2,3,4,7,8)]

mccain.rating <- lsmeans(model, pairwise ~ mccain*rating | group, mode = "prob", at = list(
  date = mean(blog.data$date), n_ppl_article = mean(blog.data$n_ppl_article),
  biden = F, obama = F, palin = F, clinton = F, biden = F, bush = F))

mccain.rating <- data.frame(mccain.rating$lsmeans)[,c(1,2,3,4,7,8)]

names(obama.rating)[names(obama.rating) == "obama"] <- "candidate"
names(mccain.rating)[names(mccain.rating) == "mccain"] <- "candidate"

obama.rating$candidate <- plyr::revalue(factor(obama.rating$candidate), c("FALSE"="No","TRUE"="obama"))
mccain.rating$candidate <- plyr::revalue(factor(mccain.rating$candidate), c("FALSE"="No","TRUE"="mccain"))

candidate.rating <- rbind(obama.rating, mccain.rating)
candidate.rating <- distinct(candidate.rating)

cand.prob.cons.plot <- ggplot(candidate.rating[which(candidate.rating$rating=="Conservative"),],
                              aes(x=group, y=prob, group=candidate, color=candidate)) +
  geom_point(size=1, aes(color=candidate), position = position_dodge(width=.3)) +
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL,
                    color=candidate), width=.1,
                position = position_dodge(width=.3)) +
  ylab("Predicted Probability") + xlab("") +
  labs(title="Conservative Blog Posts") +
  ylim(c(min(candidate.rating$lower.CL), max(candidate.rating$upper.CL))) +
  theme_bw() +
  theme(axis.text.y=element_text(size=12),
        legend.title=element_text(face="bold", size=12),
        legend.position="bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size=12),
        axis.title=element_text(size=12),
        plot.title=element_text(face="bold", size=12, hjust=.5)) +
  scale_color_manual(name="", values=c("gray50","#003366","#a51414"),
                     labels=c("No Mention of McCain, Clinton, Biden, Bush, Palin, or Obama",
                              "Mentions Obama, but not Clinton, Biden, Bush, Palin, or McCain",
                              "Mentions McCain, but not Clinton, Biden, Bush, Palin, or Obama")) +
  guides(color = guide_legend(title.position = "left",
                              title.hjust = 0.5, direction="vertical")) +
  scale_x_discrete(breaks=c(1,2,3,4,5), labels=c("Class #1","Class #2",
                                                 "Class #3","Class #4","Class #5")) #+
# scale_x_discrete(breaks=c(1,2,3,4,5), labels=c("Old\nInfluential Men,\nYoung\nMoral Women", "(Not) Old\nWhite Men",
#                                                "Anything But\nRace", "Ascribed vs.\nAchieved",
#                                                "Anything But\nGender")) +
#coord_flip()

cand.prob.lib.plot <- ggplot(candidate.rating[which(candidate.rating$rating=="Liberal"),],
                             aes(x=group, y=prob, group=candidate, color=candidate)) +
  geom_point(size=1, aes(color=candidate), position = position_dodge(width=.3)) +
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL,
                    color=candidate), width=.1,
                position = position_dodge(width=.3)) +
  ylab("Predicted Probability") + xlab("") +
  labs(title="Liberal Blog Posts") +
  ylim(c(min(candidate.rating$lower.CL), max(candidate.rating$upper.CL))) +
  theme_bw() +
  theme(axis.text.y=element_text(size=12),
        legend.title=element_text(face="bold", size=12),
        legend.position="bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size=12),
        axis.title=element_text(size=12),
        plot.title=element_text(face="bold", size=12, hjust=.5)) +
  scale_color_manual(name="", values=c("gray50","#003366","#a51414"),
                     labels=c("No Mention of McCain, Clinton, Biden, Bush, Palin, or Obama",
                              "Mentions Obama, but not Clinton, Biden, Bush, Palin, or McCain",
                              "Mentions McCain, but not Clinton, Biden, Bush, Palin, or Obama")) +
  guides(color = guide_legend(title.position = "left",
                              title.hjust = 0.5, direction="vertical")) +
  scale_x_discrete(breaks=c(1,2,3,4,5), labels=c("Class #1","Class #2",
                                                 "Class #3","Class #4","Class #5")) #+
# scale_x_discrete(breaks=c(1,2,3,4,5), labels=c("Old\nInfluential Men,\nYoung\nMoral Women", "(Not) Old\nWhite Men",
#                                                "Anything But\nRace", "Ascribed vs.\nAchieved",
#                                                "Anything But\nGender")) +
#coord_flip()

png("figures/fig5.png", width = 14, height = 8.5, units = 'in', res = 350)
ggarrange(female.prob.plot, ggarrange(cand.prob.cons.plot, cand.prob.lib.plot,
                                      common.legend = T, legend = "bottom", align="hv",
                                      nrow=2),
          align="v", widths=c(1.5,1), ncol=2, labels=c("A","B"))
dev.off()
