rm(list = ls())

library(tidyverse)
library(stringr)

setwd("~/Dropbox (Personal)/research_projects-ACTIVE/CELL-manuscript/")
d = read.csv("data/Mini-CELL grant Survey (Responses) - Form Responses 1.csv")
d$Timestamp = NULL
nn = gsub("In.your.redesigned.class..did.the.students.do.the.following...", "", names(d))
names(d) = gsub("\\.", " ", gsub("..i.e..*", "", nn))
names(d)[11] = "type"
names(d)[1] = "class"
dd = d %>% gather(activity, level, -class, -type)
vals = c(0, 1, 2)
names(vals) = c("None", "Some", "Extensive")
dd$levelnum = vals[ dd$level ]
dd1 = dd %>% group_by(type, activity) %>% summarize(level = mean(levelnum, na.rm = T))

dd1[dd1$activity == "collect data of broader relevance", "activity"] = "broader relevance"
dd1[dd1$activity == "design exp", "activity"] = "design experiments"
dd1[dd1$activity == "engage in discovery", "activity"] = "discovery"
dd1$activity = gsub(" $", "", dd1$activity)

dd1$typeF = factor(dd1$type, levels = c("CURE", "open-ended", "guided inquiry"))
dd1$activityF = factor(dd1$activity, 
                       levels = c("collaborate", "generate questions", "form hypotheses",
                                  "design experiments", "collect data", "analyze data",
                                  "iterate", "broader relevance", "discovery"))

xx = ggplot(dd1, aes(activityF, typeF, size = level, col = type)) +
  geom_point() + xlab("") + ylab("") + 
  scale_x_discrete(labels = function(x) str_wrap(str_replace_all(x, "foo" , " "), width = 10)) +
  theme_classic() + theme(legend.position = "none", 
                          axis.line = element_blank(), 
                          axis.ticks = element_blank(), 
                          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_color_manual(values = c("#2c7fb8", "#a1dab4", "#41b6c4"))
cowplot::save_plot("figures/continuum.png", xx, base_height = 2, base_width = 4.5)
