rm(list = ls())
library(ggplot2)
library(tidyverse)
library(ggalluvial)
library(cowplot)
require(foreign)
require(MASS)
require(Hmisc)

my_label <- function(x) {
  # Wrap var names
  names(x)[[1]] <- stringr::str_wrap(gsub("_", " ", names(x)[[1]]), 30)
  names(x)[[1]] <- sub("question text", "", names(x)[[1]])
  # Wrap value labels
  x[[1]] <- stringr::str_wrap(gsub("_", " ", x[[1]]), 30)
  # Call label both with sep "\n"
  label_both(x, sep = "\n")
}

convert_to_factor <- function(col) {
  vals = unique(col)
  vals2 = sort(likertvals[ match(vals, likerts) ], decreasing = T)
  return(names(vals2))
}

likerts = c("Extensive", "Some", "None", 
            "Strongly disagree", "Disagree",
            "Somewhat disagree", "Neither Agree nor Disagree",
            "Somewhat agree", "Agree", "Strongly agree",
            "Never","one or two times", "monthly",
            "weekly", "Strongly Disagree", "Strongly Agree", "Neutral")
likertvals = c(3, 2, 1, 1, 2, 3, 4, 5, 6, 7, 1, 2, 3, 4, 1, 7, 4)
names(likertvals) = likerts

setwd("~/Dropbox (Personal)/research_projects-ACTIVE/CELL-manuscript/")

d = read.csv("data/survey_data-2023-08-04.csv")
q = read.csv("~/Dropbox (CSUDH)/Grants/CaEd_LearningLab/survey_data/analysis/CELL_survey_questions-2021-01-07.csv")
q = q %>% dplyr::select(Question_ID, type, SLO, short_text) %>% distinct()


cure = c("Bio327", "Bio333", "Bio421")
d$CURE = ifelse(d$class %in% cure, TRUE, FALSE)

dx = d %>% dplyr::select(Username2, class, Question_ID, pre_post, likert_val, CURE) %>%
  spread(pre_post, likert_val)
length(unique(dx$Username2))
dd = dx %>% filter(complete.cases(pre), complete.cases(post))
length(unique(dd$Username2))
d2 = dd %>% group_by(Question_ID) %>%
  summarise(percent_increase = sum(post > pre) / n())

c1 = dd %>% group_by(Question_ID, CURE) %>%
  summarise(percent_increase = sum(post > pre) / n()) %>% ungroup %>%
  spread(CURE, percent_increase) %>% mutate(diff = `FALSE` - `TRUE`) %>%
  left_join(q)


d2$pvalue = NA
d2$n = NA
sim = 1000

for (i in 1:nrow(d2)) {
  # for each question, simulate data
  ques = pull(d2[i, "Question_ID"])
  dd1 = dd %>% filter(Question_ID == ques)
  rands = lapply(sim, function(x) sample(dd1$pre) )
  randpos = unlist(lapply(rands, function(x) {sum(x > dd1$pre)})) / nrow(dd1)
  value = pull(d2[d2$Question_ID == ques, "percent_increase"])
  d2[d2$Question_ID == ques, "pvalue"] = sum(randpos > value) / length(randpos)
  d2[d2$Question_ID == ques, "n"] = nrow(dd1)
}

d3 = left_join(d2, q)


# sci method
d3 %>% filter(type == "scientific_method") %>% arrange(percent_increase)
# More than fifty percent of students show increased comfort with labs 
# in which they have some or full input, with uncertain outcomes - c6, c3
# While students show significant increases in confidence with reading the primary literature
# and collecting data, these gains are relatively more modest (~30% of students report increased confidence)

# communication
d3 %>% filter(type == "communication") %>% arrange(percent_increase)
# 34% and 40% of students (n = 216) report increased comfort in presenting results in writing and orally, respectively - c16 & c15
# In particular, students grew more comfortable communicating with their peers
# fifty-percent of students (n = 222) show increased ability to explain their thought process to their classmates (c45)

# quantitative
d3 %>% filter(type == "quantitative")
# 37% of students (n = 219) report increased comfort in analyzing data (p < 0.001) - c15

# teamwork
d3 %>% filter(type == "teamwork") %>% arrange(percent_increase)
# 45% of students report (n = 222) increase how much they enjoy learning from their peers 
# and report that their classmates collaborate well

# belonging
d3 %>% filter(type == "belonging") %>% arrange(percent_increase)
# Overall, we are doing better facilitating belonging among students
# relative to studetns and instructor
# see higher gains in peer relationships vs. instructor relationships
# one good one - feel comfortable asking for help (48% increase (n = 222))

# inclusion
d3 %>% filter(type == "inclusion")
# 58% (n = 187) of students say that they know of one or more important scientist to whom they relate

questions = c("c6", "c45", "c14")
# sizes = c(3.5, 2, 3.5)
# questions = c("c36", "c42", "c54")
sizes = c(2, 2, 2)
qtext = pull(d3[match(questions, d3$Question_ID), "short_text"])
plts = vector("list", length(qtext))

d[d$Question_ID == "c54" & d$answer == "Somewhat agree", "answer"] = "Neutral"
d[d$Question_ID == "c54" & d$answer == "Somewhat disagree", "answer"] = "Neutral"

for (i in 1:length(plts)) {
  dd2 = d %>% filter(Question_ID == questions[i]) %>% 
    filter(answer != "<Unanswered>", answer != "I don't know") %>%
    dplyr::select(Username2, pre_post, answer) %>% spread(key = pre_post, value = answer) %>%
    mutate(post = ifelse(post == "Strongly disagree", "Strongly Disagree", post)) %>%
    mutate(post = ifelse(post == "Strongly agree", "Strongly Agree", post)) %>%
    mutate(pre = ifelse(pre == "Neither Agree nor Disagree", "Neutral", pre)) %>%
    mutate(pre = ifelse(pre == "Strongly disagree", "Strongly Disagree", pre)) %>%
    mutate(pre = ifelse(pre == "Strongly agree", "Strongly Agree", pre)) %>%
    group_by(pre, post) %>% 
    dplyr::summarize(count = n()) %>% ungroup() %>%
    filter(complete.cases(post), complete.cases(pre), pre != "Not Applicable")
  dd2 = as.data.frame(dd2)
  dd2$pre = factor(dd2$pre, levels = convert_to_factor(dd2$pre))
  dd2$post = factor(dd2$post, levels = convert_to_factor(dd2$post))
  plts[[i]] = ggplot(data = dd2, aes(axis1 = pre, axis2 = post, y = count, label = pre)) +
    scale_x_discrete(limits = c("pre", "post")) + ylab("") +
    xlab(qtext[i]) +
    geom_alluvium(aes(fill = post)) + geom_stratum() + 
    theme_classic() +
    geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = sizes[i]) + 
    scale_fill_manual(values = rev(RColorBrewer::brewer.pal(length(unique(c(dd2$pre, dd2$post))), "YlGnBu"))) +
    theme(legend.position = "none", 
          axis.line = element_blank(),
          plot.margin=unit(c(0,0,0,0), "pt"),
          axis.ticks.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          plot.title = element_textbox_simple(size = 5, halign = 0.5))
}

ab = plot_grid(plotlist = plts, ncol = 3, labels = c("A", "B", "C"), align = "v")
save_plot("figures/analysis2.pdf", ab, base_height = 3.5, base_width = 14)
# save_plot("figures/analysis3.pdf", ab, base_height = 3.5, base_width = 14)

questions = unique(d$Question_ID)
res = vector("list", length(questions))
for (i in 1:length(questions)) {
  dd2 = d %>% filter(Question_ID == questions[i]) %>% 
    filter(answer != "<Unanswered>", answer != "I don't know") %>%
    dplyr::select(Username2, pre_post, answer) %>% spread(key = pre_post, value = answer) %>%
    mutate(post = ifelse(post == "Strongly disagree", "Strongly Disagree", post)) %>%
    mutate(post = ifelse(post == "Strongly agree", "Strongly Agree", post)) %>%
    filter(complete.cases(post))
  dd2 = as.data.frame(dd2)
  dd2$post = factor(dd2$post, levels = convert_to_factor(dd2$post))
  
  dd = d %>% dplyr::select(Username2, SEX, IPEDS_RACEETH, FIRST_GEN, STUD_TYPE) %>% unique()
  dd3 = left_join(dd2, dd, by = c("Username2" = "Username2")) %>% 
    filter(IPEDS_RACEETH != "Unknown") %>% filter(FIRST_GEN != "Unknown")
  
  dd3$race = ifelse(dd3$IPEDS_RACEETH %in% c("White", "Asian American"), "non-URM", "URM")
  
  m = polr(formula = post ~ SEX + race, data = dd3, Hess = TRUE)
  ctable <- coef(summary(m))
  p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
  ctable <- as.data.frame(cbind(ctable, "pvalue" = p))
  ctable$variable = rownames(ctable)
  rownames(ctable) = NULL
  ctable$question = questions[i]
  res[[i]] = ctable[1:2, ]
}
res2 = do.call("rbind", res)
res2[res2$pvalue < 0.05, ]

toppers = c("AGREE", "EXTENSIVE", "STRONGLY AGREE", "WEEKLY")
drop = "c26, c7, c8, c22, c21, c25, c28, c17, c20, c40, c19, c30, c43, c44, c39, c32, c51, c48, c57, c52, c56, c47, c31, c60, c34, c23, c24, c4, c1, c2, c59, c33, c37, c41, c46, c12, c18, c3"
drop = strsplit(drop, ", ")[[1]]
t1 = d %>% filter(pre_post == "post", complete.cases(answer), answer != "<Unanswered>") %>% 
  mutate(answer = toupper(answer)) %>% 
  group_by(Question_ID) %>%
  dplyr::summarize(per_strong = sum(answer %in% toppers) / n()) %>% left_join(q) %>%
  mutate(per_strong = round(per_strong, 3) * 100) %>%
  arrange(type, desc(per_strong)) %>% 
  filter(!Question_ID %in% drop) %>%
  left_join(d2) %>% 
  dplyr::select(type, short_text, percent_increase, per_strong)

write.csv(t1, "~/Desktop/Table1.csv", row.names = F)

qf  = strsplit("c29, c36, c42, c38, c15, c16, c45, c54, c14, c6, c13", ", ")[[1]]
c2 = d %>% filter(pre_post == "post", complete.cases(answer), answer != "<Unanswered>") %>% 
  mutate(answer = toupper(answer)) %>% 
  group_by(Question_ID, CURE) %>%
  dplyr::summarize(per_strong = sum(answer %in% toppers) / n())%>% ungroup %>%
  left_join(q) %>% filter(Question_ID %in% qf)

vars = pull(c2 %>% tidyr::spread(CURE, per_strong) %>%
  mutate(diff = `TRUE` - `FALSE`) %>% arrange(diff) %>% 
  dplyr::select(short_text))
c2$short_text2 = factor(c2$short_text, levels = vars)
c2$type2 = ifelse(c2$CURE == TRUE, "CURE", "non-CURE")
c3 = c2 %>% dplyr::select(short_text2, type2, per_strong) %>% 
  spread(type2, per_strong) %>%
  mutate(diff = ifelse(`non-CURE` - CURE > 0, "non-CURE", "CURE"))
g2 = ggplot(c2) + 
  geom_point(aes(per_strong, short_text2, fill = type2), shape = 21, size = 2.5) +
  geom_segment(data = c3, aes(xend = CURE, x = `non-CURE`, 
                              y = short_text2, yend = short_text2, col = diff), 
               arrow = arrow(length = unit(0.03, "npc"))) +
  theme_classic() + ylab("") + xlab("% students agreeing") +
  theme(legend.title = element_blank()) + 
  guides(color = FALSE) +
  scale_fill_manual(values = c("white", "black")) +
  scale_colour_manual(values = c("black", "red"))
save_plot("figures/analysis_CURE.png", g2, base_height = 4, base_width = 8)
