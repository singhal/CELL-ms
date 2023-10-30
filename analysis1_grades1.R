rm(list = ls())
library(tidyverse)
library(rJava)
library(glmulti)
library(cowplot)

setwd("~/Dropbox (CSUDH)/Grants/CaEd_LearningLab/survey_data/")

d = readxl::read_xlsx("BIO_GRADES_CA_GRANT_DATA_11.23.22.xlsx")

gradenames = c("A", "A-", "B+", "B", "B-",
               "C+", "C", "C-", "D+", "D",
               "F", "W", "I", "WE", "WU")
pts = c(4, 3.7, 3.3, 3, 2.7,
        2.3, 2, 1.7, 1.3, 1,
        0, NA, NA, NA, NA)
simple = c("A", "A", "B", "B", "B", "C", "C", 
           "not-pass", "not-pass", "not-pass", "not-pass", 
           "not-pass", "not-pass", "not-pass", "not-pass")


# get rid of intermediate grades
d1 = d[grep("_1$", names(d), invert = T)]
d2 = d1[grep("_2$", names(d1), invert = T)]
names(d2) = gsub("_F", "", names(d2))

courses = names(d2)[ grep("BIO", names(d2)) ]
# gather the data
d3 = d2 %>% select(EMPLID, all_of(courses)) %>% 
  gather(course, grade, -EMPLID) %>%
  filter(complete.cases(grade)) %>% 
  left_join(d2 %>% select(-courses))
d3$course_num = as.numeric(gsub("BIO", "", d3$course))

# add in course data
d3$course_type = ifelse(d3$course_num %% 2 == 0, "lecture", "lab")
d3[d3$course == "BIO342", "course_type"] = "lab"
d3$course_level = ifelse(d3$course_num >= 300, "upper", "lower")

# simplify grades
d3$grade_point = pts[ match(d3$grade, gradenames) ]
d3$grade_simple = simple[ match(d3$grade, gradenames) ]

d4 = d3 %>% filter(course_type == "lab")

# only keep the grades that match to our survey data
x = read.csv("~/Dropbox (Personal)/research_projects-ACTIVE/CELL-manuscript/data/unique_student_IDs_courses.4August23.csv")
x$course = gsub(" ", "", x$course)
x$ID = as.character(x$ID)
xx = left_join(x, d4, by = c("ID" = "EMPLID", "course" = "course"))

# who is missing?
xx[!complete.cases(xx$grade), ]
# a few random students in each class - who knows
xx1 = xx[complete.cases(xx$grade), ]

nrow(xx1[xx1$grade_simple == "not-pass", ]) / nrow(xx1)
nrow(xx1[xx1$grade_simple == "A", ]) / nrow(xx1)

r = table(d$IPEDS_RACEETH)
r = names(r[r > 20])
# remove categories with only a few students in them
d5 = xx1 %>% filter(IPEDS_RACEETH %in% r) %>% 
  filter(FIRST_GEN != "Unknown")

# pell-eligible data seems wrong
# do not include for now
m1 = glmulti(y = "grade_point", 
             xr = c("SEX", "IPEDS_RACEETH", "FIRST_GEN", "STUD_TYPE"),
             data = d5,
             level = 2, fitfunction = lm, 
             marginality = TRUE, crit = "aicc", confsetsize = 100)

summary(m1@objects[[1]])

d6 = d5 %>% group_by(STUD_TYPE, grade_simple) %>% 
  summarize(value = n()) %>% ungroup()
d6[d6$STUD_TYPE == "Transfer Student", "STUD_TYPE"] = "transfer"
d6[d6$STUD_TYPE != "transfer", "STUD_TYPE"] = "non-transfer"
g0 = ggplot(d6) + 
  geom_bar(aes(fill=grade_simple, y=value, x=STUD_TYPE), position="fill", stat="identity") + 
  geom_text(data = d6 %>% group_by(STUD_TYPE) %>% summarize(value = sum(value)) %>% ungroup(), 
            aes(x = STUD_TYPE, label = value), y =  1.03, size = 2, hjust = 0.5) +
  theme_classic() +
  xlab("transfer status") + ylab("grade distribution") +
  scale_fill_manual(values = rev(RColorBrewer::brewer.pal(4, "YlGnBu")))
d6 = d5 %>% group_by(SEX, grade_simple) %>% 
  summarize(value = n()) %>% ungroup() 
g1 = ggplot(d6) + 
  geom_bar(aes(fill=grade_simple, y=value, x=SEX), position="fill", stat="identity") + 
  geom_text(data = d6 %>% group_by(SEX) %>% summarize(value = sum(value)) %>% ungroup(), 
            aes(x = SEX, label = value), y =  1.03, size = 2, hjust = 0.5) +
  theme_classic() +
  xlab("sex") + ylab("grade distribution") +
  scale_fill_manual(values = rev(RColorBrewer::brewer.pal(4, "YlGnBu")))
d6 = d5 %>% group_by(IPEDS_RACEETH, grade_simple) %>% 
  summarize(value = n()) %>% ungroup()
d6[d6$IPEDS_RACEETH == "Asian American", "IPEDS_RACEETH"] = "Asian"
d6[d6$IPEDS_RACEETH == "Black/African American", "IPEDS_RACEETH"] = "Black"
d6[d6$IPEDS_RACEETH == "Hispanic/Latino", "IPEDS_RACEETH"] = "Latino"
g2 = ggplot(d6) + 
  geom_bar(aes(fill=grade_simple, y=value, x=IPEDS_RACEETH), position="fill", stat="identity") + 
  geom_text(data = d6 %>% group_by(IPEDS_RACEETH) %>% summarize(value = sum(value)) %>% ungroup(), 
            aes(x = IPEDS_RACEETH, label = value), y =  1.03, size = 2, hjust = 0.5) +
  theme_classic() +
  xlab("race") + ylab("grade distribution") +
  scale_fill_manual(values = rev(RColorBrewer::brewer.pal(4, "YlGnBu")))
gg = plot_grid(   g0 + theme(legend.position="none"), 
                  g1 + theme(legend.position="none"), 
                   g2 + theme(legend.position="none"),
                   labels = c("A", "B", "C"), nrow = 1)
legend <- get_legend(
  # create some space to the left of the legend
  g2 + theme(legend.box.margin = margin(0, 0, 0, 6), legend.title=element_blank())
)
gg1 = plot_grid(gg, legend, rel_widths = c(3, .4))
save_plot("~/Dropbox (Personal)/research_projects-ACTIVE/CELL-manuscript/figures/grades.png",
          gg1, base_height = 2.5, base_width = 8)

# account for number of retakes
