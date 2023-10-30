rm(list = ls())
library(dplyr)
library(tidyr)
theme_set(theme_cowplot())

# questions changed across surveys
# so need to be careful
# match questions to SLOs

setwd("~/Dropbox (CSUDH)/Grants/CaEd_LearningLab/survey_data/")
q = read.csv("analysis/CELL_survey_questions-2021-01-07.csv",
             stringsAsFactors = F)

convert_data <- function(df, class, pre_post) {
  
  # convert download wide into download long
  df2 = df %>% 
    dplyr::select("Username", 
                  grep("Answer", names(df), value = T))
  df3 = df2 %>% gather(key = "question", value = "answer", -Username)
  df3$question = gsub("Answer", "Question", df3$question)
  
  qq = df %>% 
    dplyr::select(grep("Question\\.\\d", names(df), value = T)) %>%
    gather(key = "question", value = "question_text") %>% unique()
  qq$Question_ID = q[match(qq$question_text, q$Question), "Question_ID"]
  
  df3$Question_ID =  qq[match(df3$question, qq$question), "Question_ID"]
  df3$question = NULL
  df3$class = class
  df3$pre_post = pre_post
  return(df3)
}

convert_long_data <- function(df, class, pre_post) {
  # convert download wide into download long
  df2 = df %>% 
    dplyr::select("Username", question = Question.ID, answer = Answer)
  
  qq = df %>% 
    dplyr::select(Question.ID, Question) %>% unique()
  qq$Question_ID = q[match(qq$Question, q$Question), "Question_ID"]
  
  df2$Question_ID =  qq[match(df2$question, qq$Question.ID), "Question_ID"]
  df2$question = NULL
  
  df2$class = class
  df2$pre_post = pre_post
  return(df2)
}


a1 = read.csv("CSUDH/BIO 121-01 Fall 2020/Pre-Survey.download.csv",
              stringsAsFactors = F)
a1 = convert_data(a1, "Bio121", "pre")
b1 = read.csv("CSUDH/BIO 121-01 Fall 2020/Post-Survey.download.csv",
              stringsAsFactors = F)
b1 = convert_data(b1, "Bio121", "post")

a2 = read.csv("CSUDH/BIO 327-01 Fall 2020/Pre-Survey.downloadlong.csv",
              stringsAsFactors = F)
a2 = convert_long_data(a2, "Bio327", "pre")
b2 = read.csv("CSUDH/BIO 327-01 Fall 2020/Post-Survey.download.csv", 
              stringsAsFactors = F)
b2 = convert_data(b2, "Bio327", "post")

a3 = read.csv("CSUDH/BIO 327-02 Fall 2020/Pre-Survey.downloadlong.csv",
              stringsAsFactors = F)
a3 = convert_long_data(a3, "Bio327", "pre")
b3 = read.csv("CSUDH/BIO 327-02 Fall 2020/Post-Survey.download.csv", 
              stringsAsFactors = F)
b3 = convert_data(b3, "Bio327", "post")

a4 = read.csv("CSUDH/BIO 121-01 Spring 2021/Pre-Survey.downloadlong.csv",
              stringsAsFactors = F)
a4 = convert_long_data(a4, "Bio121", "pre")
b4 = read.csv("CSUDH/BIO 121-01 Spring 2021/Post-Survey.downloadlong.csv", 
              stringsAsFactors = F)
b4 = convert_long_data(b4, "Bio121", "post")

a5 = read.csv("CSUDH/BIO 125-02 Spring 2021/Pre-Survey.downloadlong.csv",
              stringsAsFactors = F)
a5 = convert_long_data(a5, "Bio125", "pre")
b5 = read.csv("CSUDH/BIO 125-02 Spring 2021/Post-Survey.downloadlong.csv", 
              stringsAsFactors = F)
b5 = convert_long_data(b5, "Bio125", "post")

a6 = read.csv("CSUDH/BIO 333 Spring 2021/Pre-Survey-TEST2.download.csv",
              stringsAsFactors = F)
a6 = convert_data(a6, "Bio333", "pre")
b6 = read.csv("CSUDH/BIO 333 Spring 2021/Post-Survey-TEST.download.csv", 
              stringsAsFactors = F)
b6 = convert_data(b6, "Bio333", "post")

a7 = read.csv("CSUDH/BIO 123 Spring 2022/Pre-course survey.downloadlong.csv",
              stringsAsFactors = F)
a7 = convert_long_data(a7, "Bio123", "pre")
b7 = read.csv("CSUDH/BIO 123 Spring 2022/Post-Survey-TEST.downloadlong.csv", 
              stringsAsFactors = F)
b7 = convert_long_data(b7, "Bio123", "post")

a8 = read.csv("CSUDH/BIO 221 Fall 21/BIO 221_Pre-Survey-TEST2_Fall21_Heckel.csv",
              stringsAsFactors = F)
a8 = convert_long_data(a8, "Bio221", "pre")
b8 = read.csv("CSUDH/BIO 221 Fall 21/BIO 221_Post-Survey-TEST_Fall21_Heckel.csv", 
              stringsAsFactors = F)
b8 = convert_long_data(b8, "Bio221", "post")

a9 = read.csv("CSUDH/BIO 313 Fall 2021/Pre-Survey-TEST.download.csv",
              stringsAsFactors = F)
a9 = convert_data(a9, "Bio313", "pre")
b9 = read.csv("CSUDH/BIO 313 Fall 2021/Post-Survey-TEST.download.csv", 
              stringsAsFactors = F)
b9 = convert_data(b9, "Bio313", "post")

a10 = read.csv("CSUDH/BIO 331 Fall 2021/Pre-Survey-Learning Lab.download.csv",
               stringsAsFactors = F)
a10 = convert_data(a10, "Bio331", "pre")
b10 = read.csv("CSUDH/BIO 331 Fall 2021/Post-Survey-TEST.download.csv", 
               stringsAsFactors = F)
b10 = convert_data(b10, "Bio331", "post")

a11 = read.csv("CSUDH/BIO 342 Fall 21/BIO 342_Pre-Survey-TEST2_Fall21_Heckel.csv",
               stringsAsFactors = F)
a11 = convert_long_data(a11, "Bio342", "pre")
b11 = read.csv("CSUDH/BIO 342 Fall 21/BIO 342_Post-Survey-TEST_Fall21_Heckel.csv", 
               stringsAsFactors = F)
b11 = convert_long_data(b11, "Bio342", "post")

a12 = read.csv("CSUDH/BIO 342 Spring 2022/Section 01/Pre-Survey.downloadlong.csv",
               stringsAsFactors = F)
a12 = convert_long_data(a12, "Bio342", "pre")
b12 = read.csv("CSUDH/BIO 342 Spring 2022/Section 01/Post-Survey.downloadlong.csv", 
               stringsAsFactors = F)
b12 = convert_long_data(b12, "Bio342", "post")

a13 = read.csv("CSUDH/BIO 421 Fall 2020/Pre-class survey due 8_28 (4 points).download.csv",
               stringsAsFactors = F)
a13 = convert_data(a13, "Bio421", "pre")
b13 = read.csv("CSUDH/BIO 421 Fall 2020/Post-class survey due 12_11 (4 points).download.csv", 
               stringsAsFactors = F)
b13 = convert_data(b13, "Bio421", "post")

dd = rbind(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13,
           b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13
)

# get longform data
# c53 and c55
lf = dd %>% filter(Question_ID %in% c("c53", "c55"))
write.csv(lf, paste0("~/Dropbox (Personal)/research_projects-ACTIVE/CELL-manuscript/data/longform_answers-",
                     Sys.Date(), ".csv"), row.names = F)

# then drop long form data
dd1 = dd %>% filter(!Question_ID %in% c("c53", "c55"))

# make sure all questions were identified
dd1[!complete.cases(dd1$Question_ID), ]

# convert likerts to numbers
# higher better or more
likerts = c("Extensive", "Some", "None", 
            "Strongly disagree", "Disagree",
            "Somewhat disagree", "Neither Agree nor Disagree",
            "Somewhat agree", "Agree", "Strongly agree",
            "Never","one or two times", "monthly",
            "weekly", "Strongly Disagree", "Strongly Agree", "Neutral")
likertvals = c(3, 2, 1, 1, 2, 3, 4, 5, 6, 7, 1, 2, 3, 4, 1, 7, 4)
names(likertvals) = likerts

dd1$likert_val = likertvals[ dd1$answer ]
dd1$Username2 = paste(dd1$Username, dd1$class, sep = "_")

# confirm all likerts were switched
unique(dd1[!complete.cases(dd1$likert_val), "answer"])

y = read.csv("unique_student_IDs.3August23.csv")
dd1$ID = as.character(y[match(dd1$Username, y$bb), "ID"])

# make sure all students identified to ID
unique(dd1[!complete.cases(dd1$ID), "Username"])
# yes

x = readxl::read_xlsx("BIO_GRADES_CA_GRANT_DATA_11.23.22.xlsx")
x1 = x %>% select(EMPLID, SEX, IPEDS_RACEETH, FIRST_GEN, PELL_ELIG, STUD_TYPE)
dd3 = left_join(dd1, x1, by = c("ID" = "EMPLID"))

# check all students identified with demo data
unique(dd3[!complete.cases(dd3$SEX), "Username"])
# four students missing - can't really fix this problem b/c the data 
# don't life with me

write.csv(dd3, paste0("~/Dropbox (Personal)/research_projects-ACTIVE/CELL-manuscript/data/survey_data-",
                      Sys.Date(), ".csv"))
