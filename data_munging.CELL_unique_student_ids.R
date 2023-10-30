rm(list = ls())
library(readxl)

setwd("~/Dropbox (CSUDH)/Grants/CaEd_LearningLab/survey_data/CSUDH/")
dd = list.files(".", recursive = T)

roster = c(19, 22, 23, 28, 33, 38, 42, 45, 
           50, 51, 60, 63, 68, 71, 74 )
dd1 = dd[roster]
ids = list("vector", length(dd1))
for (i in 1:length(dd1)) {
  f = dd1[i]
  if (grepl("xlsx", f)) {
    x = readxl::read_xlsx(f)
  } else if (grepl("csv", f)) {
    x = as_tibble(read.csv(f))
  } else {
    x = readxl::read_xls(f)
  }
  if ("Name" %in% names(x)) {
    x$fullname = x$Name
  } else if ("name" %in% names(x)) {
    x$fullname = paste(x$name, x$name1, sep = ",")
  } else {
    x$fullname = paste(x$`Last Name`, x$`First Name`, sep = ",")
  }
  x$email = pull(x[ , grep("mail", names(x))])
  x = x[ , c("ID", "fullname", "email")]
  x = x[complete.cases(x$ID), ]
  
  x$course = paste(strsplit(dd1[i], '')[[1]][1:7], collapse = "")
  ids[[i]] = x
}

ids2 = do.call("rbind", ids)
# ids3 = ids2[!is.na(ids2)]
# ids4 = data.frame(ID = unique(ids3))
ids3 = ids2 %>% unique()
# ids3$last = tolower(gsub(",.*", "", ids3$fullname))
# ids3$last = gsub("-", "", ids3$last)
# ids3$last = gsub(" ", "", ids3$last)
# ids3$first = tolower(substr(gsub(".*,", "", ids3$fullname), 1, 1))
# ids3$nameid = paste0(ids3$first, ids3$last)
ids3$bb = gsub("@\\S+", "", ids3$email)

write.csv(ids3, "~/Desktop/unique_student_IDs_courses.4August23.csv",
          row.names = F, quote = T)
