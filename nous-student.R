#' ---
#' title: "University student analysis"
#' author: "Anna Quaglieri"
#' date: "28/05/2019"
#' output:
#'   html_document:
#'     fig_caption: yes
#'     number_sections: yes
#'     theme: cosmo
#'     toc: yes
#'     toc_depth: '4'
#'     toc_float: yes
#'   pdf_document:
#'     toc: yes
#'     toc_depth: '4'
#'   github_document:
#'     toc: yes
#'     toc_depth: 4
#' ---
#' 
#' # Setup
#' 
## ----setup,echo = FALSE,message=FALSE, warning=FALSE---------------------
library(tidyverse)
library(Hmisc)
library(MASS)
library(broom)
library(cowplot)
library(MKmisc)
library(plotROC)
library(ggrepel)

knitr::opts_chunk$set(eval = TRUE,echo = TRUE,tidy = TRUE,message = FALSE,include = TRUE,warning = FALSE,cache=TRUE)

#' 
#' 
#' 
## ----read-data-----------------------------------------------------------
assess <- read_csv("anonymisedData/assessments.csv")
course <- read_csv("anonymisedData/courses.csv")
student.assess <- read_csv("anonymisedData/studentAssessment.csv")
student.info <- read_csv("anonymisedData/studentInfo.csv")
student.regio <- read_csv("anonymisedData/studentRegistration.csv")
student.vle <- read_csv("anonymisedData/studentVle.csv")
vle <- read_csv("anonymisedData/vle.csv")

#' 
#' 
## ----describe-data-------------------------------------------------------
table(assess$code_module) # 7 modules

glimpse(assess)
glimpse(course)
glimpse(student.assess)
glimpse(student.info) # students demoraphics + final results for all students
glimpse(student.regio) # student registration details
glimpse(student.vle) # interaction with virtual learning envir
glimpse(vle)

#' 
#' # Data in the cohort
#' 
#' ## Assessments
#' 
#' There are 7 modules (corses) and for each module there can be 3 assessment types (not all present in every module)
#' 
## ----assess-describe-----------------------------------------------------
table(assess$assessment_type,assess$code_module)

#' 
#' ## Students
#' 
## ----modules-per-student-------------------------------------------------
n.modules <- student.info %>%
  group_by(id_student) %>%
  summarise(len.modules = length(code_module))

#' 
#' * There is a total of `r length(unique(student.info$id_student))` students enrolled and a student attended a minimum of `r min(n.modules$len.modules)` to a maximum of `r max(n.modules$len.modules)`. 
#' 
#' * There are demographics data and assessment results for all students that registered in a module. 
#' 
## ----info-check----------------------------------------------------------
sum(!(student.assess$id_student %in% student.info$id_student))
sum(!(student.info$id_student %in% student.assess$id_student))

sum(!(student.info$id_student %in% student.assess$id_student))

sum(!(student.info$id_student %in% student.regio$id_student))

#' 
#' * In theory, if a student also has an unregistration date they should not have a final score for an exam for a module. Indeed, Students who unregistered have `Withdrawal` as the value of the `final_result` column in the `studentInfo.csv` file. However, this is not always the case and I will clean up students with a different `final_result` entry.
#' 
#' * Assessment are submitted throghout the module (different dates) and if a student does not submit an assessment then there is no record. 
#' 
#' * There are `r length(unique(student.assess$id_student[!is.na(student.assess$score)]))` students with at least an assessment available with a valid score. 
#' 
#' * All student with an assessment have demographics available.
#' 
#' * `r length(unique(student.info$id_student[!(student.info$id_student %in% student.assess$id_student)]))` students do not have an assessment available.
#' 
#' * There are `r length(unique(student.info$id_student[!(student.info$id_student %in% student.vle$id_student)]))` students who did not interact with the VLE
#' 
#' * There are `r length(unique(student.assess$id_student[!(student.assess$id_student %in% student.vle$id_student)]))` students with an assessment did not interact with the VLE.
#' 
## ------------------------------------------------------------------------
sum(!(student.info$id_student %in% student.vle$id_student))
sum(!(student.assess$id_student %in% student.vle$id_student))

#' 
#' * A student might have several entries in student.info but not all assignment
#' * This means that if I merge the assignment score with student infos, I will have some studnet.infos entries with NAs for the score. 
#' * Also some students have NAs for their scores in the assessment data
#' 
#' # Merge datasets
#' 
#' ## VLE
#' 
## ----merge-vle-----------------------------------------------------------
vle.combine <- vle %>%
left_join(student.vle) %>%
  dplyr::rename(date.vle = date)

#' 
#' ## Assessment infos
#' 
## ----merge-assessment----------------------------------------------------
# Connect assessment types scores with scores
ass <- student.assess %>%
  left_join(assess)

summary(ass$score)
table(sign(ass$date_submitted),ass$code_module,ass$code_presentation)
table(sign(ass$date_submitted),ass$assessment_type)
ass$date.sign <- sign(ass$date_submitted)
ggplot(ass,aes(x = factor(date.sign), y = score, fill=code_module)) + geom_boxplot()

ass <- ass %>%
  filter(!is.na(score))

#' 
#' * There are `r sum(is.na(ass$score))` NAs assessment scores and `r sum(sign(ass$date_submitted) == "-1")` assignments were submitted before the start of the course. Given the large number and my limited knolwledge about the dataset I will keep them in the study. They might be entry tests. However, I will remove the NAs assessment score. 
#' 
#' * On the online resource it says:
#' date – information about the final submission date of the assessment calculated as the number of days SINCE the start of the module-presentation. The starting date of the presentation has number 0 (zero). How can it be negative??
#' 
#' * Still after filtering this leaves me with `r dim(ass)[1]` assessments across `r length(unique(ass$id_student))` students. 
#' 
#' ## Students demographics
#' 
## ----merge-student-infos-------------------------------------------------
# Now I know which student what id assessment did.
combine <- student.info %>% # code_module , code_pres, id_student, demo, final score.
  left_join(student.regio) %>% 
    left_join(course) 

#' 
#' # Create combined features: assignment score and number of clicks
#' 
## ----create-features-----------------------------------------------------
summary(ass$score)
ass.total <- ass %>%
  group_by(code_module,code_presentation,id_student) %>%
  summarise(comb.score = mean(score))

click.total <- vle.combine %>%
  group_by(code_module,code_presentation,id_student) %>%
  summarise(total.clicks = sum(sum_click))

# Merge assessment and clicks
click.score <- ass.total %>%
  left_join(click.total)

#' 
#' 
#' # Filter students and recode variables
#' 
#' In this first analysis I am interested in understanding what are the factors that influence withdrawal (1 = withdrawl, 0 = PASS/FAIL/Distinction) and Success (1 = PASS/Distinction, 0 = FAIL). The factors influencing withdrawal could be very different from the factors leading to failure, e.g. external factors versus student's capabilities.
#' 
#' I will create a total weigthed score for the assigment and exams scores available in each module. Some students, if they withdrew do not have score for a module so there will be NAs once combining this meta-score with student infos. I also created a total score for the number of clicks in the VLE done in one module in each semester. I will also create a variable with total number of students in each module. 
#' 
#' 
#' ## Unregistered/Registered students
#' 
#' All students who unregistered during the course could only be `Withdrawn` as final result since they left the course.  
#' However, it appears that some students that never `unregistered` still withdrawn from the module. 
#' Also, Some students never registered but still they unregistered or they are in course??
#' 
## ----keep-registered-students--------------------------------------------
combine$Unregistered <- ifelse(is.na(combine$date_unregistration),"in course","unregistered")
combine$Registered <- ifelse(is.na(combine$date_registration),"not registered","registered") 

sum.reg <- combine %>%
  group_by(Registered,Unregistered,final_result) %>%
    summarise(n.students = length(unique(id_student)))

knitr::kable(sum.reg,caption = "Number of students by registration date, unregistration date and final result.")

#' 
#' 
## ----filter-unreg--------------------------------------------------------
combine <- combine %>%
  filter(!(Registered %in% "not registered"),
         !(Unregistered  %in% "unregistered" & Registered %in% "registered" & final_result %in% "Fail")) 

#' 
#' We have now `r length(unique(combine$id_student))` students.
#' 
## ----merge-with-ass-clicks-----------------------------------------------
dim(combine)
combine.feat <- combine %>%
  left_join(click.score)
dim(combine.feat)

#' 
#' 
#' ## Modify some variables: categorical to numeric
#' 
## ----nas-in-variables----------------------------------------------------
nas <- apply(combine.feat,2,function(x) sum(is.na(x)))
nas

#' 
#' 
## ----recode-vars---------------------------------------------------------
table(combine.feat$highest_education)
table(combine.feat$num_of_prev_attempts)

combine.feat <- combine.feat %>%
  mutate(withdraw = ifelse(final_result %in% "Withdrawn",1,0),
         imd_band_num = as.numeric(as.factor(imd_band)),
         highest_education = factor(highest_education, levels=c("No Formal quals","Lower Than A Level", "A Level or Equivalent","HE Qualification","Post Graduate Qualification")),
         num_of_prev_attempts_dich = ifelse(num_of_prev_attempts > 0,">0","0"),
         year = str_remove(code_presentation, pattern = "B|J"),
         semester = str_extract(code_presentation, pattern = "B|J"),
         code_presentation = factor(code_presentation, levels = c("2013B","2014B","2013J","2014J")))

#' 
#' 
#' # Keep one entry per student
#' 
#' * There are students who attempted the same module multiple times and students who attempted different modules. The replicates account for roughly 12% of the entries. To avoid more complicated random intercept models, if the same student attempted the same module twice then I will keep the entry with the highest number of  num_of_prev_attempts because this information should include information about previous attempts. 
#' * I will discard randomly all students who attempted different modules. If these aren't removed, the entries won't be independent and it would require introducing a mixed effect model with random intercept for each subject. The model itself is a logistic model with many independent variables and fitting a mixed model could be quite complicated and the majority of the students anyway only have one entry. 
#' 
## ----study-replicate-students--------------------------------------------
repl <- names(table(combine.feat$id_student)[table(combine.feat$id_student) > 1])
repl.student <- combine.feat[combine.feat$id_student %in% repl,]

repl.student.sum <- repl.student %>%
  group_by(id_student) %>%
  summarise(n.modules = length(unique(code_module)),
            n.results = length(unique(final_result)),
            n.repl = length(code_module))

ggplot(repl.student.sum,aes(x=factor(n.modules),fill = factor(n.results))) + geom_bar(position = "dodge") + facet_wrap(~n.repl) +
  theme_bw()

#' 
#' 
## ----remove-duplicate-students-------------------------------------------
student.to.keep <- combine.feat %>%
  group_by(code_module,id_student) %>%
  summarise(code_presentation = code_presentation[which.max(num_of_prev_attempts)]) %>%
  unite(key.student.module, code_module,id_student,code_presentation,sep="-",remove=FALSE)

combine.feat.unique <- combine.feat %>%
  unite(key.student.module, code_module,id_student,code_presentation,sep="-",remove=FALSE) %>%
  filter(key.student.module %in% student.to.keep$key.student.module )

combine.feat.unique <- combine.feat.unique[!duplicated(combine.feat.unique$id_student),]

nrow(combine.feat.unique)/nrow(combine.feat) # removed 12% of the initial data

#' 
#' 
## ----add-nstudent-per-module---------------------------------------------
# Total number of students attending a course - includign students who attempted different modules
nstudents <- combine.feat %>%
  group_by(code_module,code_presentation) %>%
  summarise(n.students = length(unique(id_student)))

combine.feat.unique <- combine.feat.unique %>%
  left_join(nstudents)

#' 
#' 
## ----table-dataset-unique------------------------------------------------
sum.reg <- combine.feat.unique %>%
  group_by(Registered,Unregistered,final_result) %>%
    summarise(n.students = length(unique(id_student)))
colnames(sum.reg) <- c("Registered","Unregistered","Final result","Number of students")

knitr::kable(sum.reg,caption = "Number of students by registration date, unregistration date and final result.")

#' 
#' 
#' # Combine VLE clicks data with students' results
#' 
#' In 2014 there was an increased activity on the VLe which is probably why it helped improving success. 
#' 
## ----vle-type-frequency--------------------------------------------------
vle.combine.sum <- vle.combine %>%
  group_by(code_module,code_presentation,activity_type,id_student) %>%
  summarise(sum_click = sum(sum_click)) %>%
  unite(key.student.module, code_module,id_student,code_presentation,sep="-",remove=FALSE) %>%
  filter(key.student.module %in% student.to.keep$key.student.module ) %>%
 mutate( year = str_remove(code_presentation, pattern = "B|J"),
         semester = str_extract(code_presentation, pattern = "B|J")) %>%
  left_join(nstudents)

nstudents.year.semester <- combine.feat %>%
  group_by(year,semester) %>%
  summarise(n.students = length(unique(id_student)))

vle.combine.sum.sum <- vle.combine.sum %>%
  group_by(activity_type,year,semester) %>%
  summarise(n.clicks = sum(sum_click)) %>%
  ungroup() %>%
  left_join(nstudents.year.semester) %>%
  mutate(n.clicks.std = n.clicks/n.students) %>%
    mutate(activity_type =  fct_reorder(activity_type,n.clicks.std))

ggplot(vle.combine.sum.sum,aes(x = activity_type, y = n.clicks.std, fill = year)) + geom_bar(stat="identity",position="dodge") +facet_wrap(~semester) + coord_flip() + theme_bw()

#' 
#' * AAA has got very little students
#' 
## ----top5-activities-----------------------------------------------------
vle.combine.top5 <- vle.combine.sum %>%
  filter(activity_type %in% c("oucontent","forumng","homepage","quiz","subpage"))

vle.combine.sum.sum <- vle.combine.top5 %>%
  group_by(activity_type,semester,code_module,n.students) %>%
  summarise(n.clicks = sum(sum_click)/unique(n.students)) %>%
  unite(key , activity_type,semester,code_module,sep="-",remove=FALSE) %>%
  ungroup() %>%
  mutate(activity_type =  fct_reorder(activity_type,n.clicks))


ggplot(vle.combine.sum.sum,aes(x = code_module, y = n.clicks, fill = semester)) + geom_bar(stat="identity",position="dodge",colour="white") +facet_wrap(~activity_type) +  theme_bw() +coord_flip()

#' 
#' 
## ----click-perstudent-topactivity----------------------------------------
# Sum of clicks done by each student per activity type
click.students.activity <- vle.combine.sum %>%
    filter(activity_type %in% c("oucontent","forumng","homepage","quiz","subpage")) %>%
  group_by(code_module,year,semester,activity_type,id_student) %>%
  summarise(n.clicks.act = sum(sum_click))

n.students.vle <- click.students.activity %>%
  group_by(code_module,year,semester) %>%
  summarise(n.students.clicks = length(unique(id_student)))

click.students.activity <- click.students.activity %>%
  left_join(n.students.vle) %>%
  mutate(clicks.per.student = n.clicks.act/n.students.clicks)

#' 
#' 
#' # Describe modules and students attending each module
#' 
#' * Across two year there were two presentations in each year
#' * Module AAA is the one with less student, probably aroun 400. Module CCC in 2014 is the one attracting mode students
#' * There seem to be a semester effect in the number students, with more students enrolling in the J part of the year
#' 
## ----affluence-to-courses, fig.cap="Number of students enrolled in each module across years and presentations. The figure included students who enrolled multiple time (same or different module). "----
sum.students <- combine.feat %>%
  group_by(code_presentation,code_module) %>%
  summarise(n.students = length(unique(id_student))) %>%
  ungroup() %>%
  mutate(year = str_remove(code_presentation, pattern = "B|J"),
         semester = str_extract(code_presentation, pattern = "B|J"),
         code_presentation = factor(code_presentation, levels = c("2013B","2014B","2013J","2014J"))) %>%
  dplyr::rename(`Code presentation` = code_presentation)


# There seem to be a semester effect in the number students, with more students enrolling in the J 
ggplot(sum.students, aes(x = code_module, y = n.students, fill = `Code presentation`)) + 
  geom_bar(stat = "identity",position = "dodge",colour="white") + theme_bw() + ggtitle("Number of students enrolled in each module") + theme(legend.position = "bottom") + labs(x  = "Code of module", y = "Number of student enrolled")

#' 
#' 
## ----affluence-to-courses-year-------------------------------------------
# There doesn't seem to be a year effect
ggplot(sum.students, aes(x = code_module, y = n.students, fill = year)) + 
  facet_wrap(~ semester) +
  geom_bar(stat = "identity",position = "dodge",colour="white") + theme_bw()+ ggtitle("Number of students enrolled in each module")

#' 
#' * The score does not look super discriminant. Am I using it correctly? 
#' 
## ----score-by-finalresul-------------------------------------------------
ggplot(combine.feat.unique, aes(x = comb.score, fill = final_result))  + geom_density() + theme_bw()

#' 
#' # Exploratory: factors that influence dropout and success
#' 
#' We defined the **success rate** as the proportion of students that either passed a module with *Distinction* or *Pass* out of all the students that registered into a module but excluding students who *Withdrawn*. Conversely, we defined the **dropout rate** as the proportion of students that *Withdrawn* from a module out of all the students that registered into any module. The reasons that push a student to dropout from a module could be very different from the reasons that led to failure.
#' 
## ----define-pass-fail----------------------------------------------------
combine.feat.unique <- combine.feat.unique %>%
  mutate(pass_fail = ifelse(final_result %in% c("Fail"),0,1)) %>%
  mutate(pass_fail = ifelse(final_result %in% "Withdrawn",NA,pass_fail))

table(combine.feat.unique$pass_fail,combine.feat.unique$final_result)
table(combine.feat.unique$withdraw,combine.feat.unique$final_result)


#' 
#' 
## ----exploratory-imd_band_num-region-------------------------------------
# Poeple that withdraw tend to have a slightly lower socio economic statis
ggplot(combine.feat.unique,aes(x = factor(withdraw), y = imd_band_num)) + geom_boxplot() + facet_wrap(~semester) + theme_bw()

# Scotland, wales and ireland are the ones with lower levels of widthdrawal
sum.region.with <- combine.feat.unique %>%
  group_by(region) %>%
  summarise(imd.mean = mean(imd_band_num,na.rm = TRUE),
            sem.imd.mean = sd(imd_band_num,na.rm = TRUE)/sqrt(length(imd_band_num[!is.na(imd_band_num)])),
            
            with.mean = mean(withdraw),
            sem.with.mean = sd(withdraw)/sqrt(length(withdraw))) %>%
  mutate(region = fct_reorder(region,with.mean))

ggplot(sum.region.with,aes(x = with.mean, y = imd.mean,colour=region)) + geom_point(stat = "identity")  + theme_bw() +
  geom_errorbar(aes(ymin = imd.mean - sem.imd.mean, 
                    ymax = imd.mean + sem.imd.mean)) +
  geom_errorbarh(aes( xmin = with.mean - sem.with.mean, 
                    xmax = with.mean + sem.with.mean)) +
  geom_label_repel(aes(label = region)) +
  geom_hline(yintercept = mean(combine.feat.unique$imd_band_num,na.rm = TRUE),linetype="dotted") + ylim(c(3.5,7.5)) +
   geom_vline(xintercept = mean(combine.feat.unique$withdraw,na.rm = TRUE),linetype="dotted") 




#' 
#' * I will use module BBB as reference for the module, it's around the mean of withdrawal
#' 
#' * The module with higheest prop of withdrawal are CCC/DDD and the ones with less withdrawal are AAA and GGG. The socio econoic area does not seem to be too heterogenous apart from module AAA.
#' 
## ----exploratory-imd_band_num-module-------------------------------------
sum.region <- combine.feat.unique %>%
  group_by(code_module,semester,year) %>%
  summarise(imd.mean = mean(imd_band_num,na.rm = TRUE),
            sem.imd.mean = sd(imd_band_num,na.rm = TRUE)/sqrt(length(imd_band_num[!is.na(imd_band_num)])),
            
            with.mean = mean(withdraw),
            sem.with.mean = sd(withdraw)/sqrt(length(withdraw))) %>%
  ungroup() %>%
  mutate(code_module = fct_reorder(code_module,with.mean))

ggplot(sum.region,aes(x = with.mean, y = imd.mean,colour=code_module,shape=year)) + geom_point(stat = "identity",size=2)  + theme_bw() +
  geom_errorbar(aes(ymin = imd.mean - sem.imd.mean, 
                    ymax = imd.mean + sem.imd.mean)) +
  geom_errorbarh(aes( xmin = with.mean - sem.with.mean, 
                    xmax = with.mean + sem.with.mean)) +
  geom_label_repel(aes(label = code_module)) + facet_wrap(~semester) +
  geom_hline(yintercept = mean(combine.feat.unique$imd_band_num,na.rm = TRUE),linetype="dotted") + ylim(c(3.5,7.5))+
   geom_vline(xintercept = mean(combine.feat.unique$withdraw,na.rm = TRUE),linetype="dotted") 

#' 
#' 
#' 
## ----exploratory-with-module---------------------------------------------
sum.region <- combine.feat.unique %>%
  group_by(code_module,semester,year) %>%
  summarise(with.mean = mean(withdraw),
            sem.with.mean = sd(withdraw)/sqrt(length(withdraw))) %>%
  ungroup() %>%
  mutate(code_module = fct_reorder(code_module,with.mean))

ggplot(sum.region,aes(x = code_module, y = with.mean,colour=code_module,shape=year)) + geom_point(stat = "identity",size=2)  + theme_bw() +
  geom_errorbar(aes(ymin = with.mean - sem.with.mean, 
                    ymax = with.mean + sem.with.mean),width=0.5) + facet_wrap(~semester) +
  geom_hline(yintercept = mean(combine.feat.unique$withdraw,na.rm = TRUE),linetype="dotted")

#' 
#' ## Socio-economical factors
#' 
#' 
## ----exploratory-withdraw-pass-fail--------------------------------------
# IMD bands 
imd.band <- unique(combine.feat.unique[,c("imd_band","imd_band_num")]) %>%
  filter(!is.na(imd_band)) %>%
  mutate(imd_band = ifelse(imd_band %in% "10-20","10-20%",imd_band),
         imd.mean = imd_band_num)

# Scotland, wales and ireland are the ones with lower levels of widthdrawal
sum.region.pass.fail <- combine.feat.unique %>%
  group_by(region) %>%
  summarise(imd.mean = round(mean(imd_band_num,na.rm = TRUE)),
            sem.imd.mean = sd(imd_band_num,na.rm = TRUE)/sqrt(length(imd_band_num[!is.na(imd_band_num)])),
            
            with.mean = mean(withdraw,na.rm = TRUE),
            sem.with.mean = sd(withdraw,na.rm = TRUE)/sqrt(length(withdraw[!is.na(withdraw)])),
            
            pass.mean = mean(pass_fail,na.rm = TRUE),
            sem.pass.mean = sd(pass_fail,na.rm = TRUE)/sqrt(length(pass_fail[!is.na(pass_fail)]))) %>%
  mutate(region = fct_reorder(region,pass.mean)) %>%
 left_join(imd.band) %>%
  mutate(Region = paste0(region," (IMD",imd_band,")")) %>%
  dplyr::rename(IMD_band = "imd_band")

plot.region =ggplot(sum.region.pass.fail,aes(x = pass.mean, y = with.mean,colour=IMD_band)) + geom_point()  + theme_classic() +
  geom_vline(xintercept = mean(combine.feat.unique$pass_fail,na.rm = TRUE),linetype="dotted",colour="grey") + 
   geom_hline(yintercept = mean(combine.feat.unique$withdraw,na.rm = TRUE),linetype="dotted",colour="grey") + 
  geom_label_repel(aes(label = Region),show.legend = FALSE,size=2)  + labs(x = "Success rate", y = "Dropout rate") + theme(legend.position = "bottom")

#' 
#' * I will use module BBB as reference for the module, it's around the mean of withdrawal
#' 
#' * The module with higheest prop of withdrawal are CCC/DDD and the ones with less withdrawal are AAA and GGG. The socio econoic area does not seem to be too heterogenous apart from module AAA.
#' 
## ----exploratory-module-region-------------------------------------------
sum.module.with <- combine.feat.unique %>%
  group_by(code_module) %>%
  summarise(with.mean = mean(withdraw,na.rm = TRUE),
            sem.with.mean = sd(withdraw,na.rm = TRUE)/sqrt(length(withdraw[!is.na(withdraw)]))) %>%
  ungroup() %>%
  mutate(code_module = fct_reorder(code_module,with.mean))

# Scotland, wales and ireland are the ones with lower levels of widthdrawal
sum.region.pass.fail <- combine.feat.unique %>%
  group_by(code_module) %>%
  summarise(pass.mean = mean(pass_fail,na.rm = TRUE),
            sem.pass.mean = sd(pass_fail,na.rm = TRUE)/sqrt(length(pass_fail[!is.na(pass_fail)])),
            imd.mean = round(mean(imd_band_num,na.rm = TRUE))) %>%
  mutate(code_module = fct_reorder(code_module,pass.mean)) %>%
  left_join(sum.module.with) %>%
   left_join(imd.band) %>%
  mutate(Module = paste0(code_module," (IMD",imd_band,")")) %>%
  dplyr::rename(IMD_band = imd_band)


data.overlay <- data.frame(success.rate = c(mean(combine.feat.unique$pass_fail,na.rm = TRUE),0.2),
                           dropout.rate =  c(0.2,mean(combine.feat.unique$withdraw,na.rm = TRUE)),
                           label= c("Average success rate","Average dropout rate"))

plot.module = ggplot(sum.region.pass.fail,aes(x = pass.mean, y = with.mean,colour=IMD_band)) + geom_point()  + theme_classic() +
  geom_vline(xintercept = mean(combine.feat.unique$pass_fail,na.rm = TRUE),linetype="dotted",colour="grey") + 
   geom_hline(yintercept = mean(combine.feat.unique$withdraw,na.rm = TRUE),linetype="dotted",colour="grey") + 
  geom_label_repel(aes(label = Module),show.legend = FALSE,size=2)  + labs(x = "Success rate", y = "Dropout rate") + theme(legend.position = "bottom") +
  xlim(0.55,0.90) + ylim(0.1,0.45)

#' 
## ----plot-region-module-imb----------------------------------------------
legend <- get_legend(plot.region)
pc1 = plot_grid(plot.region  + ggtitle("Success/dropout rate\nby region") + theme(legend.position = "none") + xlim(0.55,0.90) + ylim(0.1,0.45),
              plot.module + ggtitle("Success/dropout rate\nby module") + theme(legend.position = "none") + xlim(0.55,0.90) + ylim(0.1,0.45),nrow=1)

figure1 = plot_grid(pc1)
figure1

ggsave("output/success-dropout-region-module.pdf",width = 12,height =6)

#' 
#' 
#' ## Success in modules across years
#' 
## ----exploratory-with-module-by-year-------------------------------------
sum.module.with <- combine.feat.unique %>%
  group_by(code_module,semester,year) %>%
  summarise(with.mean = mean(withdraw,na.rm = TRUE),
            sem.with.mean = sd(withdraw,na.rm = TRUE)/sqrt(length(withdraw[!is.na(withdraw)]))) %>%
  ungroup() %>%
  mutate(code_module = fct_reorder(code_module,with.mean))

sum.region.pass.fail <- combine.feat.unique %>%
 group_by(code_module,semester,year) %>%
  summarise(pass.mean = mean(pass_fail,na.rm = TRUE),
            sem.pass.mean = sd(pass_fail,na.rm = TRUE)/sqrt(length(pass_fail[!is.na(pass_fail)]))) %>%
  ungroup() %>%
  mutate(code_module = fct_reorder(code_module,pass.mean)) %>%
  left_join(sum.module.with)


ggplot(sum.region.pass.fail,aes(x = pass.mean, y = with.mean,colour=code_module,shape=year)) + geom_point(stat = "identity",size=2)  + theme_bw() +
  geom_text_repel(aes(label = code_module)) + facet_wrap(~semester) +
  geom_hline(yintercept = mean(combine.feat.unique$withdraw,na.rm = TRUE),linetype="dotted")+
   geom_vline(xintercept = mean(combine.feat.unique$pass_fail,na.rm = TRUE),linetype="dotted")

ggsave("output/dropout-success-module-semester-year.png",width = 10,height = 10)

#' 
#' ## Education
#' 
## ----exploratory-with-education------------------------------------------
# Scotland, wales and ireland are the ones with lower levels of widthdrawal
sum.edu.pass.fail <- combine.feat.unique %>%
  group_by(highest_education) %>%
  summarise(with.mean = mean(withdraw,na.rm = TRUE),
            sem.with.mean = sd(withdraw,na.rm = TRUE)/sqrt(length(withdraw[!is.na(withdraw)])),
            
    pass.mean = mean(pass_fail,na.rm = TRUE),
            sem.pass.mean = sd(pass_fail,na.rm = TRUE)/sqrt(length(pass_fail[!is.na(pass_fail)])),
    
            imd.mean = round(mean(imd_band_num,na.rm = TRUE))) %>%
  mutate(highest_education = fct_reorder(highest_education,pass.mean)) %>%
   left_join(imd.band) %>%
  mutate(Highest_education = paste0(highest_education," (IMD",imd_band,")")) %>%
  dplyr::rename(IMD_band = imd_band)


p4 = ggplot(sum.edu.pass.fail,aes(x = pass.mean, y = with.mean,colour=IMD_band)) + geom_point()  + theme_classic() +
  geom_vline(xintercept = mean(combine.feat.unique$pass_fail,na.rm = TRUE),linetype="dotted",colour="grey") + 
   geom_hline(yintercept = mean(combine.feat.unique$withdraw,na.rm = TRUE),linetype="dotted",colour="grey") + 
  geom_text_repel(aes(label = Highest_education),show.legend = FALSE,size=2)  + labs(x = "Success rate", y = "Dropout rate") + theme(legend.position = "bottom") 

#' 
#' 
#' 
#' ## Lineplots final results and demographics
#' 
#' 
## ----lineplots-final-result,fig.width=6,fig.height=6---------------------
col.props <- data.frame(prop.table(table(combine.feat.unique$final_result,combine.feat.unique$imd_band),margin = 2)) %>%
  dplyr::rename(`Final result` = Var1) %>%
  mutate(`Final result` =  factor(`Final result`, 
                                  levels = c("Distinction","Pass","Fail","Withdrawn"))) %>%
  mutate(Var2 = factor(Var2, levels = c("0-10%","10-20","20-30%","30-40%",
                                        "40-50%","50-60%","60-70%","70-80%",
                                        "80-90%","90-100%"),
         labels=c(c("0-10","10-20","20-30","30-40",
                                        "40-50","50-60","60-70","70-80",
                                        "80-90","90-100"))))

plot.imd =ggplot(col.props,aes(x=Var2,y=Freq*100,colour=`Final result`,group=`Final result`)) + geom_point() + geom_line()+
  theme_bw()  + labs(x = "Band of Index of Multiple Depravation (%)" ,y = "% Students")+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(axis.text.x = element_text(angle = 5, hjust = 0.5,size=7),
         legend.key.size = unit(0.3, "cm"),
  legend.key.width = unit(0.05,"cm") ,
  axis.title = element_text(size=9),
  title = element_text(size=10),
  legend.position = "bottom")


col.props <- data.frame(prop.table(table(combine.feat.unique$highest_education,combine.feat.unique$age_band),margin = 2)) 

p2=ggplot(col.props,aes(x=Var2,y=Freq*100,colour=Var1,group=Var1)) + geom_point() + geom_line()+
  theme_bw() + labs(x = "Age class" ,y = "% Students")+ theme(legend.position = "bottom")

# education
col.props <- data.frame(prop.table(table(combine.feat.unique$final_result,combine.feat.unique$highest_education),margin = 2))%>%
  dplyr::rename(`Final result` = Var1) %>%
  mutate(`Final result` =  factor(`Final result`, 
                                  levels = c("Distinction","Pass","Fail","Withdrawn"))) %>%
  mutate(Var2 = factor(Var2, levels = levels(Var2),
                       labels = c("No formal qual","< Than A","A or equivalent","HE","Post Graduate")))

plot.edu = ggplot(col.props,aes(x=Var2,y=Freq*100,colour=`Final result`,group=`Final result`)) + geom_point() + geom_line()+
  theme_bw() + labs(x = "Highest education" ,y = "% Students")+ theme(legend.position = "bottom") + theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(axis.text.x = element_text(angle = 5, hjust = 0.5,size=7),
         legend.key.size = unit(0.3, "cm"),
  legend.key.width = unit(0.05,"cm") ,
  axis.title = element_text(size=9),
  title = element_text(size=10))

#' 
## ----lineplot-imd-education.pdf------------------------------------------
p = plot_grid(plot.imd + theme(legend.position = "none"),
          plot.edu + theme(legend.position = "none"),nrow=1)
legend <- get_legend(plot.imd)
plot_grid(p,legend,nrow=2,rel_heights = c(5,1))

ggsave("output/imd-education-final-result.pdf",width = 8,height = 3)

#' 
#' 
## ----lineplots-edu-by-imb------------------------------------------------
col.props <- data.frame(prop.table(table(combine.feat.unique$imd_band,combine.feat.unique$highest_education),margin = 2)) %>%
  mutate(Var1 = factor(Var1, levels = c("0-10%","10-20","20-30%","30-40%",
                                        "40-50%","50-60%","60-70%","70-80%",
                                        "80-90%","90-100%"),
         labels=c(c("0-10%","10-20%","20-30%","30-40%",
                                        "40-50%","50-60%","60-70%","70-80%",
                                        "80-90%","90-100%")))) %>%
  dplyr::rename(`IMD band` =Var1) %>%
  mutate(Var2 = factor(Var2, levels = levels(Var2),
                       labels = c("No formal qual","< Than A","A or equivalent","HE","Post Graduate")))

plot.imd.edu <- ggplot(col.props,aes(x=Var2,y=Freq*100,colour=`IMD band`,group=`IMD band`)) + geom_point() + geom_line()+
  theme_bw() + labs(x = "Highest education" ,y = "% Students") + ggtitle("Qualification by IMD band")+ 
  theme(axis.text.x = element_text(angle = 5, hjust = 0.5,size=7),
         legend.key.size = unit(0.3, "cm"),
  legend.key.width = unit(0.05,"cm") ,
  axis.title = element_text(size=9),
  title = element_text(size=10))
  

#' 
#' 
#' ## Gender
#' 
## ----success-dropout-by-gender-------------------------------------------
# Scotland, wales and ireland are the ones with lower levels of widthdrawal
sum.edu.pass.fail <- combine.feat.unique %>%
  group_by(gender) %>%
  summarise(with.mean = mean(withdraw,na.rm = TRUE),
            sem.with.mean = sd(withdraw,na.rm = TRUE)/sqrt(length(withdraw[!is.na(withdraw)])),
            
    pass.mean = mean(pass_fail,na.rm = TRUE),
            sem.pass.mean = sd(pass_fail,na.rm = TRUE)/sqrt(length(pass_fail[!is.na(pass_fail)])),
    
            imd.mean = round(mean(imd_band_num,na.rm = TRUE))) %>%
  mutate(gender = fct_reorder(gender,pass.mean)) %>%
  dplyr::rename(Success = pass.mean,
                Dropout = with.mean) %>%
  dplyr::select(gender,Success,Dropout) %>%
  gather(key = Result, value = Proportion,Success,Dropout)


plot.gender=ggplot(sum.edu.pass.fail,aes(x = gender, y = Proportion,fill=Result)) + geom_bar(stat="identity",position = "dodge")  + theme_classic()+ 
  theme(axis.text.x = element_text(angle = 5, hjust = 0.5,size=7),
         legend.key.size = unit(0.3, "cm"),
  axis.title = element_text(size=9),
  title = element_text(size=10))

#' 
#' 
## ----imd-by-edu-gender---------------------------------------------------
plot_grid(plot.imd.edu,
          plot.gender + ggtitle("Dropout/Success rate by gender"),nrow=1)
ggsave("output/imd-by-education.pdf",width = 8,height = 3)

#' 
#' 
#' ## VLE: dropout/success 
#' 
#' * What's the frequency of final_results among people that clicked and not clicked?
#' 
## ----frequency-table-VLE-final-result,fig.height=2,fig.width=2-----------
combine.feat.clicks <- combine.feat.unique %>%
  left_join(click.students.activity) %>%
  mutate(final_result = factor(final_result, levels = c("Distinction","Pass",
                                                        "Fail","Withdrawn"))) 

freq.results <- combine.feat.clicks %>%
  group_by(id_student,final_result) %>%
  summarise(n.clicks = round(mean(n.clicks.act,na.rm = TRUE))) %>%
  mutate(VLE = ifelse(is.na(n.clicks),"Never used VLE","Used VLE")) %>%
  group_by(VLE,final_result) %>%
  summarise(n = n())



#' 
## ----save-freq-usedVLE-by-result-----------------------------------------
plot.freq.vle = ggplot(freq.results,aes(x = final_result,y = n, fill = VLE)) + geom_bar(stat = "identity",position = "dodge") + theme_classic() +
  geom_text(aes(label = round(n)), hjust=0.5, vjust=-1, size = 2,position=position_dodge(width=0.9)) + labs(x = "Final result", y = "Number of students")+
   theme(axis.text.x = element_text(angle = 0, hjust = 0.5,size=6),
         axis.text.y = element_text(angle = 0, hjust = 0.5,size=6),
         legend.key.size = unit(0.3, "cm"),
         legend.key.width = unit(0.3, "cm"),
         legend.text = element_text(size = 5),
  axis.title = element_text(size=6),
  title = element_text(size=8),legend.position = "top") 

plot.freq.vle

ggsave("output/usedVLE-by-result.pdf",width = 3,height = 3)

#' 
#' 
## ----chi-square-results-vle----------------------------------------------
freq.results <- combine.feat.clicks %>%
  group_by(id_student,final_result) %>%
  summarise(n.clicks = mean(n.clicks.act,na.rm = TRUE)) %>%
  mutate(VLE = ifelse(is.na(n.clicks),"Never used VLE","Used VLE"))

# Chi square
chisq.test(table(freq.results$final_result,freq.results$VLE))

#' 
#' 
## ----combined-clicks-ass,fig.height=3,fig.width=5------------------------
n.final.result <- combine.feat.unique %>%
  group_by(final_result) %>%
  summarise(n = n())

# sum all clicks done for each result and activity type and divide by number of students in each result category
result.clicks <- combine.feat.clicks %>%
  group_by(final_result,activity_type) %>%
  left_join(n.final.result) %>%
  summarise(sum.clicks = round(sum(n.clicks.act,na.rm = TRUE))/unique(n)) %>%
  filter(!is.na(activity_type)) %>%
  ungroup() %>%
  mutate(activity_type =  fct_reorder(activity_type,sum.clicks)) %>%
  mutate(final_result = factor(final_result, levels = c("Distinction","Pass",
                                                        "Fail","Withdrawn"))) %>%
  filter(!(activity_type %in% c("ouwiki","resource")))

top5 = ggplot(result.clicks,aes(x=final_result,y=sum.clicks,fill=activity_type)) + geom_bar(stat="identity",position = "dodge",colour="white") + theme_classic() + theme(legend.position = "bottom") + labs(x = "Final result", y = "Sum of clicks per activity type")+ geom_text(aes(label = round(sum.clicks)), hjust=0.5, vjust=-1, size = 2,position=position_dodge(width=0.9))+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5,size=6),
         axis.text.y = element_text(angle = 0, hjust = 0.5,size=6),
         legend.key.size = unit(0.3, "cm"),
         legend.key.width = unit(0.3, "cm"),
  axis.title = element_text(size=6),
  title = element_text(size=8),legend.position = "bottom") 

#' 
#' 
## ------------------------------------------------------------------------
ggdraw() +
  draw_plot(top5 + theme(legend.position = "bottom"), 0, 0, 1, 1) +
  draw_plot(plot.freq.vle  + 
              theme(legend.position = "top"), x = 0.53, y = 0.55, width = 0.5, height = 0.4,scale = 0.9) +
  draw_plot_label(c("A", "B"), c(0, 0.58), c(1, 0.92), size = 10)

ggsave("output/usedVLE-by-result-top5.pdf",width = 5,height = 4)

#' 
#' 
#' * quiz and outcontent most fruitful
#' 
## ----combined-clicks-ass-logistic----------------------------------------
# correlation
combine.feat.unique.num <- combine.feat.clicks %>%
  spread(key = activity_type, value = n.clicks.act) %>%
  mutate_at(.vars = c("oucontent","quiz","homepage","subpage"),.funs = function(x) log(x)) %>%
  dplyr::select(pass_fail, oucontent,quiz,homepage,subpage,disability, forumng,semester ,year,gender,code_module) %>%
  mutate_all(.funs = function(x) as.numeric(as.factor(x)))

corrplot::corrplot(cor(combine.feat.unique.num,use="pairwise.complete.obs"))

combine.feat.unique.num <- combine.feat.clicks %>%
  spread(key = activity_type, value = n.clicks.act)

apply(combine.feat.unique.num,2,function(x) sum(is.na(x)))

# remove homepage
m <- glm(pass_fail ~  disability + log(oucontent) + semester + year + gender + code_module, data = combine.feat.unique.num, family = binomial)

car::vif(m)

tidym <- tidy(m,conf.int=TRUE) 

tidym <- tidym %>%
  mutate(term = factor(term,levels = term)) %>%
  filter(term != "(Intercept)")

ggplot(tidym,aes(x = term, y = estimate,colour=-log10(p.value))) + geom_point() + geom_errorbar(aes(ymin = conf.low, ymax = conf.high))  +
  theme_bw() + geom_hline(yintercept = 0, linetype = "dotted") + coord_flip() + ggtitle("Clicks associated with success vs failure")

#' 
#' 
## ----exploratory-clicks-activity-module----------------------------------
sum.region <- combine.feat.clicks %>%
  group_by(code_module,semester) %>%
  summarise(clicks.mean = mean(n.clicks.act,na.rm = TRUE),
            sem.clicks.mean = sd(n.clicks.act,na.rm = TRUE)/sqrt(length(n.clicks.act[!is.na(n.clicks.act)])),
            
            with.mean = mean(withdraw),
            sem.with.mean = sd(withdraw)/sqrt(length(withdraw))) %>%
  ungroup() %>%
  mutate(code_module = fct_reorder(code_module,with.mean))

ggplot(sum.region,aes(x = with.mean, y = clicks.mean,colour=code_module)) + geom_point(stat = "identity")  + theme_bw() +
  geom_errorbar(aes(ymin = clicks.mean - sem.clicks.mean, 
                    ymax = clicks.mean + sem.clicks.mean)) +
  geom_errorbarh(aes( xmin = with.mean - sem.with.mean, 
                    xmax = with.mean + sem.with.mean)) +
  geom_label_repel(aes(label = code_module)) + facet_wrap(~semester) + labs(x="Dropout rate",y="Mean of clicks")

#' 
#' 
#' # Logistic on withdral including both B and J together
#' 
#' * East anglian region as reference
#' * BBB as reference module since it is more around the average withdraw/success while AAA is very extreme (high success and lower dropout)
#' * I am going to randomly take one observation from students with multiple courses
#' 
#' ## Combined semester
#' 
## ----very-skewed-vars----------------------------------------------------
# centered on the mean
combine.feat.unique$comb.score.std <- scale(combine.feat.unique$comb.score,center = TRUE,scale = TRUE) 
combine.feat.unique$total.clicks.std <- scale(log(combine.feat.unique$total.clicks),center = TRUE,scale = TRUE)
combine.feat.unique$studied_credits.std <- scale(log(combine.feat.unique$studied_credits),center = TRUE,scale = TRUE)

par(mfrow=c(2,2))
hist(combine.feat.unique$comb.score.std)
hist(combine.feat.unique$total.clicks.std)
hist(combine.feat.unique$studied_credits.std)

# Recode modules
regions <- names(table(combine.feat.unique$region))[!(names(table(combine.feat.unique$region)) %in% "South West Region")]
combine.feat.unique <- combine.feat.unique %>%
  mutate(code_module = factor(code_module, 
                              levels = c("BBB","AAA","CCC","DDD","EEE","FFF","GGG")),
        
          region = factor(region, levels=c("South West Region",regions)))

#' 
#' 
#' * Very bad collinearity
#' 
## ----logistic1-withdraw--------------------------------------------------
# No imd
m <- glm(withdraw ~ gender + region + highest_education + studied_credits.std + disability +
    module_presentation_length + code_module + semester + year + total.clicks.std + n.students + num_of_prev_attempts_dich + comb.score.std, data = combine.feat.unique, family = binomial)

car::vif(m)

#' 
#' A few independent variables show quite high VIF which is symptoms of  multicollinearity. This makes sense because the number of students and length of a course are correlated with the module. 
#' 
## ----nstudents-by-module-------------------------------------------------
ggplot(combine.feat.unique,aes(x= code_module, y = n.students,fill=semester)) + geom_bar(stat = "identity",position = "dodge") + theme_bw()

#' 
#' *  Remove `n.students` and `module_presentation_length`
#' 
## ----logistic2-withdraw--------------------------------------------------
m <- glm(withdraw ~ gender + region + highest_education + studied_credits.std + disability + code_module + semester + year + total.clicks.std +  num_of_prev_attempts_dich + comb.score.std + imd_band_num, data = combine.feat.unique, family = binomial)

car::vif(m) # looks better

summary(m)

tidym.combined <- tidy(m,conf.int=TRUE) %>%
  mutate(term = factor(term,levels = term)) %>%
  filter(term != "(Intercept)") %>%
  mutate(OR = exp(estimate))

#' 
#' 
## ----result-combine-logistic-wothdrawal----------------------------------
knitr::kable(tidym.combined)

#' 
#' 
## ----plot-logistic-with--------------------------------------------------
ggplot(tidym.combined,aes(x = term, y = estimate)) + geom_point() + geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
  theme_bw() + geom_hline(yintercept = 0, linetype = "dotted") + coord_flip() + ggtitle("Factors associated with withdrawal")

#' 
#' 
#' ### Diagnostics and prediction
#' 
## ----check-predictions---------------------------------------------------
aug1 <- cbind(combine.feat.unique, predict(m, newdata = combine.feat.unique, type = "link",
    se = TRUE))
pairs(aug1[,c("fit","total.clicks.std","studied_credits.std")])


aug1 <- cbind(combine.feat.unique, predict(m, newdata = combine.feat.unique, type = "response",
    se = TRUE))
ggplot(aug1,aes(x = fit , fill = factor(withdraw))) + geom_histogram(alpha=0.5)+ theme_bw()

# Chisquare t-test
HLgof.test(fit = aug1$fit[!is.na(aug1$fit)], 
           obs = aug1$withdraw[!is.na(aug1$fit)])


# Roc curve
aug1 <- cbind(combine.feat.unique, predict(m, newdata = combine.feat.unique, type = "response",
    se = TRUE))
roc.log <- ggplot(aug1, aes(d = withdraw, m = fit)) + geom_roc() + theme_bw()
roc.log + 
  ggtitle("Withdrawal - combined semesters") + 
  annotate("text", x = .75, y = .25, 
           label = paste("AUC =", round(calc_auc(roc.log)$AUC, 2))) +
  geom_abline(sslope = 1, intercept = 0, linetype = "dotted") +
  scale_x_continuous("1 - Specificity", breaks = seq(0, 1, by = .1))

#' 
#' 
#' # Logistic on success including both B and J together
#' 
#' ## Combined semester
#' 
## ----logistic-success----------------------------------------------------
m <- glm(pass_fail ~ gender + region + highest_education + studied_credits.std + disability + code_module + semester + year + total.clicks.std +  num_of_prev_attempts_dich + comb.score.std + imd_band_num, data = combine.feat.unique, family = binomial)

car::vif(m) # looks better

summary(m)

tidym.combined <- tidy(m,conf.int=TRUE) %>%
  mutate(term = factor(term,levels = term)) %>%
  filter(term != "(Intercept)")

#' 
#' 
## ----plot-logistic-success-----------------------------------------------
ggplot(tidym.combined,aes(x = term, y = estimate)) + geom_point() + geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
  theme_bw() + geom_hline(yintercept = 0, linetype = "dotted") + coord_flip() + ggtitle("Factors associated with withdrawal")

#' 
#' 
#' ## Diagnostics and prediction
#' 
## ----check-predictions-success-------------------------------------------
aug1 <- cbind(combine.feat.unique, predict(m, newdata = combine.feat.unique, type = "link",
    se = TRUE))
pairs(aug1[,c("fit","total.clicks.std","studied_credits.std")])


aug1 <- cbind(combine.feat.unique, predict(m, newdata = combine.feat.unique, type = "response",
    se = TRUE))
ggplot(aug1,aes(x = fit , fill = factor(withdraw))) + geom_histogram(alpha=0.5)+ theme_bw()

# Chisquare t-test
HLgof.test(fit = aug1$fit[!is.na(aug1$fit)], 
           obs = aug1$withdraw[!is.na(aug1$fit)])


# Roc curve
aug1 <- cbind(combine.feat.unique, predict(m, newdata = combine.feat.unique, type = "response",
    se = TRUE))
roc.log <- ggplot(aug1, aes(d = withdraw, m = fit)) + geom_roc() + theme_bw()
roc.log + 
  ggtitle("Withdrawal - combined semesters") + 
  annotate("text", x = .75, y = .25, 
           label = paste("AUC =", round(calc_auc(roc.log)$AUC, 2))) +
  geom_abline(sslope = 1, intercept = 0, linetype = "dotted") +
  scale_x_continuous("1 - Specificity", breaks = seq(0, 1, by = .1))

#' 
#' 
#' # Withdrawal by semester
#' 
#' ## Semester B
#' 
#' * Remove regions, it was adding coplexity to the model but not really making that much of a difference. Should probably test it.
#' 
## ------------------------------------------------------------------------
semB <- subset(combine.feat.unique, semester %in% "B")
m <- glm(withdraw ~ gender + region + highest_education + studied_credits.std + disability + code_module + year + total.clicks.std +  num_of_prev_attempts_dich + comb.score.std + imd_band_num, data = semB, family = binomial)

car::vif(m) # oooks better

summary(m)

tidyB <- tidy(m,conf.int=TRUE) %>%
  mutate(term = factor(term,levels = term)) %>%
  filter(term != "(Intercept)")

ggplot(tidyB,aes(x = term, y = estimate)) + geom_point() + geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
  theme_bw() + geom_hline(yintercept = 0, linetype = "dotted") + coord_flip() + ggtitle("Factors associated with withdrawal")

#' 
## ------------------------------------------------------------------------
aug1 <- cbind(semB, predict(m, newdata = semB, type = "response",
    se = TRUE))

ggplot(aug1,aes(x = fit , fill = factor(withdraw))) + geom_histogram(alpha=0.5)+ theme_bw()

HLgof.test(fit = aug1$fit[!is.na(aug1$fit)], 
           obs = aug1$withdraw[!is.na(aug1$fit)])

#' 
#' ## Semester J
#' 
## ------------------------------------------------------------------------
semJ <- subset(combine.feat.unique, semester %in% "J")
m <- glm(withdraw ~ gender + region + highest_education + studied_credits.std + disability + code_module + year + total.clicks.std +  num_of_prev_attempts_dich + comb.score.std + imd_band_num, data = semJ, family = binomial)

car::vif(m) # oooks better

summary(m)

tidyJ <- tidy(m,conf.int=TRUE) %>%
  mutate(term = factor(term,levels = term)) %>%
  filter(term != "(Intercept)")

ggplot(tidyJ,aes(x = term, y = estimate)) + geom_point() + geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
  theme_bw() + geom_hline(yintercept = 0, linetype = "dotted") + coord_flip() + ggtitle("Factors associated with withdrawal - J")

#' 
## ------------------------------------------------------------------------
aug1 <- cbind(semJ, predict(m, newdata = semJ, type = "response",
    se = TRUE))

ggplot(aug1,aes(x = fit , fill = factor(withdraw))) + geom_histogram(alpha=0.5)+ theme_bw()

HLgof.test(fit = aug1$fit[!is.na(aug1$fit)], 
           obs = aug1$withdraw[!is.na(aug1$fit)])

#' 
#' 
#' ## Compare B and J: difference of estimates
#' 
## ----blue-diff-betas-----------------------------------------------------
tidyB <- tidyB %>%
  mutate(semester = "B")

tidyJ <- tidyJ %>%
  mutate(semester = "J")

# Differences of betas
tidy.compare.beta <- bind_rows(tidyB,tidyJ) %>%
  dplyr::select(term,estimate,semester) %>%
  spread(key = semester, value = estimate)  %>%
  mutate(diff.estimate = B - J)

# Uncertainty of difference
tidy.compare.std <- bind_rows(tidyB,tidyJ) %>%
  dplyr::select(term,std.error,semester) %>%
  spread(key = semester, value = std.error )  %>%
  group_by(term) %>%
  mutate(diff.std = sqrt(B^2 + J^2)) %>%
  dplyr::select(-B,-J)

# Combine and CI
tidy.compare <- tidy.compare.beta %>%
  left_join(tidy.compare.std) %>%
  mutate(diff.estimate =  diff.estimate/diff.std) %>%
  mutate(conf.low = diff.estimate - 1.96*diff.std,
         conf.high = diff.estimate + 1.96*diff.std) 

ggplot(tidy.compare,aes(x=term,y=diff.estimate)) + geom_point() +
  geom_errorbar(aes(ymin = conf.low,ymax= conf.high),width=0.5) + theme_bw() + geom_hline(yintercept = 0,linetype="dotted") + coord_flip() + labs(y = "B - J") + ggtitle("Differences between semesters")

#' 
#' 
#' # Linear model on score - not used
#' 
#' Now I will study what factors influence the total score obtained with assignments and exams.
#' 
#' It is slightly skewed. 
#' 
## ---- eval=FALSE---------------------------------------------------------
## hist(combine.feat.unique$comb.score)

#' 
#' ## Combined by semester
#' 
## ---- eval=FALSE---------------------------------------------------------
## m <- lm(comb.score ~ gender + region + highest_education + studied_credits.std + disability + code_module + semester + year + total.clicks.std +  num_of_prev_attempts_dich + final_result, data = combine.feat.unique)
## 
## car::vif(m)
## 
## par(mfrow=c(2,2))
## plot(m)
## 

#' 
#' * diagnostics
#' 
## ---- eval=FALSE---------------------------------------------------------
## library(MASS)
## 
## qqnorm(combine.feat.unique$comb.score,
##        ylab="Quantiles for comb.score")
## qqline(combine.feat.unique$comb.score,
##        col="red")
## 
## Box = boxcox(combine.feat.unique$comb.score + 1 ~ 1,              # Transform Turbidity as a single vector
##              lambda = seq(-6,6,0.1))
## 
## Cox = data.frame(Box$x, Box$y)            # Create a data frame with the results
## 
## Cox2 = Cox[with(Cox, order(-Cox$Box.y)),] # Order the new data frame by decreasing y
## 
## Cox2[1,]                                  # Display the lambda with the greatest
##                                           #    log likelihood
## 
## lambda = Cox2[1, "Box.x"]                 # Extract that lambda
## 
## combine.feat.unique$comb.score.box = (combine.feat.unique$comb.score ^ lambda - 1)/lambda   # Transform the original data
## 
## combine.feat.unique$comb.score.box.st <- scale(combine.feat.unique$comb.score.box,center = TRUE,scale = TRUE)
## 
## qqnorm(combine.feat.unique$comb.score.box,
##        ylab="Quantiles for comb.score")
## qqline(combine.feat.unique$comb.score.box,
##        col="red")
## 
## 

#' 
#' 
#' 
## ---- eval=FALSE---------------------------------------------------------
## m <- lm(comb.score.std ~ gender + region + highest_education + studied_credits.std + disability + code_module + semester + year + total.clicks.std +  num_of_prev_attempts_dich, data = combine.feat.unique)
## 
## par(mfrow=c(2,4))
## plot(m)
## 
## # After box-cox
## m <- lm(comb.score.box.st ~ gender + region + highest_education + studied_credits.std + disability + code_module + semester + year + total.clicks.std +  num_of_prev_attempts_dich, data = combine.feat.unique)
## 
## plot(combine.feat.unique$comb.score.box.st,combine.feat.unique$comb.score)
## 
## plot(m)
## 
## car::vif(m)
## 
## 
## tidym <- tidy(m,conf.int=TRUE)
## 
## tidym <- tidym %>%
##   mutate(term = factor(term,levels = term)) %>%
##   filter(term != "(Intercept)")
## 
## ggplot(tidym,aes(x = term, y = estimate,colour=-log10(p.value))) + geom_point() + geom_errorbar(aes(ymin = conf.low, ymax = conf.high))  +
##   theme_bw() + geom_hline(yintercept = 0, linetype = "dotted") + coord_flip() + ggtitle("Factors associated with total score")

#' 
#' 
#' ## Semester B
#' 
## ----semesterB-total-score, eval=FALSE-----------------------------------
## m <- lm(comb.score.std ~ gender + region + highest_education + studied_credits.std + disability + code_module + year + total.clicks.std +  num_of_prev_attempts_dich, data = combine.feat.unique[combine.feat.unique$semester %in% "B",])
## 
## par(mfrow=c(2,4))
## plot(m)
## 
## # After box-cox
## m <- lm(comb.score.box.st ~ gender + region + highest_education + studied_credits.std + disability + code_module  + year + total.clicks.std +  num_of_prev_attempts_dich, data = combine.feat.unique[combine.feat.unique$semester %in% "B",])
## 
## par(mfrow=c(2,2))
## plot(m)
## 
## car::vif(m)

#' 
#' * From the plot of residuals and quite high correlation between the y variable and the residuals it looks like we are not explaining that much and indeed the Rsquared of the model is just 0.2.  
#' 
## ----residuals-B-sem, eval=FALSE-----------------------------------------
## aug <- augment(m)
## 
## pairs(aug[,c(".resid",".fitted","total.clicks.std",".std.resid","studied_credits.std","comb.score.box.st")])
## 
## ggplot(aug,aes(x = code_module,y=.resid,fill=region)) + geom_boxplot()

#' 
#' 
## ---- eval=FALSE---------------------------------------------------------
## # After box-cox
## m <- lm(comb.score.box.st ~ gender + highest_education + total.clicks.std +  num_of_prev_attempts_dich, data = combine.feat.unique[combine.feat.unique$semester %in% "B",])
## 
## summary(m)
## 
## par(mfrow=c(2,2))
## plot(m)
## 
## aug <- augment(m)
## 
## pairs(aug[,c(".resid",".fitted","total.clicks.std",".std.resid","comb.score.box.st")])

#' 
#' ## Semester J
#' 
## ----semesterJ-total-score, eval=FALSE-----------------------------------
## m <- lm(comb.score.std ~ gender + region + highest_education + studied_credits.std + disability + code_module + year + total.clicks.std +  num_of_prev_attempts_dich, data = combine.feat.unique[combine.feat.unique$semester %in% "J",])
## 
## summary(m)
## 
## par(mfrow=c(2,2))
## plot(m)
## 
## # After box-cox
## m <- lm(comb.score.box.st ~ gender + highest_education + studied_credits.std + disability + code_module  + year + total.clicks.std +  num_of_prev_attempts_dich, data = combine.feat.unique[combine.feat.unique$semester %in% "J",])
## 
## par(mfrow=c(2,2))
## plot(m)
## 
## summary(m)
## 
## car::vif(m)

#' 
#' * Same thing for this semester 
#' 
## ----residuals-J-sem, eval=FALSE-----------------------------------------
## aug <- augment(m)
## 
## pairs(aug[,c(".resid",".fitted","total.clicks.std",".std.resid","studied_credits.std","comb.score.box.st")])

#' 
#' 
#' # Purl
#' 
## ----eval=FALSE----------------------------------------------------------
## knitr::purl("nous-student.Rmd", output = "nous-student.R", documentation = 2)

#' 
#' 
