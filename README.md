---
title: "tech_cri"
output:
  word_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Load in data
```{r echo=FALSE, warning=FALSE}
setwd("T:/CRI_Research/telehealth_evaluation/data_codebooks/satisfaction")
tech_cri_dat = read.csv("TelehealthSnapMDZoom_DATA_2020-05-20_0453.csv", header = TRUE, na.strings = c(""))
tech_cri_dat = tech_cri_dat[-c(1:6),]

tech_cri_dat_complete  = subset(tech_cri_dat, my_first_instrument_timestamp != "[not completed]")
```
Check missingness, create data sets, and n's
1, Telephone only | 2, Zoom video and audio | 3, Zoom audio only | 4, SnapMD video and audio | 5, SnapMD audio only | 6, I do not provide televideo or teleaudio services

6, I do not provide televideo or teleaudio services not included in clincical data set.

1, Indiana | 2, Florida | 3, Tennessee | 4, Illinois | 5, Another state
```{r}
library(naniar)
library(descr)
library(gt)
library(ggplot2)
library(ggrepel)
library(webshot)
library(prettyR)
library(dplyr)
miss_var_summary(tech_cri_dat_complete)
dim(tech_cri_dat_complete)
dim(tech_cri_dat)

describe.factor(tech_cri_dat_complete$state)
tech_cri_dat_complete$state = ifelse(tech_cri_dat_complete$state == 1, "Indiana", ifelse(tech_cri_dat_complete$state == 2, "Florida", ifelse(tech_cri_dat_complete$state == 3, "Tennessee", ifelse(tech_cri_dat_complete$state == 4, "Illinois", ifelse(tech_cri_dat_complete$state == 5, "Another state", "Wrong")))))
tech_cri_dat_complete$state = as.factor(tech_cri_dat_complete$state)
tech_cri_dat_complete$state = factor(tech_cri_dat_complete$state, levels = c("Tennessee", "Indiana", "Illinois", "Florida", "Another state"))
describe.factor(tech_cri_dat_complete$state)

### Clinician survey data 
clincian_survey_dat = subset(tech_cri_dat_complete, job_title != 3)
clincian_survey_dat = subset(clincian_survey_dat, service_provided___6 != 1)

n_clinician_survey = dim(clincian_survey_dat)[1]
n_survey = dim(tech_cri_dat_complete)[1]
n_survey

### For barriers and faciliators to Zoom and SnapMD need the number of people who used each.  This means selecting 
#1, Telephone only | 2, Zoom video and audio | 3, Zoom audio only | 4, SnapMD video and audio | 5, SnapMD audio only | 6, I do not provide televideo or teleaudio services
### SnapMD
n_snap_md = ifelse(clincian_survey_dat$service_provided___4 == 1,1, ifelse(clincian_survey_dat$service_provided___5 == 1, 1, 0))
n_snap_md = sum(n_snap_md)
### Zoom


n_zoom = ifelse(clincian_survey_dat$service_provided___2 == 1,1, ifelse(clincian_survey_dat$service_provided___3 == 1, 1, 0))
n_zoom = sum(n_zoom)

n_clinician_survey 
#### Need if you did not select SnapMD
n_no_snap_md = ifelse(clincian_survey_dat$service_provided___4 != 1 & clincian_survey_dat$service_provided___5 != 1, 1, 0)
n_no_snap_md = sum(n_no_snap_md)

### Check you are adding correctly 
n_no_snap_md + n_snap_md == n_clinician_survey
#### Need if you did not select Zoom
n_no_zoom = ifelse(clincian_survey_dat$service_provided___2 != 1 & clincian_survey_dat$service_provided___3 != 1, 1, 0)
n_no_zoom = sum(n_no_zoom)

n_zoom+n_no_zoom == n_clinician_survey
```
Job title by state
```{r}
job_title_dat = na.omit(data.frame(job_title = tech_cri_dat_complete$job_title, state = tech_cri_dat_complete$state))
n_job_title = dim(job_title_dat)[1]

n_state_job_title = describe.factor(job_title_dat$state, decr.order = FALSE)
n_state_job_title = data.frame(n_state_job_title)
n_state_job_title = n_state_job_title[1,]
n_state_job_title

job_title_dat$job_title = recode(job_title_dat$job_title, "1" = "Client Facing", "2" = "Client Facing Medical Provider","3" = "Non Client Facing")
job_title_dat = job_title_dat%>% group_by(state) %>% count(job_title)

job_title_dat$percent = ifelse(job_title_dat$state == "Indiana", job_title_dat$n / n_state_job_title$Indiana, ifelse(job_title_dat$state == "Tennessee", job_title_dat$n / n_state_job_title$Tennessee, ifelse(job_title_dat$state == "Illinois", job_title_dat$n / n_state_job_title$Illinois, ifelse(job_title_dat$state == "Florida", job_title_dat$n / n_state_job_title$Florida, job_title_dat$n / n_state_job_title$Another.state))))

job_title_dat$state = ifelse(job_title_dat$state == "Indiana", paste0(job_title_dat$state, " ", "n=", n_state_job_title$Indiana) , ifelse(job_title_dat$state == "Tennessee", paste0(job_title_dat$state, " ", "n=", n_state_job_title$Tennessee), ifelse(job_title_dat$state == "Illinois", paste0(job_title_dat$state, " ", "n=", n_state_job_title$Illinois), ifelse(job_title_dat$state == "Florida", paste0(job_title_dat$state, " ", "n=", n_state_job_title$Florida), paste0(job_title_dat$state, " ", "n=", n_state_job_title$Another.state)))))

job_title_dat$state = as.factor(job_title_dat$state)

job_title_dat$state = factor(job_title_dat$state, levels = levels(job_title_dat$state)[5:1])

job_title_dat$percent = round(job_title_dat$percent, 2)*100
job_title_dat$percent = paste0(job_title_dat$percent, "%")
title_job_title = paste0("What work group are you in?", " ", "n=", n_job_title)


plot_job_title = ggplot(job_title_dat, aes(x = job_title,y =n, fill = state))+
  geom_bar(stat = "identity", position = "dodge2")+
  labs(title=title_job_title, y = "Count", x = "Response option")+
  scale_y_continuous(limits = c(0,700))+
  geom_text(aes(label = job_title_dat$percent), position=position_dodge(width=0.9), vjust=-0.25)+
  labs(fill = "State")
plot_job_title

```
Job title overall
Keep state in the data so the N's are consistent above

440+486+238+15	
```{r}
job_title_overall_dat = na.omit(data.frame(job_title = tech_cri_dat_complete$job_title, state = tech_cri_dat_complete$state))
n_job_title_overall = dim(job_title_overall_dat)[1]



job_title_overall_dat$job_title = recode(job_title_overall_dat$job_title, "1" = "Client Facing", "2" = "Client Facing Medical Provider","3" = "Non Client Facing")
job_title_overall_dat = job_title_overall_dat %>% count(job_title)
job_title_overall_dat$percent = as.numeric(job_title_overall_dat$n / n_job_title_overall)

job_title_overall_dat$percent = round(job_title_overall_dat$percent, 2)*100
job_title_overall_dat$percent = paste0(job_title_overall_dat$percent, "%")
title_job_title_overall = paste0("What work group are you in?", " ", "n=", n_job_title_overall)


plot_job_title_overall = ggplot(job_title_overall_dat, aes(x = job_title,y =n, fill = job_title))+
  geom_bar(stat = "identity", position = "dodge2")+
  labs(title=title_job_title_overall, y = "Count", x = "Response option")+
  scale_y_continuous(limits = c(0,2000))+
  geom_text(aes(label = percent), position=position_dodge(width=0.9), vjust=-0.25)+
  theme(legend.position = "none")
plot_job_title_overall

```


### Run this each time need n_why for later
Graph of situation
1, I am working from home | 2, I am working from a Centerstone office | 3, I am working both from home some days and at a Centerstone office some days | 4, I am not working at all (On Leave or Cannot Work From Home or Office)
```{r echo=FALSE}
#############################
situation_dat = na.omit(data.frame(situation = tech_cri_dat_complete$situation, state = tech_cri_dat_complete$state))
n_situation = dim(situation_dat)[1]

n_state_situation = describe.factor(situation_dat$state, decr.order = FALSE)
n_state_situation = data.frame(n_state_situation)
n_state_situation = n_state_situation[1,]
n_state_situation

situation_dat = situation_dat%>% group_by(state) %>% count(situation)

situation_dat$percent = ifelse(situation_dat$state == "Indiana", situation_dat$n / n_state_situation$Indiana, ifelse(situation_dat$state == "Tennessee", situation_dat$n / n_state_situation$Tennessee, ifelse(situation_dat$state == "Illinois", situation_dat$n / n_state_situation$Illinois, ifelse(situation_dat$state == "Florida", situation_dat$n / n_state_situation$Florida, situation_dat$n / n_state_situation$Another.state))))

situation_dat$state = ifelse(situation_dat$state == "Indiana", paste0(situation_dat$state, " ", "n=", n_state_situation$Indiana) , ifelse(situation_dat$state == "Tennessee", paste0(situation_dat$state, " ", "n=", n_state_situation$Tennessee), ifelse(situation_dat$state == "Illinois", paste0(situation_dat$state, " ", "n=", n_state_situation$Illinois), ifelse(situation_dat$state == "Florida", paste0(situation_dat$state, " ", "n=", n_state_situation$Florida), paste0(situation_dat$state, " ", "n=", n_state_situation$Another.state)))))

situation_dat$state = as.factor(situation_dat$state)

situation_dat$state = factor(situation_dat$state, levels = levels(situation_dat$state)[5:1])

situation_dat$percent = round(situation_dat$percent, 2)*100
situation_dat$percent = paste0(situation_dat$percent, "%")
situation_dat$situation = recode(situation_dat$situation, "1"= "Working from home","2"= "Centerstone office","3"= "Working from home\n and Centerstone office", "4" = "Not working")

title_situation = paste0("Please choose the option that best describes your situation.", " ", "n=", n_situation)
plot_situation = ggplot(situation_dat, aes(x = situation,y =n, fill = state))+
  geom_bar(stat = "identity", position = "dodge2")+
  labs(title=title_situation, y = "Count", x = "Response option")+
  scale_y_continuous(limits = c(0,1000))+
  geom_text(aes(label = situation_dat$percent), position=position_dodge(width=0.9), vjust=-0.25)+
  labs(fill = "State")
plot_situation

```
Situation overall
```{r}
situation_overall_dat = na.omit(data.frame(situation = tech_cri_dat_complete$situation, state = tech_cri_dat_complete$state))
n_situation_overall = dim(situation_overall_dat)[1]


situation_overall_dat$situation = recode(situation_overall_dat$situation, "1"= "Working from home","2"= "Centerstone office","3"= "Working from home\n and Centerstone office", "4" = "Not working")

situation_overall_dat = situation_overall_dat %>% count(situation)
situation_overall_dat$percent = as.numeric(situation_overall_dat$n / n_situation_overall)

situation_overall_dat$percent = round(situation_overall_dat$percent, 2)*100
situation_overall_dat$percent = paste0(situation_overall_dat$percent, "%")
title_situation_overall = paste0("What work group are you in?", " ", "n=", n_situation_overall)
write.csv(situation_overall_dat, "situation_overall_dat.csv", row.names = FALSE)

plot_situation_overall = ggplot(situation_overall_dat, aes(x = situation,y =n, fill = situation))+
  geom_bar(stat = "identity", position = "dodge2")+
  labs(title=title_situation_overall, y = "Count", x = "Response option")+
  scale_y_continuous(limits = c(0,2000))+
  geom_text(aes(label = percent), position=position_dodge(width=0.9), vjust=-0.25)+
  theme(legend.position = "none")
plot_situation_overall


```


Home productivity

1, I have no barriers and am working productively 
2, I have poor internet/connection 
3, I lack enabling technology equipment (Examples: monitor, headset, webcam, phone) 
4, My workspace is not ideal (lacks space, lacks privacy, inadequate furnishing) 
5, Other

"1"= "Working from home","2"= "Centerstone office","3"= "Working from home\n and Centerstone office", "4" = "Not working"


```{r echo=FALSE}
home_productive_dat = subset(tech_cri_dat_complete, situation == 1)
home_productive_dat = na.omit(data.frame(home_productive_dat[,6:10], state = home_productive_dat$state))


n_state_home_productive = describe.factor(home_productive_dat$state, decr.order = FALSE)
n_state_home_productive = data.frame(n_state_home_productive)
n_state_home_productive = n_state_home_productive[1,]
n_state_home_productive
home_productive_dat = reshape(home_productive_dat, varying = list(c("home_productivity___1", "home_productivity___2", "home_productivity___3", "home_productivity___4", "home_productivity___5")), times = c(1,2,3,4,5), direction = "long")
home_productive_dat


colnames(home_productive_dat)[2:3] = c("type_productive", "home_productive")
home_productive_dat$type_productive = as.factor(home_productive_dat$type_productive)

home_productive_dat = home_productive_dat%>% group_by(state, type_productive) %>% 
  count(home_productive)
home_productive_dat

home_productive_dat = subset(home_productive_dat, home_productive == 1)
home_productive_dat$home_productive = NULL
home_productive_dat
home_productive_dat$percent = ifelse(home_productive_dat$state == "Indiana", home_productive_dat$n / n_state_home_productive$Indiana, ifelse(home_productive_dat$state == "Tennessee", home_productive_dat$n / n_state_home_productive$Tennessee, ifelse(home_productive_dat$state == "Illinois", home_productive_dat$n / n_state_home_productive$Illinois, ifelse(home_productive_dat$state == "Florida", home_productive_dat$n / n_state_home_productive$Florida, home_productive_dat$n / n_state_home_productive$Another.state))))
home_productive_dat$percent = round(home_productive_dat$percent, 2)*100
home_productive_dat$percent = paste0(home_productive_dat$percent, "%")
home_productive_dat$type_productive = recode(home_productive_dat$type_productive, "1"= "I have no barriers and am working productively","2"= "I have poor internet/connection","3"= "I lack enabling technology equipment", "4" = "My workspace is not ideal", "5" = "Other")
home_productive_dat

home_productive_dat$state = ifelse(home_productive_dat$state == "Indiana", paste0(home_productive_dat$state, " ", "n=", n_state_home_productive$Indiana) , ifelse(home_productive_dat$state == "Tennessee", paste0(home_productive_dat$state, " ", "n=", n_state_home_productive$Tennessee), ifelse(home_productive_dat$state == "Illinois", paste0(home_productive_dat$state, " ", "n=", n_state_home_productive$Illinois), ifelse(home_productive_dat$state == "Florida", paste0(home_productive_dat$state, " ", "n=", n_state_home_productive$Florida), paste0(home_productive_dat$state, " ", "n=", n_state_home_productive$Another.state)))))
home_productive_dat
title_home_productive = paste0("What barriers do you have to working from home at full productivity?")

home_productive_dat = home_productive_dat%>% group_by(state)

table_home_productive = 
  gt(home_productive_dat) %>%
  tab_header(title = title_home_productive)%>%
  tab_footnote(footnote = "Respondents can select all that apply so count / percent can add up to more / less than total n / 100%.  N is the total number who completed the survey according to REDCap, did not have missing data in any of the response options for each state, and stated they were working from home.",  locations = cells_body(columns = vars(percent, n), rows = 1))%>%
  cols_label(type_productive = md("Response option"), n = md("N"), percent = md("Percent"))
table_home_productive
gtsave(table_home_productive, "table_home_productive.png")
```
Home productivity overall
```{r}
home_productive_overall_dat = subset(tech_cri_dat_complete, situation == 1)
home_productive_overall_dat = na.omit(data.frame(home_productive_overall_dat[,6:10], state = home_productive_overall_dat$state))

n_home_productive_overall_dat = dim(home_productive_overall_dat)[1]

home_productive_overall_dat = reshape(home_productive_overall_dat, varying = list(c("home_productivity___1", "home_productivity___2", "home_productivity___3", "home_productivity___4", "home_productivity___5")), times = c(1,2,3,4,5), direction = "long")



colnames(home_productive_overall_dat)[2:3] = c("type_productive", "home_productive_overall")
home_productive_overall_dat$type_productive = as.factor(home_productive_overall_dat$type_productive)

home_productive_overall_dat = home_productive_overall_dat%>% group_by(type_productive) %>% 
  count(home_productive_overall)
home_productive_overall_dat

home_productive_overall_dat = subset(home_productive_overall_dat, home_productive_overall == 1)
home_productive_overall_dat$home_productive_overall = NULL
home_productive_overall_dat$percent = home_productive_overall_dat$n / n_home_productive_overall_dat


home_productive_overall_dat$percent = round(home_productive_overall_dat$percent, 2)*100
home_productive_overall_dat$percent = paste0(home_productive_overall_dat$percent, "%")
home_productive_overall_dat$type_productive = recode(home_productive_overall_dat$type_productive, "1"= "I have no barriers and am working productively","2"= "I have poor internet/connection","3"= "I lack enabling technology equipment", "4" = "My workspace is not ideal", "5" = "Other")
home_productive_overall_dat


home_productive_overall_dat = home_productive_overall_dat %>% ungroup()


title_home_productive_overall = paste0("What barriers do you have to working from home at full productivity?", " ", "n=", n_home_productive_overall_dat)

table_home_productive_overall = 
  gt(home_productive_overall_dat) %>%
  tab_header(title = title_home_productive_overall)%>%
  tab_footnote(footnote = "Respondents can select all that apply so count / percent can add up to more / less than total n / 100%.  N is the total number who completed the survey according to REDCap, did not have missing data in any of the response options, and stated they were working from home.",  locations = cells_body(columns = vars(percent, n), rows = 1))%>%
  cols_label(type_productive = md("Response option"), n = md("N"), percent = md("Percent"))
table_home_productive_overall
gtsave(table_home_productive_overall, "table_home_productive_overall.png")
```


Qualitative other barriers at home code later
```{r echo=FALSE}
other_barriers_home_complete =  na.omit(data.frame(other_barriers_home = tech_cri_dat_complete$other_barriers_home, state = tech_cri_dat_complete$state))
write.csv(other_barriers_home_complete, "other_barriers_home_complete.csv", row.names = FALSE)
### other_barriers_home_complete
setwd("T:/CRI_Research/telehealth_evaluation/data_codebooks/satisfaction/clinician_qual")
other_barriers_home_complete_dat = read.csv("other_barriers_home_complete_dat.csv", header = TRUE, na.strings = "")
other_barriers = other_barriers_home_complete_dat[,2:5]

n_state_other_barriers = describe.factor(other_barriers$state, decr.order = FALSE)
n_state_other_barriers = data.frame(n_state_other_barriers)
n_state_other_barriers = n_state_other_barriers[1,]

colnames(n_state_other_barriers) = c("Indiana", "Florida", "Tennessee", "Illinois", "Another state")

### Stack all the themes and keep the original n for the percentage what about state
### datPrePost3month = reshape(datPrePost3month, varying  = list(c("Sec1Qa.x", "Sec1Qa.y", "Sec1Qa"), direction = "long", times =c(0,1,2))
n_other_barriers = dim(other_barriers)[1]
other_barriers_long = reshape(other_barriers, varying = list(c("Theme.1", "Theme.2", "Theme.3")), direction  = "long", times = c(1,2,3))
other_barriers_long_complete = na.omit(other_barriers_long)
other_barriers_long_complete
other_barriers_long_complete_results = other_barriers_long_complete%>% group_by(state) %>% count(Theme.1)
other_barriers_long_complete_results

other_barriers_long_complete_results$percent = ifelse(other_barriers_long_complete_results$state == 1, other_barriers_long_complete_results$n / n_state_other_barriers$Indiana, ifelse(other_barriers_long_complete_results$state == 3, other_barriers_long_complete_results$n / n_state_other_barriers$Tennessee, ifelse(other_barriers_long_complete_results$state == 4, other_barriers_long_complete_results$n / n_state_other_barriers$Illinois, ifelse(other_barriers_long_complete_results$state == 2, other_barriers_long_complete_results$n / n_state_other_barriers$Florida, other_barriers_long_complete_results$n / n_state_other_barriers$`Another state`))))


other_barriers_long_complete_results$percent = round(other_barriers_long_complete_results$percent, 2)*100
other_barriers_long_complete_results$percent = paste0(other_barriers_long_complete_results$percent, "%")


#1, Indiana | 2, Florida | 3, Tennessee | 4, Illinois | 5, Another state
other_barriers_long_complete_results$state = ifelse(other_barriers_long_complete_results$state == 1, "Indiana", ifelse(other_barriers_long_complete_results$state == 2, "Florida", ifelse(other_barriers_long_complete_results$state == 3, "Tennessee", ifelse(other_barriers_long_complete_results$state == 4, "Illinois", ifelse(other_barriers_long_complete_results$state == 5, "Another state", "Wrong")))))

other_barriers_long_complete_results$state = ifelse(other_barriers_long_complete_results$state == "Indiana", paste0(other_barriers_long_complete_results$state, " ", "n=", n_state_other_barriers$Indiana) , ifelse(other_barriers_long_complete_results$state == "Tennessee", paste0(other_barriers_long_complete_results$state, " ", "n=", n_state_other_barriers$Tennessee), ifelse(other_barriers_long_complete_results$state == "Illinois", paste0(other_barriers_long_complete_results$state, " ", "n=", n_state_other_barriers$Illinois), ifelse(other_barriers_long_complete_results$state == "Florida", paste0(other_barriers_long_complete_results$state, " ", "n=", n_state_other_barriers$Florida), paste0(other_barriers_long_complete_results$state, " ", "n=", n_state_other_barriers$`Another state`)))))


other_barriers_long_complete_results$state = as.factor(other_barriers_long_complete_results$state)

other_barriers_long_complete_results$state = factor(other_barriers_long_complete_results$state, levels = levels(other_barriers_long_complete_results$state)[5:1])
other_barriers_long_complete_results = other_barriers_long_complete_results[order(other_barriers_long_complete_results$state),]
#describe.factor(other_barriers_long_complete_results$state)
colnames(other_barriers_long_complete_results)[2] = "Theme"
title_other_barriers = paste0("Other barriers to working from home")
table_other_barriers_home = 
  gt(other_barriers_long_complete_results) %>%
  tab_header(title = title_other_barriers)%>%
  tab_footnote(footnote = "Respondent answers can have multiple themes so count / percent can add up to more / less than total n / 100%.  N is the total number who completed the survey according to REDCap, did not have missing data in any of the response options for each state, stated they were working from home, and selected other.",  locations = cells_body(columns = vars(percent, n), rows = 1)) %>%
  cols_label(n = md("N"), percent = md("Percent"))
table_other_barriers_home
gtsave(table_other_barriers_home, "table_other_barriers_home.png") 
```
Other qualitative barriers overall
```{r}
other_barriers_home_complete =  na.omit(data.frame(other_barriers_home = tech_cri_dat_complete$other_barriers_home, state = tech_cri_dat_complete$state))
write.csv(other_barriers_home_complete, "other_barriers_home_complete.csv", row.names = FALSE)
### other_barriers_home_complete
setwd("T:/CRI_Research/telehealth_evaluation/data_codebooks/satisfaction/clinician_qual")
overall_other_barriers_home_complete_dat = read.csv("other_barriers_home_complete_dat.csv", header = TRUE, na.strings = "")

overall_other_barriers = overall_other_barriers_home_complete_dat[,3:5]

n_overall_other_barriers_overall = dim(overall_other_barriers)[1]
overall_other_barriers_long = reshape(overall_other_barriers, varying = list(c("Theme.1", "Theme.2", "Theme.3")), direction  = "long", times = c(1,2,3))
overall_other_barriers_long_complete = na.omit(overall_other_barriers_long)
overall_other_barriers_long_complete
overall_other_barriers_long_complete_results = overall_other_barriers_long_complete %>% count(Theme.1)
overall_other_barriers_long_complete_results

overall_other_barriers_long_complete_results$percent = overall_other_barriers_long_complete_results$n / n_overall_other_barriers_overall

overall_other_barriers_long_complete_results$percent = round(overall_other_barriers_long_complete_results$percent, 2)*100
overall_other_barriers_long_complete_results$percent = paste0(overall_other_barriers_long_complete_results$percent, "%")

colnames(overall_other_barriers_long_complete_results)[1:2] = c("Theme", "n")
title_overall_other_barriers = paste0("Other barriers to working from home", " ", "n=", n_overall_other_barriers_overall)

overall_other_barriers_long_complete_results = overall_other_barriers_long_complete_results %>% ungroup()

table_overall_other_barriers_home = 
  gt(overall_other_barriers_long_complete_results) %>%
  tab_header(title = title_overall_other_barriers)%>%
  tab_footnote(footnote = "Respondent answers can have multiple themes so count / percent can add up to more / less than total n / 100%.  N is the total number who completed the survey according to REDCap, did not have missing data in any of the response options for each state, stated they were working from home, and selected other. Additionally, some respondents selected other, but did not write anything, so total other category may not add up to other category in previous table.",  locations = cells_body(columns = vars(Theme, n), rows = 1))
table_overall_other_barriers_home
gtsave(table_overall_other_barriers_home, "table_overall_other_barriers_home.png")
```



office_why
I am working in the office because: 

1, My job is essential and cannot be performed remotely | 2, I have poor internet/connection at home | 3, I lack enabling technology equipment at home (Examples: monitor, headset, webcam, phone) | 4, My workspace is not ideal at home (lacks space, lacks privacy, inadequate furnishing) | 5, I am choosing (with leadership permission) to continue to work in the office
```{r echo=FALSE}

office_why_dat = subset(tech_cri_dat_complete, situation == 2)

office_why_dat = na.omit(data.frame(office_why_dat[,12:16], state = office_why_dat$state))
office_why_dat
n_state_office_why = describe.factor(office_why_dat$state, decr.order = FALSE)
n_state_office_why = data.frame(n_state_office_why)
n_state_office_why = n_state_office_why[1,]
n_state_office_why
office_why_dat = reshape(office_why_dat, varying = list(c("office_why___1", "office_why___2", "office_why___3", "office_why___4", "office_why___5")), times = c(1,2,3,4,5), direction = "long")
office_why_dat


colnames(office_why_dat)[2:3] = c("type_productive", "office_why")
office_why_dat$type_productive = as.factor(office_why_dat$type_productive)

office_why_dat = office_why_dat%>% group_by(state, type_productive) %>% 
  count(office_why)
office_why_dat

office_why_dat = subset(office_why_dat, office_why == 1)
office_why_dat$office_why = NULL
office_why_dat
office_why_dat$percent = ifelse(office_why_dat$state == "Indiana", office_why_dat$n / n_state_office_why$Indiana, ifelse(office_why_dat$state == "Tennessee", office_why_dat$n / n_state_office_why$Tennessee, ifelse(office_why_dat$state == "Illinois", office_why_dat$n / n_state_office_why$Illinois, ifelse(office_why_dat$state == "Florida", office_why_dat$n / n_state_office_why$Florida, office_why_dat$n / n_state_office_why$Another.state))))
office_why_dat$percent = round(office_why_dat$percent, 2)*100
office_why_dat$percent = paste0(office_why_dat$percent, "%")
office_why_dat$type_productive = recode(office_why_dat$type_productive, "1"= "My job is essential and cannot be performed remotely","2"= "I have poor internet/connection at home ","3"= "I lack enabling technology equipment", "4" = "My workspace is not ideal", "5" = "I am choosing (with leadership permission) to continue to work in the office")
office_why_dat

office_why_dat$state = ifelse(office_why_dat$state == "Indiana", paste0(office_why_dat$state, " ", "n=", n_state_office_why$Indiana) , ifelse(office_why_dat$state == "Tennessee", paste0(office_why_dat$state, " ", "n=", n_state_office_why$Tennessee), ifelse(office_why_dat$state == "Illinois", paste0(office_why_dat$state, " ", "n=", n_state_office_why$Illinois), ifelse(office_why_dat$state == "Florida", paste0(office_why_dat$state, " ", "n=", n_state_office_why$Florida), paste0(office_why_dat$state, " ", "n=", n_state_office_why$Another.state)))))
office_why_dat
title_office_why = paste0("I am working in the office because:")

office_why_dat = office_why_dat%>% group_by(state)

table_office_why = 
  gt(office_why_dat) %>%
  tab_header(title = title_office_why)%>%
  tab_footnote(footnote = "Respondents can select all that apply so count / percent can add up to more / less than total n / 100%.  N is the total number who completed the survey according to REDCap, did not have missing data in any of the response options for each state, and stated they were working from the office.",  locations = cells_body(columns = vars(percent, n), rows = 1))%>%
  cols_label(type_productive = md("Response option"), n = md("N"), percent = md("Percent"))
table_office_why
gtsave(table_office_why, "table_office_why.png")


```
Office why overall
```{r}
office_why_overall_dat = subset(tech_cri_dat_complete, situation == 2)
office_why_overall_dat = na.omit(data.frame(office_why_overall_dat[,12:16], state = office_why_overall_dat$state))

n_office_why_overall_dat = dim(office_why_overall_dat)[1]

office_why_overall_dat = reshape(office_why_overall_dat, varying = list(c("office_why___1", "office_why___2", "office_why___3", "office_why___4", "office_why___5")), times = c(1,2,3,4,5), direction = "long")



colnames(office_why_overall_dat)[2:3] = c("type_productive", "office_why_overall")
office_why_overall_dat$type_productive = as.factor(office_why_overall_dat$type_productive)

office_why_overall_dat = office_why_overall_dat%>% group_by(type_productive) %>% 
  count(office_why_overall)
office_why_overall_dat

office_why_overall_dat = subset(office_why_overall_dat, office_why_overall == 1)
office_why_overall_dat$office_why_overall = NULL
office_why_overall_dat$percent = office_why_overall_dat$n / n_office_why_overall_dat


office_why_overall_dat$percent = round(office_why_overall_dat$percent, 2)*100
office_why_overall_dat$percent = paste0(office_why_overall_dat$percent, "%")
office_why_overall_dat$type_productive = recode(office_why_overall_dat$type_productive, "1"= "My job is essential and cannot be performed remotely","2"= "I have poor internet/connection at home ","3"= "I lack enabling technology equipment", "4" = "My workspace is not ideal", "5" = "I am choosing (with leadership permission) to continue to work in the office")
office_why_overall_dat


office_why_overall_dat = office_why_overall_dat %>% ungroup()


title_office_why_overall = paste0("I am working in the office because:", " ", "n=", n_office_why_overall_dat)

table_office_why_overall = 
  gt(office_why_overall_dat) %>%
  tab_header(title = title_office_why_overall)%>%
  tab_footnote(footnote = "Respondents can select all that apply so count / percent can add up to more / less than total n / 100%.  N is the total number who completed the survey according to REDCap, did not have missing data in any of the response options and stated they were working from the office.",  locations = cells_body(columns = vars(percent, n), rows = 1))%>%
  cols_label(type_productive = md("Response option"), n = md("N"), percent = md("Percent"))
table_office_why_overall
gtsave(table_office_why_overall, "table_office_why_overall.png")
```


barriers_office
n_why[2,1]
1, I have no barriers and am working productively. | 2, I lack enabling technology equipment in the office (Examples: monitor, headset, webcam, phone | 3, Other
```{r echo=FALSE}

barriers_office_dat = subset(tech_cri_dat_complete, situation == 2)
barriers_office_dat = na.omit(data.frame(barriers_office_dat[,17:19], state = barriers_office_dat$state))
barriers_office_dat
n_state_barriers_office = describe.factor(barriers_office_dat$state, decr.order = FALSE)
n_state_barriers_office = data.frame(n_state_barriers_office)
n_state_barriers_office = n_state_barriers_office[1,]
n_state_barriers_office
barriers_office_dat = reshape(barriers_office_dat, varying = list(c("barriers_office___1", "barriers_office___2", "barriers_office___3")), times = c(1,2,3), direction = "long")
barriers_office_dat


colnames(barriers_office_dat)[2:3] = c("type_productive", "barriers_office")
barriers_office_dat$type_productive = as.factor(barriers_office_dat$type_productive)

barriers_office_dat = barriers_office_dat%>% group_by(state, type_productive) %>% 
  count(barriers_office)
barriers_office_dat

barriers_office_dat = subset(barriers_office_dat, barriers_office == 1)
barriers_office_dat$barriers_office = NULL
barriers_office_dat
barriers_office_dat$percent = ifelse(barriers_office_dat$state == "Indiana", barriers_office_dat$n / n_state_barriers_office$Indiana, ifelse(barriers_office_dat$state == "Tennessee", barriers_office_dat$n / n_state_barriers_office$Tennessee, ifelse(barriers_office_dat$state == "Illinois", barriers_office_dat$n / n_state_barriers_office$Illinois, ifelse(barriers_office_dat$state == "Florida", barriers_office_dat$n / n_state_barriers_office$Florida, barriers_office_dat$n / n_state_barriers_office$Another.state))))
barriers_office_dat$percent = round(barriers_office_dat$percent, 2)*100
barriers_office_dat$percent = paste0(barriers_office_dat$percent, "%")

barriers_office_dat$type_productive = recode(barriers_office_dat$type_productive, "1"= "I have no barriers and am working productively","2"= "I lack enabling technology equipment in the office", "3" = "Other")
barriers_office_dat

barriers_office_dat$state = ifelse(barriers_office_dat$state == "Indiana", paste0(barriers_office_dat$state, " ", "n=", n_state_barriers_office$Indiana) , ifelse(barriers_office_dat$state == "Tennessee", paste0(barriers_office_dat$state, " ", "n=", n_state_barriers_office$Tennessee), ifelse(barriers_office_dat$state == "Illinois", paste0(barriers_office_dat$state, " ", "n=", n_state_barriers_office$Illinois), ifelse(barriers_office_dat$state == "Florida", paste0(barriers_office_dat$state, " ", "n=", n_state_barriers_office$Florida), paste0(barriers_office_dat$state, " ", "n=", n_state_barriers_office$Another.state)))))
barriers_office_dat
title_barriers_office = paste0("What barriers if any do you have working in the office?")

barriers_office_dat = barriers_office_dat%>% group_by(state)

table_barriers_office = 
  gt(barriers_office_dat) %>%
  tab_header(title = title_barriers_office)%>%
  tab_footnote(footnote = "Respondents can select all that apply so count / percent can add up to more / less than total n / 100%.  N is the total number who completed the survey according to REDCap, did not have missing data in any of the response options for each state, and stated they were working from the office.",  locations = cells_body(columns = vars(percent, n), rows = 1))%>%
  cols_label(type_productive = md("Response option"), n = md("N"), percent = md("Percent"))
table_barriers_office
gtsave(table_barriers_office, "table_barriers_office.png")

```
Barriers barriers_office overall
```{r}
barriers_office_overall_dat = subset(tech_cri_dat_complete, situation == 2)
barriers_office_overall_dat = na.omit(data.frame(barriers_office_overall_dat[,17:19], state = barriers_office_overall_dat$state))
barriers_office_overall_dat

n_barriers_office_overall_dat = dim(barriers_office_overall_dat)[1]

barriers_office_overall_dat = reshape(barriers_office_overall_dat, varying = list(c("barriers_office___1", "barriers_office___2", "barriers_office___3")), times = c(1,2,3), direction = "long")



colnames(barriers_office_overall_dat)[2:3] = c("type_productive", "barriers_office_overall")
barriers_office_overall_dat$type_productive = as.factor(barriers_office_overall_dat$type_productive)

barriers_office_overall_dat = barriers_office_overall_dat%>% group_by(type_productive) %>% 
  count(barriers_office_overall)
barriers_office_overall_dat

barriers_office_overall_dat = subset(barriers_office_overall_dat, barriers_office_overall == 1)
barriers_office_overall_dat$barriers_office_overall = NULL
barriers_office_overall_dat
barriers_office_overall_dat$percent = barriers_office_overall_dat$n / n_barriers_office_overall_dat


barriers_office_overall_dat$percent = round(barriers_office_overall_dat$percent, 2)*100
barriers_office_overall_dat$percent = paste0(barriers_office_overall_dat$percent, "%")

barriers_office_overall_dat$type_productive = recode(barriers_office_overall_dat$type_productive, "1"= "I have no barriers and am working productively","2"= "I lack enabling technology equipment in the office", "3" = "Other")
barriers_office_overall_dat

title_barriers_office_overall = paste0("What barriers if any do you have working in the office?", " ", "n=", n_barriers_office_overall_dat)

barriers_office_overall_dat = barriers_office_overall_dat%>% ungroup()

table_barriers_office_overall = 
  gt(barriers_office_overall_dat) %>%
  tab_header(title = title_barriers_office_overall)%>%
  tab_footnote(footnote = "Respondents can select all that apply so count / percent can add up to more / less than total n / 100%.  N is the total number who completed the survey according to REDCap, did not have missing data in any of the response options, and stated they were working from the office.",  locations = cells_body(columns = vars(percent, n), rows = 1))%>%
  cols_label(type_productive = md("Response option"), n = md("N"), percent = md("Percent"))
table_barriers_office_overall
gtsave(table_barriers_office_overall, "table_barriers_office_overall.png")
```


other_barriers_office
Please describe the barriers.
Code later
Must be greater than 100 to code
```{r echo=FALSE}
length(tech_cri_dat_complete$other_barriers_office)-sum(is.na(tech_cri_dat_complete$other_barriers_office))

```



office_home_why
I am working in the office some days because:
1, Parts of my job cannot be performed remotely | 2, I am rotating with other staff covering office based tasks | 3, I have poor internet/connection at home and need to be on the network for my work. | 4, I lack enabling technology equipment at home (Examples: monitor, headset, webcam, phone). | 5, My workspace is not ideal at home (lacks space, lacks privacy, inadequate furnishing) | 6, I am choosing (with leadership permission) to continue to work in the office part of the time.
n_why[3,1]
```{r echo=FALSE}

office_home_why_dat = subset(tech_cri_dat_complete, situation == 3)
office_home_why_dat = na.omit(data.frame(office_home_why_dat[,21:26], state = office_home_why_dat$state))
office_home_why_dat
n_state_office_home_why = describe.factor(office_home_why_dat$state, decr.order = FALSE)
n_state_office_home_why = data.frame(n_state_office_home_why)
n_state_office_home_why = n_state_office_home_why[1,]
n_state_office_home_why
office_home_why_dat = reshape(office_home_why_dat, varying = list(c("office_home_why___1", "office_home_why___2", "office_home_why___3", "office_home_why___4", "office_home_why___5", "office_home_why___6")), times = c(1:6), direction = "long")
office_home_why_dat


colnames(office_home_why_dat)[2:3] = c("type_productive", "office_home_why")
office_home_why_dat$type_productive = as.factor(office_home_why_dat$type_productive)

office_home_why_dat = office_home_why_dat%>% group_by(state, type_productive) %>% 
  count(office_home_why)
office_home_why_dat

office_home_why_dat = subset(office_home_why_dat, office_home_why == 1)
office_home_why_dat$office_home_why = NULL
office_home_why_dat
office_home_why_dat$percent = ifelse(office_home_why_dat$state == "Indiana", office_home_why_dat$n / n_state_office_home_why$Indiana, ifelse(office_home_why_dat$state == "Tennessee", office_home_why_dat$n / n_state_office_home_why$Tennessee, ifelse(office_home_why_dat$state == "Illinois", office_home_why_dat$n / n_state_office_home_why$Illinois, ifelse(office_home_why_dat$state == "Florida", office_home_why_dat$n / n_state_office_home_why$Florida, office_home_why_dat$n / n_state_office_home_why$Another.state))))
office_home_why_dat$percent = round(office_home_why_dat$percent, 2)*100
office_home_why_dat$percent = paste0(office_home_why_dat$percent, "%")

office_home_why_dat$type_productive = recode(office_home_why_dat$type_productive, "1"= "Parts of my job cannot be performed remotely", "2"= "I am rotating with other staff covering office based tasks", "3"= "I have poor internet/connection at home and need to be on the network for my work","4"=  "I lack enabling technology equipment at home (Examples: monitor, headset, webcam, phone)", "5" ="My workspace is not ideal at home (lacks space, lacks privacy, inadequate furnishing)", "6" = "I am choosing (with leadership permission) to continue to work in the office part of the time")
office_home_why_dat

office_home_why_dat$state = ifelse(office_home_why_dat$state == "Indiana", paste0(office_home_why_dat$state, " ", "n=", n_state_office_home_why$Indiana) , ifelse(office_home_why_dat$state == "Tennessee", paste0(office_home_why_dat$state, " ", "n=", n_state_office_home_why$Tennessee), ifelse(office_home_why_dat$state == "Illinois", paste0(office_home_why_dat$state, " ", "n=", n_state_office_home_why$Illinois), ifelse(office_home_why_dat$state == "Florida", paste0(office_home_why_dat$state, " ", "n=", n_state_office_home_why$Florida), paste0(office_home_why_dat$state, " ", "n=", n_state_office_home_why$Another.state)))))
office_home_why_dat
title_office_home_why = paste0("I am working in the office some days because:")

office_home_why_dat = office_home_why_dat%>% group_by(state)

table_office_home_why = 
  gt(office_home_why_dat) %>%
  tab_header(title = title_office_home_why)%>%
  tab_footnote(footnote = "Respondents can select all that apply so count / percent can add up to more / less than total n / 100%.  N is the total number who completed the survey according to REDCap, did not have missing data in any of the response options for each state, and stated they were working from the office some days.",  locations = cells_body(columns = vars(percent, n), rows = 1))%>%
  cols_label(type_productive = md("Response option"), n = md("N"), percent = md("Percent"))
table_office_home_why
gtsave(table_office_home_why, "table_office_home_why.png")

```
Overall why_office_home
```{r}
office_home_why_dat = subset(tech_cri_dat_complete, situation == 3)
office_home_why_dat = na.omit(data.frame(office_home_why_dat[,21:26], state = office_home_why_dat$state))
office_home_why_dat
n_office_home_why_dat = dim(office_home_why_dat)[1]

office_home_why_dat = reshape(office_home_why_dat, varying = list(c("office_home_why___1", "office_home_why___2", "office_home_why___3", "office_home_why___4", "office_home_why___5", "office_home_why___6")), times = c(1:6), direction = "long")
office_home_why_dat


colnames(office_home_why_dat)[2:3] = c("type_productive", "office_home_why")
office_home_why_dat$type_productive = as.factor(office_home_why_dat$type_productive)

office_home_why_dat = office_home_why_dat%>% group_by(type_productive) %>% 
  count(office_home_why)
office_home_why_dat

office_home_why_dat = subset(office_home_why_dat, office_home_why == 1)
office_home_why_dat$office_home_why = NULL
office_home_why_dat
office_home_why_dat$percent = office_home_why_dat$n / n_office_home_why_dat
office_home_why_dat$percent= round(office_home_why_dat$percent, 2)*100
office_home_why_dat$percent = paste0(office_home_why_dat$percent, "%")


office_home_why_dat$type_productive = recode(office_home_why_dat$type_productive, "1"= "Parts of my job cannot be performed remotely", "2"= "I am rotating with other staff covering office based tasks", "3"= "I have poor internet/connection at home and need to be on the network for my work","4"=  "I lack enabling technology equipment at home (Examples: monitor, headset, webcam, phone)", "5" ="My workspace is not ideal at home (lacks space, lacks privacy, inadequate furnishing)", "6" = "I am choosing (with leadership permission) to continue to work in the office part of the time")
office_home_why_dat


title_office_home_why = paste0("I am working in the office some days because:", " ", "n=", n_office_home_why_dat)

office_home_why_dat = office_home_why_dat%>% ungroup()

table_office_home_why = 
  gt(office_home_why_dat) %>%
  tab_header(title = title_office_home_why)%>%
  tab_footnote(footnote = "Respondents can select all that apply so count / percent can add up to more / less than total n / 100%.  N is the total number who completed the survey according to REDCap, did not have missing data in any of the response options, and stated they were working from the office some days.",  locations = cells_body(columns = vars(percent, n), rows = 1))%>%
  cols_label(type_productive = md("Response option"), n = md("N"), percent = md("Percent"))
table_office_home_why
gtsave(table_office_home_why, "table_office_home_why.png")
```



barriers_office_home
What barriers if any do you have working in the office?
1, I have no barriers and am working productively. | 2, I lack enabling technology equipment in the office (Examples: monitor, headset, webcam, phone | 3, Other
n_why[3,1]
```{r echo=FALSE}

barriers_office_home_dat = subset(tech_cri_dat_complete, situation == 3)
barriers_office_home_dat = na.omit(data.frame(barriers_office_home_dat[,27:29], state = barriers_office_home_dat$state))
barriers_office_home_dat
n_state_barriers_office_home = describe.factor(barriers_office_home_dat$state, decr.order = FALSE)
n_state_barriers_office_home = data.frame(n_state_barriers_office_home)
n_state_barriers_office_home = n_state_barriers_office_home[1,]
n_state_barriers_office_home
barriers_office_home_dat = reshape(barriers_office_home_dat, varying = list(c("barriers_office_home___1", "barriers_office_home___2", "barriers_office_home___3")), times = c(1:3), direction = "long")
barriers_office_home_dat


colnames(barriers_office_home_dat)[2:3] = c("type_productive", "barriers_office_home")
barriers_office_home_dat$type_productive = as.factor(barriers_office_home_dat$type_productive)

barriers_office_home_dat = barriers_office_home_dat%>% group_by(state, type_productive) %>% 
  count(barriers_office_home)
barriers_office_home_dat

barriers_office_home_dat = subset(barriers_office_home_dat, barriers_office_home == 1)
barriers_office_home_dat$barriers_office_home = NULL
barriers_office_home_dat
barriers_office_home_dat$percent = ifelse(barriers_office_home_dat$state == "Indiana", barriers_office_home_dat$n / n_state_barriers_office_home$Indiana, ifelse(barriers_office_home_dat$state == "Tennessee", barriers_office_home_dat$n / n_state_barriers_office_home$Tennessee, ifelse(barriers_office_home_dat$state == "Illinois", barriers_office_home_dat$n / n_state_barriers_office_home$Illinois, ifelse(barriers_office_home_dat$state == "Florida", barriers_office_home_dat$n / n_state_barriers_office_home$Florida, barriers_office_home_dat$n / n_state_barriers_office_home$Another.state))))
barriers_office_home_dat$percent = round(barriers_office_home_dat$percent, 2)*100
barriers_office_home_dat$percent = paste0(barriers_office_home_dat$percent, "%")

barriers_office_home_dat$type_productive = recode(barriers_office_home_dat$type_productive, "1"= "I have no barriers and am working productively.", "2"= "I lack enabling technology equipment in the office", "3"= "Other")
barriers_office_home_dat

barriers_office_home_dat$state = ifelse(barriers_office_home_dat$state == "Indiana", paste0(barriers_office_home_dat$state, " ", "n=", n_state_barriers_office_home$Indiana) , ifelse(barriers_office_home_dat$state == "Tennessee", paste0(barriers_office_home_dat$state, " ", "n=", n_state_barriers_office_home$Tennessee), ifelse(barriers_office_home_dat$state == "Illinois", paste0(barriers_office_home_dat$state, " ", "n=", n_state_barriers_office_home$Illinois), ifelse(barriers_office_home_dat$state == "Florida", paste0(barriers_office_home_dat$state, " ", "n=", n_state_barriers_office_home$Florida), paste0(barriers_office_home_dat$state, " ", "n=", n_state_barriers_office_home$Another.state)))))
barriers_office_home_dat
title_barriers_office_home = paste0("What barriers if any do you have working in the office some days?")

barriers_office_home_dat = barriers_office_home_dat%>% group_by(state)

table_barriers_office_home = 
  gt(barriers_office_home_dat) %>%
  tab_header(title = title_barriers_office_home)%>%
  tab_footnote(footnote = "Respondents can select all that apply so count / percent can add up to more / less than total n / 100%.  N is the total number who completed the survey according to REDCap, did not have missing data in any of the response options for each state, and stated they were working from the office some days.",  locations = cells_body(columns = vars(percent, n), rows = 1))%>%
  cols_label(type_productive = md("Response option"), n = md("N"), percent = md("Percent"))
table_barriers_office_home
gtsave(table_barriers_office_home, "table_barriers_office_home.png")
```
other_barriers_office_home
Please describe the other barriers.
Code later
```{r echo=FALSE}

length(tech_cri_dat_complete$other_barriers_office_home)-sum(is.na(tech_cri_dat_complete$other_barriers_office_home))
```
Overall barriers_office_home
```{r}
barriers_office_home_dat = subset(tech_cri_dat_complete, situation == 3)
barriers_office_home_dat = na.omit(data.frame(barriers_office_home_dat[,27:29], state = barriers_office_home_dat$state))
barriers_office_home_dat
n_barriers_office_home_dat = dim(barriers_office_home_dat)[1]
barriers_office_home_dat = reshape(barriers_office_home_dat, varying = list(c("barriers_office_home___1", "barriers_office_home___2", "barriers_office_home___3")), times = c(1:3), direction = "long")
barriers_office_home_dat


colnames(barriers_office_home_dat)[2:3] = c("type_productive", "barriers_office_home")
barriers_office_home_dat$type_productive = as.factor(barriers_office_home_dat$type_productive)

barriers_office_home_dat = barriers_office_home_dat%>% group_by(type_productive) %>% 
  count(barriers_office_home)
barriers_office_home_dat

barriers_office_home_dat = subset(barriers_office_home_dat, barriers_office_home == 1)
barriers_office_home_dat$barriers_office_home = NULL
barriers_office_home_dat
barriers_office_home_dat$percent = barriers_office_home_dat$n / n_barriers_office_home_dat
barriers_office_home_dat$percent = round(barriers_office_home_dat$percent, 2)*100
barriers_office_home_dat$percent = paste0(barriers_office_home_dat$percent, "%")
barriers_office_home_dat

barriers_office_home_dat$type_productive = recode(barriers_office_home_dat$type_productive, "1"= "I have no barriers and am working productively.", "2"= "I lack enabling technology equipment in the office", "3"= "Other")
barriers_office_home_dat

title_barriers_office_home = paste0("What barriers if any do you have working in the office some days?", " ", "n=", n_barriers_office_home_dat)

barriers_office_home_dat = barriers_office_home_dat%>% ungroup()

table_barriers_office_home = 
  gt(barriers_office_home_dat) %>%
  tab_header(title = title_barriers_office_home)%>%
  tab_footnote(footnote = "Respondents can select all that apply so count / percent can add up to more / less than total n / 100%.  N is the total number who completed the survey according to REDCap, did not have missing data in any of the response options, and stated they were working from the office some days.",  locations = cells_body(columns = vars(percent, n), rows = 1))%>%
  cols_label(type_productive = md("Response option"), n = md("N"), percent = md("Percent"))
table_barriers_office_home
gtsave(table_barriers_office_home, "table_barriers_office_home.png")
```


#############
not_working_why
Only two responses makes sense, because people not working are not checking their email.
#####################

service_provided
Which of Centerstone's televideo or teleaudio services have you provided to client(s)? (please check all that apply)
1, Telephone only | 2, Zoom video and audio | 3, Zoom audio only | 4, SnapMD video and audio | 5, SnapMD audio only | 6, I do not provide televideo or teleaudio services

Fine to have tech_cri_dat_complete instead of clincian_survey_dat, becuase if you were not eligible for this survey you got a zero and I am counting the number of 1's and dividing by the number in the clincian survey
```{r echo=FALSE}

service_provided = tech_cri_dat_complete[,36:41]
service_provided = apply(service_provided, 2, sum)
service_provided = data.frame(service_provided)
service_provided$percent = service_provided$service_provided / n_clinician_survey
service_provided
response_options =  c("Telephone only", "Zoom video and audio", "Zoom audio only", "SnapMD video and audio", "SnapMD audio only", "I do not provide televideo or teleaudio services")
service_provided = data.frame(response_options, service_provided)
rownames(service_provided) = NULL
colnames(service_provided)[2] = "count"
service_provided$percent = round(service_provided$percent,2)
service_provided$percent = paste0(service_provided$percent*100, "%")
service_provided = service_provided[order(service_provided$count,decreasing = TRUE),]
service_provided
title_service_provided = paste0("Which of Centerstone's televideo or teleaudio services have you provided to client(s)?", " ", "n=", n_clinician_survey)
table_service_provided = 
  gt(service_provided) %>%
  tab_header(title = title_service_provided)%>%
  tab_footnote(footnote = "Respondents can select all that apply so count / percent can add up to more than total n / 100%",  locations = cells_body(columns = vars(percent, count), rows = 1)) %>%
  cols_label(response_options = md("Response options"), count = md("Count"), percent = md("Percent"))
table_service_provided
gtsave(table_service_provided, "table_service_provided.png")
```
barriers_snap_md_use
We are wondering if there are barriers to using SnapMD?  If there are, please check all that apply. 
1, Difficulty developing treatment plans | 2, Difficulty coordinating services across multiple providers | 3, Client's limited access to technology | 4, Client's limited access to private space | 5, Lack of clear policies for conducting televideo / teleaudio | 6, Lack of security for conducting televideo / teleaudio | 7, Decreased rapport with client(s) | 8, Difficulty gathering data from client(s) | 9, Difficulty accessing client(s) information | 10, Clients are uncomfortable with the technology | 11, Lack of training opportunities | 12, Other barriers not listed above

```{r}
barriers_snap_md_use = tech_cri_dat_complete[,42:53]
barriers_snap_md_use = apply(barriers_snap_md_use, 2, sum)
barriers_snap_md_use = data.frame(barriers_snap_md_use)
barriers_snap_md_use$percent = barriers_snap_md_use$barriers_snap_md_use / n_snap_md
barriers_snap_md_use
response_options =  c("Difficulty developing treatment plans", "Difficulty coordinating services across multiple providers", "Client's limited access to technology", "Client's limited access to private space", "Lack of clear policies for conducting televideo / teleaudio", "Lack of security for conducting televideo / teleaudio", "Decreased rapport with client(s)", "Difficulty gathering data from client(s)", "Difficulty accessing client(s) information", "Clients are uncomfortable with the technology", "Lack of training opportunities", "Other barriers not listed above")
barriers_snap_md_use = data.frame(response_options, barriers_snap_md_use)
rownames(barriers_snap_md_use) = NULL
colnames(barriers_snap_md_use)[2] = "count"
barriers_snap_md_use$percent = round(barriers_snap_md_use$percent,2)
barriers_snap_md_use$percent = paste0(barriers_snap_md_use$percent*100, "%")
write.csv(barriers_snap_md_use, "barriers_snap_md_use.csv", row.names = FALSE)
barriers_snap_md_use
barriers_snap_md_use = barriers_snap_md_use[order(barriers_snap_md_use$count,decreasing = TRUE),]

title_barriers_snap_md_use = paste0("We are wondering if there are barriers to using SnapMD?  If there are, please check all that apply.", " ", "n=", n_snap_md)
table_barriers_snap_md_use = 
  gt(barriers_snap_md_use) %>%
  tab_header(title = title_barriers_snap_md_use)%>%
  tab_footnote(footnote = "Respondents can select all that apply so count / percent can add up to more than total n / 100%.  The n is the total number of clinicians who said they used SnapMD video and audio or SnapMD audio only.",  locations = cells_body(columns = vars(percent, count), rows = 1)) %>%
  cols_label(response_options = md("Response options"), count = md("Count"), percent = md("Percent"))
table_barriers_snap_md_use
gtsave(table_barriers_snap_md_use, "table_barriers_snap_md_use.png")

```
other_snap_barriers_use
Please list the other barrier(s).
Code later
```{r}

```


facilitate_snapmd
We are wondering if SnapMD makes it easier to provide services to clients relative to in person?  If it does, please check all that apply.
1, Increased work life balance | 2, Decreased commute time | 3, Increased schedule flexibility | 4, Increased access to difficult to reach clients | 5, Quicker access to clients | 6, Increased convenience | 7, Other factors not listed above
```{r echo=FALSE}
facilitate_snapmd = tech_cri_dat_complete[,55:61]
facilitate_snapmd = apply(facilitate_snapmd, 2, sum)
facilitate_snapmd = data.frame(facilitate_snapmd)
facilitate_snapmd$percent = facilitate_snapmd$facilitate_snapmd / n_snap_md
facilitate_snapmd
response_options =  c("Increased work life balance", "Decreased commute time", "Increased schedule flexibility", "Increased access to difficult to reach clients", "Quicker access to clients", "Increased convenience", "Other factors not listed above")
facilitate_snapmd = data.frame(response_options, facilitate_snapmd)
rownames(facilitate_snapmd) = NULL
colnames(facilitate_snapmd)[2] = "count"
facilitate_snapmd$percent = round(facilitate_snapmd$percent,2)
facilitate_snapmd$percent = paste0(facilitate_snapmd$percent*100, "%")

facilitate_snapmd = facilitate_snapmd[order(facilitate_snapmd$count,decreasing = TRUE),]
title_facilitate_snapmd = paste0("We are wondering if SnapMD makes it easier to provide services to clients relative to in person?  If it does, please check all that apply.", " ", "n=", n_snap_md)
table_facilitate_snapmd = 
  gt(facilitate_snapmd) %>%
  tab_header(title = title_facilitate_snapmd)%>%
  tab_footnote(footnote = "Respondents can select all that apply so count / percent can add up to more than total n / 100%. The n is the total number of clinicians who said they used SnapMD video and audio or SnapMD audio only.",  locations = cells_body(columns = vars(percent, count), rows = 1)) %>%
  cols_label(response_options = md("Response options"), count = md("Count"), percent = md("Percent"))
table_facilitate_snapmd
gtsave(table_facilitate_snapmd, "table_facilitate_snapmd.png")

```

other_facilitate_snapmd
Not needed only 9 responses

barriers_zoom_use
We are wondering if there are barriers to using Zoom?  If there are, please check all that apply.
```{r echo=FALSE}
barriers_zoom_use = tech_cri_dat_complete[,63:74]
#barriers_zoom_use = apply(barriers_zoom_use, 2, as.factor)
barriers_zoom_use = apply(barriers_zoom_use, 2, sum)
barriers_zoom_use = data.frame(barriers_zoom_use)
barriers_zoom_use$percent = barriers_zoom_use$barriers_zoom_use / n_zoom
barriers_zoom_use
response_options =  c("Difficulty developing treatment plans", "Difficulty coordinating services across multiple providers", "Client's limited access to technology", "Client's limited access to private space", "Lack of clear policies for conducting televideo / teleaudio", "Lack of security for conducting televideo / teleaudio", "Decreased rapport with client(s)", "Difficulty gathering data from client(s)", "Difficulty accessing client(s) information", "Clients are uncomfortable with the technology", "Lack of training opportunities", "Other barriers not listed above")
barriers_zoom_use = data.frame(response_options, barriers_zoom_use)
rownames(barriers_zoom_use) = NULL
colnames(barriers_zoom_use)[2] = "count"
barriers_zoom_use$percent = round(barriers_zoom_use$percent,2)
barriers_zoom_use$percent = paste0(barriers_zoom_use$percent*100, "%")
barriers_zoom_use = barriers_zoom_use[order(barriers_zoom_use$count,decreasing = TRUE),]
barriers_zoom_use

title_barriers_zoom_use = paste0("We are wondering if there are barriers to using Zoom?  If there are, please check all that apply.", " ", "n=", n_zoom)
table_barriers_zoom_use = 
  gt(barriers_zoom_use) %>%
  tab_header(title = title_barriers_zoom_use)%>%
  tab_footnote(footnote = "Respondents can select all that apply so count / percent can add up to more than total n / 100%.  The n is the total number of clinicians who said they used Zoom video and audio or Zoom audio only.",  locations = cells_body(columns = vars(percent, count), rows = 1)) %>%
  cols_label(response_options = md("Response options"), count = md("Count"), percent = md("Percent"))
table_barriers_zoom_use
gtsave(table_barriers_zoom_use, "table_barriers_zoom_use.png")

```
other_zoom_barriers_use
Please list the other barrier(s).
Code this one

Cat 
```{r}


setwd("T:/CRI_Research/telehealth_evaluation/data_codebooks/satisfaction/clinician_qual")
other_zoom_use_complete_dat = read.csv("other_zoom_barriers_use_complete_dat.csv", header = TRUE, na.strings = "")
other_barriers = other_zoom_use_complete_dat[,2:5]

n_other_barriers = dim(other_barriers)[1]
other_barriers_long = reshape(other_barriers, varying = list(c("Theme.1", "Theme.2", "Theme.3")), direction  = "long", times = c(1,2,3))
other_barriers_long_complete = na.omit(other_barriers_long)
other_barriers_long_complete
other_barriers_long_complete_results = other_barriers_long_complete %>% count(Theme.1)
other_barriers_long_complete_results
other_barriers_long_complete_results$percent = other_barriers_long_complete_results$n / n_other_barriers

other_barriers_long_complete_results$percent = round(other_barriers_long_complete_results$percent, 2)*100
other_barriers_long_complete_results$percent = paste0(other_barriers_long_complete_results$percent, "%")


other_barriers_long_complete_results = other_barriers_long_complete_results[order(-other_barriers_long_complete_results$n),]
#describe.factor(other_barriers_long_complete_results$state)
colnames(other_barriers_long_complete_results)[1:2] = c("Theme", "n")
title_other_barriers = paste0("Other barriers to using Zoom")
table_other_zoom_use = 
  gt(other_barriers_long_complete_results) %>%
  tab_header(title = title_other_barriers)%>%
  tab_footnote(footnote = "Respondents can select all that apply so count / percent can add up to more / less than total n / 100%.  N is the total number who completed the survey according to REDCap, did not have missing data in any of the response options for each state, stated they used Zoom audio and or Zoom audio and video, and selected other.",  locations = cells_body(columns = vars(percent, n), rows = 1))
table_other_zoom_use
gtsave(table_other_zoom_use, "table_other_zoom_use.png")
```

facilitate_zoom
We are wondering if Zoom makes it easier to provide services to clients relative to in person?  If it does, please check all that apply.
```{r echo=FALSE}
facilitate_zoom = tech_cri_dat_complete[,76:82]
facilitate_zoom = apply(facilitate_zoom, 2, sum)
facilitate_zoom = data.frame(facilitate_zoom)
facilitate_zoom$percent = facilitate_zoom$facilitate_zoom / n_zoom
facilitate_zoom
response_options =  c("Increased work life balance", "Decreased commute time", "Increased schedule flexibility", "Increased access to difficult to reach clients", "Quicker access to clients", "Increased convenience", "Other factors not listed above")
facilitate_zoom = data.frame(response_options, facilitate_zoom)
rownames(facilitate_zoom) = NULL
colnames(facilitate_zoom)[2] = "count"
facilitate_zoom$percent = round(facilitate_zoom$percent,2)
facilitate_zoom$percent = paste0(facilitate_zoom$percent*100, "%")

facilitate_zoom = facilitate_zoom[order(facilitate_zoom$count,decreasing = TRUE),]

title_facilitate_zoom = paste0("We are wondering if Zoom makes it easier to provide services to clients relative to in person?  If it does, please check all that apply.", " ", "n=", n_zoom)
table_facilitate_zoom = 
  gt(facilitate_zoom) %>%
  tab_header(title = title_facilitate_zoom)%>%
  tab_footnote(footnote = "Respondents can select all that apply so count / percent can add up to more than total n / 100%. The n is the total number of clinicians who said they used Zoom video and audio or Zoom audio only.",  locations = cells_body(columns = vars(percent, count), rows = 1)) %>%
  cols_label(response_options = md("Response options"), count = md("Count"), percent = md("Percent"))
table_facilitate_zoom
gtsave(table_facilitate_zoom, "table_facilitate_zoom.png")
```
other_facilitate_zoom
```{r}
length(tech_cri_dat_complete$other_facilitate_zoom) - sum(is.na(tech_cri_dat_complete$other_facilitate_zoom))

```

barriers_snap_md
Since you did not select SnapMD, we are wondering, what are the barriers to using SnapMD? (please check all that apply)
n = service_provided[c(1:3,6),2]
Not SnapMD selected
```{r echo=FALSE}

barriers_snap_md = subset(clincian_survey_dat, service_provided___4 == 0 & service_provided___5 == 0)
barriers_snap_md = barriers_snap_md[,84:95]
barriers_snap_md = apply(barriers_snap_md, 2, sum)
barriers_snap_md = data.frame(barriers_snap_md)
barriers_snap_md$percent = barriers_snap_md$barriers_snap_md / n_no_snap_md
barriers_snap_md
response_options =  c("Difficulty developing treatment plans", "Difficulty coordinating services across multiple providers", "Client's limited access to technology", "Client's limited access to private space", "Lack of clear policies for conducting televideo / teleaudio", "Lack of security for conducting televideo / teleaudio", "Decreased rapport with client(s)", "Difficulty gathering data from client(s)", "Difficulty accessing client(s) information", "Clients are uncomfortable with the technology", "Lack of training opportunities", "Other barriers not listed above")
barriers_snap_md = data.frame(response_options, barriers_snap_md)
rownames(barriers_snap_md) = NULL
colnames(barriers_snap_md)[2] = "count"
barriers_snap_md$percent = round(barriers_snap_md$percent,2)
barriers_snap_md$percent = paste0(barriers_snap_md$percent*100, "%")

barriers_snap_md = barriers_snap_md[order(barriers_snap_md$count,decreasing = TRUE),]

title_barriers_snap_md = paste0("Since you did not select SnapMD, we are wondering, what are the barriers to using SnapMD? (please check all that apply)", " ", "n=", n_no_snap_md)
table_barriers_snap_md = 
  gt(barriers_snap_md) %>%
  tab_header(title = title_barriers_snap_md)%>%
  tab_footnote(footnote = "Respondents can select all that apply so count / percent can add up to more than total n / 100%.  The n is the total number of clinicians who said they did not use SnapMD video and audio or SnapMD audio only.",  locations = cells_body(columns = vars(percent, count), rows = 1)) %>%
  cols_label(response_options = md("Response options"), count = md("Count"), percent = md("Percent"))
table_barriers_snap_md

gtsave(table_barriers_snap_md, "table_barriers_snap_md.png")

```


other_snap_barriers
Other barriers not listed above
Jess
Correct Jess's codes
Need to match what she did with the correct sample 
```{r}
setwd("T:/CRI_Research/telehealth_evaluation/data_codebooks/satisfaction/clinician_qual")
combine_correct_other_snap_barriers = read.csv("combine_correct_other_snap_barriers.csv", header = TRUE)
combine_correct_other_snap_barriersdup =  duplicated(combine_correct_other_snap_barriers$other_snap_barriers)
combine_correct_other_snap_barriers = subset(combine_correct_other_snap_barriers, dup == FALSE)
write.csv(combine_correct_other_snap_barriers, "correct_snap_other_barriers.csv", row.names = FALSE)
```

Other barriers working from home
```{r}
##############################################################
# Replace the code below later
##############################################################
other_barriers_home_complete =  na.omit(data.frame(other_barriers_home = tech_cri_dat_complete$other_barriers_home, state = tech_cri_dat_complete$state))
write.csv(other_barriers_home_complete, "other_barriers_home_complete.csv", row.names = FALSE)
### other_barriers_home_complete
setwd("T:/CRI_Research/telehealth_evaluation/data_codebooks/satisfaction/clinician_qual")
other_barriers_home_complete_dat = read.csv("other_barriers_home_complete_dat.csv", header = TRUE, na.strings = "")
other_barriers = other_barriers_home_complete_dat[,2:5]
install.packages("prettyR")
library(prettyR)
n_state_other_barriers = describe.factor(other_barriers$state, decr.order = FALSE)
n_state_other_barriers = data.frame(n_state_other_barriers)
n_state_other_barriers = n_state_other_barriers[1,]

colnames(n_state_other_barriers) = c("Indiana", "Florida", "Tennessee", "Illinois", "Another state")

### Stack all the themes and keep the original n for the percentage what about state
### datPrePost3month = reshape(datPrePost3month, varying  = list(c("Sec1Qa.x", "Sec1Qa.y", "Sec1Qa"), direction = "long", times =c(0,1,2))
n_other_barriers = dim(other_barriers)[1]
other_barriers_long = reshape(other_barriers, varying = list(c("Theme.1", "Theme.2", "Theme.3")), direction  = "long", times = c(1,2,3))
other_barriers_long_complete = na.omit(other_barriers_long)
other_barriers_long_complete
other_barriers_long_complete_results = other_barriers_long_complete%>% group_by(state) %>% count(Theme.1)
other_barriers_long_complete_results

other_barriers_long_complete_results$percent = ifelse(other_barriers_long_complete_results$state == 1, other_barriers_long_complete_results$n / n_state_other_barriers$Indiana, ifelse(other_barriers_long_complete_results$state == 3, other_barriers_long_complete_results$n / n_state_other_barriers$Tennessee, ifelse(other_barriers_long_complete_results$state == 4, other_barriers_long_complete_results$n / n_state_other_barriers$Illinois, ifelse(other_barriers_long_complete_results$state == 2, other_barriers_long_complete_results$n / n_state_other_barriers$Florida, other_barriers_long_complete_results$n / n_state_other_barriers$`Another state`))))


other_barriers_long_complete_results$percent = round(other_barriers_long_complete_results$percent, 2)*100
other_barriers_long_complete_results$percent = paste0(other_barriers_long_complete_results$percent, "%")


#1, Indiana | 2, Florida | 3, Tennessee | 4, Illinois | 5, Another state
other_barriers_long_complete_results$state = ifelse(other_barriers_long_complete_results$state == 1, "Indiana", ifelse(other_barriers_long_complete_results$state == 2, "Florida", ifelse(other_barriers_long_complete_results$state == 3, "Tennessee", ifelse(other_barriers_long_complete_results$state == 4, "Illinois", ifelse(other_barriers_long_complete_results$state == 5, "Another state", "Wrong")))))

other_barriers_long_complete_results$state = ifelse(other_barriers_long_complete_results$state == "Indiana", paste0(other_barriers_long_complete_results$state, " ", "n=", n_state_other_barriers$Indiana) , ifelse(other_barriers_long_complete_results$state == "Tennessee", paste0(other_barriers_long_complete_results$state, " ", "n=", n_state_other_barriers$Tennessee), ifelse(other_barriers_long_complete_results$state == "Illinois", paste0(other_barriers_long_complete_results$state, " ", "n=", n_state_other_barriers$Illinois), ifelse(other_barriers_long_complete_results$state == "Florida", paste0(other_barriers_long_complete_results$state, " ", "n=", n_state_other_barriers$Florida), paste0(other_barriers_long_complete_results$state, " ", "n=", n_state_other_barriers$`Another state`)))))


other_barriers_long_complete_results$state = as.factor(other_barriers_long_complete_results$state)

other_barriers_long_complete_results$state = factor(other_barriers_long_complete_results$state, levels = levels(other_barriers_long_complete_results$state)[5:1])
other_barriers_long_complete_results = other_barriers_long_complete_results[order(other_barriers_long_complete_results$state),]
#describe.factor(other_barriers_long_complete_results$state)
colnames(other_barriers_long_complete_results)[2] = "Theme"
title_other_barriers = paste0("Other barriers to working from home")
table_other_barriers_home = 
  gt(other_barriers_long_complete_results) %>%
  tab_header(title = title_other_barriers)%>%
  tab_footnote(footnote = "Respondents can select all that apply so count / percent can add up to more / less than total n / 100%.  N is the total number who completed the survey according to REDCap, did not have missing data in any of the response options for each state, stated they were working from home, and selected other.",  locations = cells_body(columns = vars(percent, n), rows = 1))
table_other_barriers_home
gtsave(table_other_barriers_home, "table_other_barriers_home.png")

```
other_home_barriers overall


barriers_zoom
Since you did not select Zoom, we are wondering, what are the barriers to using Zoom? (please check all that apply)
### Messed up skip logic
Need to subset for those who selected Zoom options
1, Telephone only | 2, Zoom video and audio | 3, Zoom audio only | 4, SnapMD video and audio | 5, SnapMD audio only | 6, I do not provide televideo or teleaudio services



```{r echo=FALSE}

sum(tech_cri_dat_complete$barriers_zoom___3)
barriers_zoom = subset(clincian_survey_dat, service_provided___2 == 0 & service_provided___3 == 0)
dim(barriers_zoom)
barriers_zoom = barriers_zoom[,97:108]

barriers_zoom = apply(barriers_zoom, 2, sum)
barriers_zoom = data.frame(barriers_zoom)
barriers_zoom$percent = barriers_zoom$barriers_zoom / n_no_zoom
barriers_zoom
response_options =  c("Difficulty developing treatment plans", "Difficulty coordinating services across multiple providers", "Client's limited access to technology", "Client's limited access to private space", "Lack of clear policies for conducting televideo / teleaudio", "Lack of security for conducting televideo / teleaudio", "Decreased rapport with client(s)", "Difficulty gathering data from client(s)", "Difficulty accessing client(s) information", "Clients are uncomfortable with the technology", "Lack of training opportunities", "Other barriers not listed above")
barriers_zoom = data.frame(response_options, barriers_zoom)
rownames(barriers_zoom) = NULL
colnames(barriers_zoom)[2] = "count"
barriers_zoom$percent = round(barriers_zoom$percent,2)
barriers_zoom$percent = paste0(barriers_zoom$percent*100, "%")


barriers_zoom = barriers_zoom[order(barriers_zoom$count,decreasing = TRUE),]

title_barriers_zoom = paste0("Since you did not select Zoom, we are wondering, what are the barriers to using Zoom? (please check all that apply)", " ", "n=", n_no_zoom)
table_barriers_zoom = 
  gt(barriers_zoom) %>%
  tab_header(title = title_barriers_zoom)%>%
  tab_footnote(footnote = "Respondents can select all that apply so count / percent can add up to more than total n / 100%. The n is the total number of clinicians who said they did not use Zoom video and audio or Zoom audio only.",  locations = cells_body(columns = vars(percent, count), rows = 1)) %>%
  cols_label(response_options = md("Response options"), count = md("Count"), percent = md("Percent"))
table_barriers_zoom
gtsave(table_barriers_zoom, "table_barriers_zoom.png")
```
other_zoom_barriers
Please list the other barrier(s).
Code later
```{r}
length(tech_cri_dat_complete$other_zoom_barriers) - sum(is.na(tech_cri_dat_complete$other_zoom_barriers))

```


communicate
Televideo has helped me communicate with my client(s).
1, Strongly disagree | 2, Disagree | 3, Undecided | 4, Agree | 5, Strongly agree | 6, N/A

Let's do an average score and then plot by all four of them
```{r echo=FALSE}
### Client survey data
#clincian_survey_dat
telehealth_sat_dat = clincian_survey_dat
n_telehealth_sat_dat = dim(telehealth_sat_dat)[1]
## Subset 6, because you are a dumbass!!!!  6 = N/A
telehealth_sat_dat = telehealth_sat_dat[,110:113]
telehealth_sat_dat[telehealth_sat_dat == 6] = NA
telehealth_sat_p = telehealth_sat_dat
apply(telehealth_sat_dat, 2, range, na.rm = TRUE)
telehealth_sat_dat = data.frame(telehealth_sat = telehealth_sat_dat)
telehealth_sat_dat = apply(telehealth_sat_dat, 2, mean, na.rm = TRUE)
telehealth_sat_dat = round(telehealth_sat_dat,2)
telehealth_sat_dat = data.frame(telehealth_sat_dat)
var_names = c("communicate", "substance", "manage", "recovery")
telehealth_sat_dat = data.frame(var_names, telehealth_sat_dat)
rownames(telehealth_sat_dat) = NULL
telehealth_sat_dat
## Get rid of total
title_telehealth_sat = paste0("Televideo satisfaction", " ", "n=", n_telehealth_sat_dat)
plot_telehealth_sat = ggplot(telehealth_sat_dat, aes(x = var_names,y = telehealth_sat_dat, fill = telehealth_sat_dat))+
  geom_bar(stat = "identity")+
  labs(title=title_telehealth_sat, x ="Outcome", y = "Average rating")+
  scale_y_continuous(limits = c(0,5))+
  labs(fill = "")
plot_telehealth_sat
telehealth_sat_p = telehealth_sat_dat
telehealth_sat_p = apply(telehealth_sat_p, 2, function(x){ifelse(x > 3,1,0)})
telehealth_sat_p
telehealth_sat_p = data.frame(telehealth_sat_p)
telehealth_sat_p_complete = na.omit(telehealth_sat_p)
n_telehealth_sat_p_complete  = dim(telehealth_sat_p_complete)[1]
telehealth_sat_p_complete_n = apply(telehealth_sat_p_complete, 2, sum)
telehealth_sat_p_complete_p = round(telehealth_sat_p_complete_n/n_telehealth_sat_p_complete,2)
telehealth_sat_p_complete = rbind(telehealth_sat_p_complete_n, telehealth_sat_p_complete_p)
rownames(telehealth_sat_p_complete) = c("n", "%")
telehealth_sat_p_complete
```
comfort_televideo
What is your level of comfort with televideo?
1, Very uncomfortable | 2, Uncomfortable | 3, Somewhat uncomfortable | 4, Neither uncomfortable nor comfortable | 5, Somewhat comfortable | 6, Comfortable | 7, Very comfortable
```{r echo=FALSE}

### Client survey data
#clincian_survey_dat
comfort_televideo_dat = na.omit(clincian_survey_dat$comfort_televideo)
comfort_televideo_dat = data.frame(comfort_televideo = comfort_televideo_dat)
n_comfort_televideo_dat = dim(comfort_televideo_dat)[1]

library(descr)
library(installr)
uninstall.packages("questionr")
uninstall.packages("frequency")
uninstall.packages("prettyR")
comfort_televideo_dat = data.frame(freq(comfort_televideo_dat$comfort_televideo))
## Get rid of total

comfort_televideo_dat = comfort_televideo_dat[-8,]
var_names =  rownames(comfort_televideo_dat)
comfort_televideo_dat$var_names = var_names
typeof(comfort_televideo_dat$Frequency)
comfort_televideo_dat$Percent = round(comfort_televideo_dat$Percent,0)
greater_comfort = sum(comfort_televideo_dat$Percent[5:7])
comfort_televideo_dat$Percent = paste0(comfort_televideo_dat$Percent, "%")
title_comfort_televideo_dat = paste0("What is your level of comfort with televideo?", " ", "n=", n_comfort_televideo_dat)
#comfort_televideo_dat$Frequency = paste0("n=",comfort_televideo_dat$Frequency)
plot_comfort_televideo = ggplot(comfort_televideo_dat, aes(x = var_names,y = Frequency, fill = var_names))+
  geom_bar(stat = "identity")+
  labs(title=title_comfort_televideo_dat, x ="Response option", y = "Count")+
  scale_y_continuous(limits = c(0,600))+
  theme(legend.position = "none")+
  geom_text_repel(label = comfort_televideo_dat$Percent, vjust = -.5)
plot_comfort_televideo
greater_comfort
```
increase_comfort
Is there something Centerstone can do to increase your comfort level?  If so, please describe what Centerstone can do.
```{r echo=FALSE}
#head(clincian_survey_dat$increase_comfort,15)
```
interest_working_home
What is your level of interest in providing televideo services in the future?
1, Very disinterested | 2, Disinterested | 3, Somewhat disinterested | 4, Neither disinterested nor interested | 5, Somewhat interested | 6, Interested | 7, Very interested

Also include those who service_provided != 6
```{r echo=FALSE}
interest_working_home_dat = na.omit(clincian_survey_dat$interest_working_home)
interest_working_home_dat = data.frame(interest_working_home = interest_working_home_dat)
n_interest_working_home_dat = dim(interest_working_home_dat)[1]
interest_working_home_dat = data.frame(freq(interest_working_home_dat$interest_working_home))
## Get rid of total
interest_working_home_dat = interest_working_home_dat[-8,]
var_names =  rownames(interest_working_home_dat)
interest_working_home_dat$var_names = var_names
typeof(interest_working_home_dat$Frequency)
interest_working_home_dat$Percent = round(interest_working_home_dat$Percent,0)
greater_interest = sum(interest_working_home_dat$Percent[5:7])
interest_working_home_dat$Percent = paste0(interest_working_home_dat$Percent, "%")
title_interest_working_home_dat = paste0("What is your level of interest in providing televideo \n services in the future?", " ", "n=", n_interest_working_home_dat)
plot_interest_working_home = ggplot(interest_working_home_dat, aes(x = var_names,y = Frequency, fill = var_names))+
  geom_bar(stat = "identity")+
  labs(title=title_interest_working_home_dat, x ="Response option", y = "Count")+
  scale_y_continuous(limits = c(0,600))+
  theme(legend.position = "none")+
  geom_text_repel(label = interest_working_home_dat$Percent, vjust = -.5)
plot_interest_working_home
greater_interest
```
barriers_work_home
Are there barriers limiting your interest in providing televideo from home in the future?  If so, please list them.
```{r echo=FALSE}

```
prefer_service
In the future, how would you prefer to provide services? (please check all that apply)
1, Televideo | 2, Teleaudio | 3, In-person

clincian_survey_dat
```{r echo=FALSE}

prefer_service = tech_cri_dat_complete[,118:120]
prefer_service = apply(prefer_service, 2, sum)
prefer_service = data.frame(prefer_service)
prefer_service$percent = prefer_service$prefer_service / n_clinician_survey
prefer_service
response_options =  c("Televideo", "Teleaudio", "In-person")
prefer_service = data.frame(response_options, prefer_service)
rownames(prefer_service) = NULL
colnames(prefer_service)[2] = "count"
prefer_service$percent = round(prefer_service$percent,2)
prefer_service$percent = paste0(prefer_service$percent*100, "%")

prefer_service = prefer_service[order(prefer_service$count,decreasing = TRUE),]


title_prefer_service = paste0("In the future, how would you prefer to provide services? (please check all that apply)", " ", "n=", n_clinician_survey)
table_prefer_service = 
  gt(prefer_service) %>%
  tab_header(title = title_prefer_service)%>%
  tab_footnote(footnote = "Respondents can select all that apply so count / percent can add up to more than total n / 100%",  locations = cells_body(columns = vars(percent, count), rows = 1)) %>%
  cols_label(response_options = md("Response options"), count = md("Count"), percent = md("Percent"))
table_prefer_service

gtsave(table_prefer_service, "table_prefer_service.png")

```
ideal_features
Please select the features you would like to see in your future ideal televideo platform. (please check all that apply)
1, Integration with medical records | 2, Electronic assessment capabilities | 3, Ability to conduct group sessions | 4, Virtual walk-in capabilities | 5, Ability to collect signatures from clients | 6, Ability to send documents to clients | 7, Client's ability to schedule appointments | 8, Other feature(s) not listed here
If you select select prefer_service___1 == 1

```{r echo=FALSE}
clincian_survey_ideal_dat = subset(clincian_survey_dat, prefer_service___1 == 1)
dim(clincian_survey_ideal_dat)
n_clincian_survey_ideal = dim(clincian_survey_ideal_dat)[1]
ideal_features = clincian_survey_ideal_dat[,121:128]
ideal_features = apply(ideal_features, 2, sum)
ideal_features = data.frame(ideal_features)
ideal_features$percent = ideal_features$ideal_features / n_clincian_survey_ideal
ideal_features
response_options =  c("Integration with medical records", "Electronic assessment capabilities", "Ability to conduct group sessions", "Virtual walk-in capabilities", "Ability to collect signatures from clients", "Ability to send documents to clients", "Client's ability to schedule appointments", "Other feature(s) not listed here")
ideal_features = data.frame(response_options, ideal_features)
rownames(ideal_features) = NULL
colnames(ideal_features)[2] = "count"
ideal_features$percent = round(ideal_features$percent,2)
ideal_features$percent = paste0(ideal_features$percent*100, "%")

ideal_features = ideal_features[order(ideal_features$count,decreasing = TRUE),]

title_ideal_features = paste0("Please select the features you would like to see in your future ideal televideo platform. (please check all that apply)", " ", "n=", n_clincian_survey_ideal)
table_ideal_features = 
  gt(ideal_features) %>%
  tab_header(title = title_ideal_features)%>%
  tab_footnote(footnote = "Respondents can select all that apply so count / percent can add up to more than total n / 100%.  Only respondents who selected at least televideo.",  locations = cells_body(columns = vars(percent, count), rows = 1)) %>%
  cols_label(response_options = md("Response options"), count = md("Count"), percent = md("Percent"))
table_ideal_features

gtsave(table_ideal_features, "table_ideal_features.png")

```
other_ideal_features
Code later
```{r}

```


ideal_features_no
Would any of these features increase your preference to use televideo in the future? (please check all that apply)
[prefer_service(1)] <> '1' and ([job_title] <> '3' and [service_provided(6)] <> '1')
service_provided__1 == 0 
```{r echo=FALSE}
clincian_survey_ideal_no_dat = subset(clincian_survey_dat, prefer_service___1 == 0)
dim(clincian_survey_ideal_no_dat)
n_clincian_survey_ideal_no = dim(clincian_survey_ideal_no_dat)[1]
ideal_features_no = clincian_survey_ideal_no_dat[,130:137]
ideal_features_no = apply(ideal_features_no, 2, sum)
ideal_features_no = data.frame(ideal_features_no)
ideal_features_no$percent = ideal_features_no$ideal_features_no / n_clincian_survey_ideal_no
response_options =  c("Integration with medical records", "Electronic assessment capabilities", "Ability to conduct group sessions", "Virtual walk-in capabilities", "Ability to collect signatures from clients", "Ability to send documents to clients", "Client's ability to schedule appointments", "Other feature(s) not listed here")
ideal_features_no = data.frame(response_options, ideal_features_no)
rownames(ideal_features_no) = NULL
colnames(ideal_features_no)[2] = "count"
ideal_features_no$percent = round(ideal_features_no$percent,2)
ideal_features_no$percent = paste0(ideal_features_no$percent*100, "%")

ideal_features_no = ideal_features_no[order(ideal_features_no$count,decreasing = TRUE),]

title_ideal_features_no = paste0("Would any of these features increase your preference to use televideo in the future? (please check all that apply)", " ", "n=", n_clincian_survey_ideal_no)
table_ideal_features_no = 
  gt(ideal_features_no) %>%
  tab_header(title = title_ideal_features_no)%>%
  tab_footnote(footnote = "Respondants can select all that apply so count / percent can add up to more than total n / 100%. Only respondents who did not select televideo." ,  locations = cells_body(columns = vars(percent, count), rows = 1)) %>%
  cols_label(response_options = md("Response options"), count = md("Count"), percent = md("Percent"))
table_ideal_features_no

gtsave(table_ideal_features_no, "table_ideal_features_no.png")

```
other_ideal_features_nopref
Code later
```{r}

```


Emotional social support
available_listen
listen_concern
supports_emotions

Instrumental social support
supervision
helpful_advice
actions_decisions

perceived organizational support 
time_resources
contribution
extra_effort

1, Strongly disagree | 2, Disagree | 3, Undecided | 4, Agree | 5, Strongly agree | 6, N/A
```{r echo=FALSE}
supervisor_dat = clincian_survey_dat[,139:147]
supervisor_dat[supervisor_dat == 6] = NA
ess = apply(supervisor_dat[,1:3],1,mean, na.rm = TRUE)
iss = apply(supervisor_dat[,4:6],1,mean, na.rm = TRUE)
pos = apply(supervisor_dat[,7:9],1,mean, na.rm = TRUE)

supervisor_dat = data.frame(ess, iss, pos)
supervisor_dat_p = supervisor_dat
supervisor_dat = apply(supervisor_dat, 2, mean, na.rm = TRUE)
supervisor_dat = data.frame(supervisor_dat)
supervisor_dat
var_names = c("Emotional social support", "Instrumental social support", "Perceived organizational support")
supervisor_dat = data.frame(var_names, supervisor_dat)
rownames(supervisor_dat) = NULL
## Get rid of total
title_supervisor_dat = paste0("Supervison support", " ", "n=", n_clinician_survey)
plot_ess = ggplot(supervisor_dat, aes(x = var_names,y = supervisor_dat, fill = supervisor_dat))+
  geom_bar(stat = "identity")+
  labs(title=title_supervisor_dat, x ="Outcome", y = "Average rating")+
  scale_y_continuous(limits = c(0,5))+
  labs(fill = "")
plot_ess
supervisor_dat_p_complete = data.frame(apply(supervisor_dat_p, 2, function(x){ifelse(x > 3,1,0)}))
supervisor_dat_p_complete = na.omit(supervisor_dat_p_complete)
n_supervisor_dat_p_complete = dim(supervisor_dat_p_complete)[1]
supervisor_dat_p_sum = apply(supervisor_dat_p_complete, 2, sum)
supervisor_dat_p_p = round(supervisor_dat_p_sum / n_supervisor_dat_p_complete,2)
supervisor_dat_p_p
supervisor_dat_p_n_p = rbind(supervisor_dat_p_sum, supervisor_dat_p_p)
rownames(supervisor_dat_p_n_p) = c("N", "%")
supervisor_dat_p_n_p

```

benefits
Code later
For you personally, what are the benefits of using televideo services relative to in-person services?
Cat do first 200
```{r echo=FALSE}
#head(clincian_survey_dat$benefits)

benefits_complete = na.omit(clincian_survey_dat$benefits)
length(benefits_complete)
```
barriers 
Code later
For you personally, what are the barriers of using televideo relative to in-person services?
Jess do first 200
```{r echo=FALSE}
#head(clincian_survey_dat$barriers)
barriers_complete = na.omit(clincian_survey_dat$barriers)
length(barriers_complete)
```
something_else
code later
Is there something else you would like us to know about challenges or successes you are encountering as a result of working in an altered work environment?
```{r echo=FALSE}
#head(clincian_survey_dat$something_else, 15)
something_else_complete = na.omit(clincian_survey_dat$something_else)
length(something_else_complete)
```
State
1, Indiana | 2, Florida | 3, Tennessee | 4, Illinois | 5, Another state
tech_cri_dat_complete
```{r}
state_dat = na.omit(tech_cri_dat_complete$state)
state_dat = data.frame(state = state_dat)
n_state_dat = dim(state_dat)[1]
state_dat = data.frame(freq(state_dat$state))
## Get rid of total change to 6 later
state_dat = state_dat[-6,]
state_dat$var_names = c("Tennessee", "Indiana", "Illinois", "Florida", "Another state")
state_dat$var_names = as.factor(state_dat$var_names)

state_dat$var_names = factor(state_dat$var_names, levels = levels(state_dat$var_names)[5:1])

state_dat$Percent = state_dat$Percent / 100
state_dat$Percent = round(state_dat$Percent, 2)*100
state_dat$Percent = paste0(state_dat$Percent, "%")
typeof(state_dat$Frequency)
title_state = paste0("Which state do you currently live in?", " ", "n=", n_state_dat)
plot_state = ggplot(state_dat, aes(x = var_names,y = Frequency, fill = var_names))+
  geom_bar(stat = "identity")+
  labs(title=title_state, y = "Count", x = "Response option")+
  scale_y_continuous(limits = c(0,1000))+
  theme(legend.position = "none")+
  geom_text_repel(label = state_dat$Percent, vjust = -.5)
plot_state
```
age
What is your age?
1, 18 to 25 years old | 2, 26 to 34 years old | 3, 35 to 44 years old | 4, 45 to 54 years old | 5, 55 to 64 years old | 6, 65 to 74 years old | 7, 75+
```{r}
age_dat = na.omit(tech_cri_dat_complete$age)
age_dat = data.frame(age = age_dat)
n_age_dat = dim(age_dat)[1]
age_dat = data.frame(freq(age_dat$age))
## Get rid of total change to 8 later
age_dat = age_dat[-8,]
age_dat$var_names = c("18 to 25 \n years old", "26 to 34 \n years old", "35 to 44 \n years old", "45 to 54 \n years old", "55 to 64 \n years old", "65 to 74 \n years old", "75+")
age_dat$Percent = age_dat$Percent / 100
age_dat$Percent = round(age_dat$Percent, 2)*100
age_dat$Percent = paste0(age_dat$Percent, "%")
typeof(age_dat$Frequency)
title_age = paste0("What is your age?", " ", "n=", n_age_dat)
plot_age = ggplot(age_dat, aes(x = var_names,y = Frequency, fill = var_names))+
  geom_bar(stat = "identity")+
  labs(title=title_age, y = "Count", x = "Response option")+
  scale_y_continuous(limits = c(0,1000))+
  theme(legend.position = "none")+
  geom_text_repel(label = age_dat$Percent, vjust = -.5)
plot_age


```
race
What is your racial identity?
1, White | 2, Black or African American | 3, American Indian or Alaska Native | 4, Asian | 5, Native Hawaiian or Other Pacific Islander | 6, Multiracial | 7, Another racial identity | 8, Prefer not to respond
```{r echo=FALSE}
race_dat = na.omit(tech_cri_dat_complete$race)
race_dat = data.frame(race = race_dat)
n_race_dat = dim(race_dat)[1]
race_dat = data.frame(freq(race_dat$race))
## Get rid of total change to 9 later
race_dat = race_dat[-9,]
race_dat$var_names = c("White", "Black or African American", "American Indian or \n Alaska Native", "Asian", "Native Hawaiian or \n Other Pacific Islander", "Multiracial", "Another racial identity", "Prefer not to respond")
race_dat$Percent = race_dat$Percent / 100
race_dat$Percent = round(race_dat$Percent, 2)*100
race_dat$Percent = paste0(race_dat$Percent, "%")
typeof(race_dat$Frequency)
title_race = paste0("What is your race?", " ", "n=", n_race_dat)
plot_race = ggplot(race_dat, aes(x = var_names,y = Frequency, fill = var_names))+
  geom_bar(stat = "identity")+
  labs(title=title_race, y = "Count", x = "Response option")+
  scale_y_continuous(limits = c(0,n_race_dat))+
  theme(legend.position = "none")+
  geom_text_repel(label = race_dat$Percent, vjust = -.5)
plot_race


```
Gender
What is your gender identity?
1, Male | 2, Female | 3, Another gender identity | 4, Prefer not to respond
```{r}
gender_dat = na.omit(tech_cri_dat_complete$gender)
gender_dat = data.frame(gender = gender_dat)
n_gender_dat = dim(gender_dat)[1]
gender_dat = data.frame(freq(gender_dat$gender))
## Get rid of total change to 5 later
gender_dat = gender_dat[-5,]
gender_dat$var_names = c("Male", "Female", "Another gender identity", "Prefer not to respond")
gender_dat$Percent = gender_dat$Percent / 100
gender_dat$Percent = round(gender_dat$Percent, 2)*100
gender_dat$Percent = paste0(gender_dat$Percent, "%")
typeof(gender_dat$Frequency)
title_gender = paste0("What is your gender?", " ", "n=", n_gender_dat)
plot_gender = ggplot(gender_dat, aes(x = var_names,y = Frequency, fill = var_names))+
  geom_bar(stat = "identity")+
  labs(title=title_gender, y = "Count", x = "Response option")+
  scale_y_continuous(limits = c(0,n_gender_dat))+
  theme(legend.position = "none")+
  geom_text_repel(label = gender_dat$Percent, vjust = -.5)
plot_gender
```
job_title_extend
Which option best describes your job title?
1, Psychiatrists | 2, Nurse Practitioners | 3, Clinician - Masters, non-licensed | 4, Clinician - Masters, licensed | 5, Clinician - Bachelors | 6, Peer Support Specialist | 7, Another job title
```{r echo=FALSE}
job_title_extend_dat = na.omit(tech_cri_dat_complete$job_title_extend)
job_title_extend_dat = data.frame(job_title_extend = job_title_extend_dat)
n_job_title_extend_dat = dim(job_title_extend_dat)[1]
job_title_extend_dat = data.frame(freq(job_title_extend_dat$job_title_extend))
## Get rid of total change to 8 later
job_title_extend_dat = job_title_extend_dat[-8,]
job_title_extend_dat$var_names = c("Psychiatrists", "Nurse Practitioners", "Clinician - Masters, \n non-licensed", "Clinician - Masters, \n licensed", "Clinician - Bachelors", "Peer Support Specialist", "Another job title")
job_title_extend_dat$Percent = job_title_extend_dat$Percent / 100
job_title_extend_dat$Percent = round(job_title_extend_dat$Percent, 2)*100
job_title_extend_dat$Percent = paste0(job_title_extend_dat$Percent, "%")
typeof(job_title_extend_dat$Frequency)
title_job_title_extend = paste0("Which option best describes your job title?", " ", "n=", n_job_title_extend_dat)
plot_job_title_extend = ggplot(job_title_extend_dat, aes(x = var_names,y = Frequency, fill = var_names))+
  geom_bar(stat = "identity")+
  labs(title=title_job_title_extend, y = "Count", x = "Response option")+
  scale_y_continuous(limits = c(0,1500))+
  theme(legend.position = "none")+
  geom_text_repel(label = job_title_extend_dat$Percent, vjust = -.5)
plot_job_title_extend

```
other_job_title
```{r echo=FALSE}
head(tech_cri_dat_complete[,159],15)

```
