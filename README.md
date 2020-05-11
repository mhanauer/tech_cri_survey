---
title: "tech_cri"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Load in data
```{r}
setwd("T:/CRI_Research/telehealth_evaluation/data_codebooks/satisfaction")
tech_cri_dat = read.csv("TelehealthSnapMDZoom_DATA_2020-05-11_0519.csv", header = TRUE, na.strings = c("", "N/A"))
tech_cri_dat_complete  = subset(tech_cri_dat, my_first_instrument_timestamp != "[not completed]")
head(tech_cri_dat_complete)
```
Check missingness
```{r}
library(naniar)
miss_var_summary(tech_cri_dat_complete)
dim(tech_cri_dat_complete)
dim(tech_cri_dat)

### Clinician survey data 
clincian_survey_dat = subset(tech_cri_dat_complete, job_title != 3)

```


Get complete n and graphs for job title and situation 
```{r}
n_survey = dim(tech_cri_dat_complete)[1]
n_survey

job_title_dat = na.omit(tech_cri_dat_complete$job_title)
length(job_title_dat)
job_title_dat = data.frame(job_title = job_title_dat)

job_title_dat = data.frame(freq(job_title_dat$job_title))
## Get rid of total change to 4 later
job_title_dat = job_title_dat[-4,]
job_title_dat$var_names = c("Client Facing", "Client Facing Medical Provider", "Non Client Facing")
job_title_dat$Frequency = as.factor(job_title_dat$Frequency)
job_title_dat$Percent = job_title_dat$Percent / 100


job_title_dat$Frequency = paste0("n=",job_title_dat$Frequency)
plot_job_title = ggplot(job_title_dat, aes(x = var_names,y = Percent, fill = var_names))+
  geom_bar(stat = "identity")+
  labs(title="Count and percent of job title", y = "Percent", x = "Response option")+
  scale_y_continuous(labels = scales::percent, limits = c(0,1))+
  theme(legend.position = "none")+
  geom_text_repel(label = job_title_dat$Frequency, vjust = -.5)
plot_job_title


```
Graph of situation
1, I am working from home | 2, I am working from a Centerstone office | 3, I am working both from home some days and at a Centerstone office some days | 4, I am not working at all (On Leave or Cannot Work From Home or Office)
```{r}
situation_dat = na.omit(tech_cri_dat_complete$situation)
situation_dat = data.frame(situation = situation_dat)

situation_dat = data.frame(freq(situation_dat$situation))
## Get rid of total change to 5 later
situation_dat = situation_dat[-5,]
situation_dat$var_names = c("Working from home", "Centerstone office", "Working from home and Centerstone office", "Not working")

situation_dat$var_names = factor(situation_dat$var_names, levels = c("Working from home", "Centerstone office", "Working from home and Centerstone office", "Not working"))
situation_dat$Frequency = as.factor(situation_dat$Frequency)
situation_dat$Percent = situation_dat$Percent / 100
### Need this for later
n_why = situation_dat
write.csv(n_why, "n_why.csv", row.names = FALSE)
n_why = read.csv("n_why.csv", header = TRUE)
n_why
situation_dat$Frequency = paste0("n=",situation_dat$Frequency)
plot_situation = ggplot(situation_dat, aes(x = var_names,y = Percent, fill = var_names))+
  geom_bar(stat = "identity")+
  labs(title="Count and percent of job title", y = "Percent", x = "Response option")+
  scale_y_continuous(labels = scales::percent, limits = c(0,1))+
  theme(legend.position = "none")+
  geom_text_repel(label = situation_dat$Frequency, vjust = -.5)
plot_situation


```



Home productivity

1, I have no barriers and am working productively 
2, I have poor internet/connection 
3, I lack enabling technology equipment (Examples: monitor, headset, webcam, phone) 
4, My workspace is not ideal (lacks space, lacks privacy, inadequate furnishing) 
5, Other
```{r}
head(tech_cri_dat_complete)
home_productive =  tech_cri_dat_complete[,6:10]
#home_productive = apply(home_productive, 2, as.factor)
home_productive = apply(home_productive, 2, sum)

home_productive = data.frame(home_productive)
## Divide by number of people working from home
home_productive$percent = home_productive$home_productive / n_why[1,1]

response_options =  c("I have no barriers and am working productively", "I have poor internet/connection", "I lack enabling technology equipment", "My workspace is not ideal", "Other")
home_productive = data.frame(response_options, home_productive)
rownames(home_productive) = NULL
colnames(home_productive)[2] = "count"
home_productive$percent = round(home_productive$percent,2)
home_productive$percent = paste0(home_productive$percent*100, "%")
write.csv(home_productive, "home_productive.csv", row.names = FALSE)
```
Qualitative other barriers at home code later
```{r}
barriers_home_complete =  na.omit(tech_cri_dat_complete$other_barriers_home)
length(barriers_home_complete)
write.csv(barriers_home_complete, "barriers_home_complete.csv", row.names = FALSE)
```
office_why
Take n_why and divide each by that number to get the percent who answered yes to this quesiton. 
n_why[2,1]
1, My job is essential and cannot be performed remotely | 2, I have poor internet/connection at home | 3, I lack enabling technology equipment at home (Examples: monitor, headset, webcam, phone) | 4, My workspace is not ideal at home (lacks space, lacks privacy, inadequate furnishing) | 5, I am choosing (with leadership permission) to continue to work in the office
```{r}
head(tech_cri_dat_complete[,12:16])
tech_cri_dat_complete[,12:16]

head(tech_cri_dat_complete)
office_why = tech_cri_dat_complete[,12:16]
#office_why = apply(office_why, 2, as.factor)
office_why = apply(office_why, 2, sum)
office_why = data.frame(office_why)
office_why$percent = office_why$office_why / n_why[2,1]
response_options =  c("My job is essential and cannot be performed remotely", "I have poor internet/connection at home", "I lack enabling technology equipment at home", "My workspace is not ideal at home", "I am choosing (with leadership permission) to continue to work in the office")
office_why = data.frame(response_options, office_why)
rownames(office_why) = NULL
colnames(office_why)[2] = "count"
office_why$percent = round(office_why$percent,2)
office_why$percent = paste0(office_why$percent*100, "%")
write.csv(office_why, "office_why.csv", row.names = FALSE)
office_why
```
barriers_office
n_why[2,1]
1, I have no barriers and am working productively. | 2, I lack enabling technology equipment in the office (Examples: monitor, headset, webcam, phone | 3, Other
```{r}
head(tech_cri_dat_complete[,17:19])
tech_cri_dat_complete[,12:16]

head(tech_cri_dat_complete)
barriers_office = tech_cri_dat_complete[,17:19]
#barriers_office = apply(barriers_office, 2, as.factor)
barriers_office = apply(barriers_office, 2, sum)
barriers_office = data.frame(barriers_office)
barriers_office$percent = barriers_office$barriers_office / n_why[2,1]
barriers_office
response_options =  c("I have no barriers and am working productively", "I lack enabling technology equipment in the office", "Other")
barriers_office = data.frame(response_options, barriers_office)
rownames(barriers_office) = NULL
colnames(barriers_office)[2] = "count"
barriers_office$percent = round(barriers_office$percent,2)
barriers_office$percent = paste0(barriers_office$percent*100, "%")
write.csv(barriers_office, "barriers_office.csv", row.names = FALSE)
barriers_office
```
other_barriers_office
Please describe the barriers.
Code later
```{r}

```



office_home_why
I am working in the office some days because:
1, Parts of my job cannot be performed remotely | 2, I am rotating with other staff covering office based tasks | 3, I have poor internet/connection at home and need to be on the network for my work. | 4, I lack enabling technology equipment at home (Examples: monitor, headset, webcam, phone). | 5, My workspace is not ideal at home (lacks space, lacks privacy, inadequate furnishing) | 6, I am choosing (with leadership permission) to continue to work in the office part of the time.
n_why[3,1]
```{r}
head(tech_cri_dat_complete[,21:26])
tech_cri_dat_complete[,12:16]

head(tech_cri_dat_complete)
office_home_why = tech_cri_dat_complete[,21:26]
#office_home_why = apply(office_home_why, 2, as.factor)
office_home_why = apply(office_home_why, 2, sum)
office_home_why = data.frame(office_home_why)
office_home_why$percent = office_home_why$office_home_why / n_why[3,1]
office_home_why
response_options =  c("Parts of my job cannot be performed remotely", "I am rotating with other staff covering office based tasks", "I have poor internet/connection at home and need to be on the network for my work.", "I lack enabling technology equipment at home (Examples: monitor, headset, webcam, phone).", "My workspace is not ideal at home (lacks space, lacks privacy, inadequate furnishing)", "I am choosing (with leadership permission) to continue to work in the office part of the time.)")
office_home_why = data.frame(response_options, office_home_why)
rownames(office_home_why) = NULL
colnames(office_home_why)[2] = "count"
office_home_why$percent = round(office_home_why$percent,2)
office_home_why$percent = paste0(office_home_why$percent*100, "%")
write.csv(office_home_why, "office_home_why.csv", row.names = FALSE)
office_home_why

```
barriers_office_home
What barriers if any do you have working in the office?
1, I have no barriers and am working productively. | 2, I lack enabling technology equipment in the office (Examples: monitor, headset, webcam, phone | 3, Other
n_why[3,1]
```{r}
head(tech_cri_dat_complete[,27:29])

head(tech_cri_dat_complete)
barriers_office_home = tech_cri_dat_complete[,27:29]
#barriers_office_home = apply(barriers_office_home, 2, as.factor)
barriers_office_home = apply(barriers_office_home, 2, sum)
barriers_office_home = data.frame(barriers_office_home)
barriers_office_home$percent = barriers_office_home$barriers_office_home / n_why[3,1]
barriers_office_home
response_options =  c("I have no barriers and am working productively.", "I lack enabling technology equipment in the office (Examples: monitor, headset, webcam, phone)", "Other")
barriers_office_home = data.frame(response_options, barriers_office_home)
rownames(barriers_office_home) = NULL
colnames(barriers_office_home)[2] = "count"
barriers_office_home$percent = round(barriers_office_home$percent,2)
barriers_office_home$percent = paste0(barriers_office_home$percent*100, "%")
write.csv(barriers_office_home, "barriers_office_home.csv", row.names = FALSE)
barriers_office_home
```
other_barriers_office_home
Please describe the other barriers.
Code later
```{r}

```


not_working_why
Only two responses makes sense, because people not working are not checking their email.

service_provided
Which of Centerstone's televideo or teleaudio services have you provided to client(s)? (please check all that apply)
1, Telephone only | 2, Zoom video and audio | 3, Zoom audio only | 4, SnapMD video and audio | 5, SnapMD audio only | 6, I do not provide televideo or teleaudio services
```{r}

n_clinician_survey = subset(tech_cri_dat_complete, job_title != 3)
n_clinician_survey = dim(n_clinician_survey)[1]
head(tech_cri_dat_complete[,36:41])
head(tech_cri_dat_complete)
service_provided = tech_cri_dat_complete[,36:41]
#service_provided = apply(service_provided, 2, as.factor)
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
write.csv(service_provided, "service_provided.csv", row.names = FALSE)
service_provided


```
barriers_snap_md_use
We are wondering if there are barriers to using SnapMD?  If there are, please check all that apply. 
1, Difficulty developing treatment plans | 2, Difficulty coordinating services across multiple providers | 3, Client's limited access to technology | 4, Client's limited access to private space | 5, Lack of clear policies for conducting televideo / teleaudio | 6, Lack of security for conducting televideo / teleaudio | 7, Decreased rapport with client(s) | 8, Difficulty gathering data from client(s) | 9, Difficulty accessing client(s) information | 10, Clients are uncomfortable with the technology | 11, Lack of training opportunities | 12, Other barriers not listed above
n = service_provided[4:5,2]
```{r}
head(tech_cri_dat_complete[,42:53])
head(tech_cri_dat_complete)
barriers_snap_md_use = tech_cri_dat_complete[,42:53]
#barriers_snap_md_use = apply(barriers_snap_md_use, 2, as.factor)
barriers_snap_md_use = apply(barriers_snap_md_use, 2, sum)
barriers_snap_md_use = data.frame(barriers_snap_md_use)
barriers_snap_md_use$percent = barriers_snap_md_use$barriers_snap_md_use / sum(service_provided[4:5,2])
barriers_snap_md_use
response_options =  c("Difficulty developing treatment plans", "Difficulty coordinating services across multiple providers", "Client's limited access to technology", "Client's limited access to private space", "Lack of clear policies for conducting televideo / teleaudio", "Lack of security for conducting televideo / teleaudio", "Decreased rapport with client(s)", "Difficulty gathering data from client(s)", "Difficulty accessing client(s) information", "Clients are uncomfortable with the technology", "Lack of training opportunities", "Other barriers not listed above")
barriers_snap_md_use = data.frame(response_options, barriers_snap_md_use)
rownames(barriers_snap_md_use) = NULL
colnames(barriers_snap_md_use)[2] = "count"
barriers_snap_md_use$percent = round(barriers_snap_md_use$percent,2)
barriers_snap_md_use$percent = paste0(barriers_snap_md_use$percent*100, "%")
write.csv(barriers_snap_md_use, "barriers_snap_md_use.csv", row.names = FALSE)
barriers_snap_md_use


```
other_snap_barriers_use
Please list the other barrier(s).
Code later

facilitate_snapmd
We are wondering if SnapMD makes it easier to provide services to clients relative to in person?  If it does, please check all that apply.
1, Increased work life balance | 2, Decreased commute time | 3, Increased schedule flexibility | 4, Increased access to difficult to reach clients | 5, Quicker access to clients | 6, Increased convenience | 7, Other factors not listed above
```{r}
head(tech_cri_dat_complete[,55:61])
head(tech_cri_dat_complete)
facilitate_snapmd = tech_cri_dat_complete[,55:61]
#facilitate_snapmd = apply(facilitate_snapmd, 2, as.factor)
facilitate_snapmd = apply(facilitate_snapmd, 2, sum)
facilitate_snapmd = data.frame(facilitate_snapmd)
facilitate_snapmd$percent = facilitate_snapmd$facilitate_snapmd / sum(service_provided[4:5,2])
facilitate_snapmd
response_options =  c("Increased work life balance", "Decreased commute time", "Increased schedule flexibility", "Increased access to difficult to reach clients", "Quicker access to clients", "Increased convenience", "Other factors not listed above")
facilitate_snapmd = data.frame(response_options, facilitate_snapmd)
rownames(facilitate_snapmd) = NULL
colnames(facilitate_snapmd)[2] = "count"
facilitate_snapmd$percent = round(facilitate_snapmd$percent,2)
facilitate_snapmd$percent = paste0(facilitate_snapmd$percent*100, "%")
write.csv(facilitate_snapmd, "facilitate_snapmd.csv", row.names = FALSE)
facilitate_snapmd



```

other_facilitate_snapmd
Not needed only 9 responses

barriers_zoom_use
We are wondering if there are barriers to using Zoom?  If there are, please check all that apply.
```{r}
head(tech_cri_dat_complete[,63:74])
head(tech_cri_dat_complete)
barriers_zoom_use = tech_cri_dat_complete[,63:74]
#barriers_zoom_use = apply(barriers_zoom_use, 2, as.factor)
barriers_zoom_use = apply(barriers_zoom_use, 2, sum)
barriers_zoom_use = data.frame(barriers_zoom_use)
barriers_zoom_use$percent = barriers_zoom_use$barriers_zoom_use / sum(service_provided[2:3,2])
barriers_zoom_use
response_options =  c("Difficulty developing treatment plans", "Difficulty coordinating services across multiple providers", "Client's limited access to technology", "Client's limited access to private space", "Lack of clear policies for conducting televideo / teleaudio", "Lack of security for conducting televideo / teleaudio", "Decreased rapport with client(s)", "Difficulty gathering data from client(s)", "Difficulty accessing client(s) information", "Clients are uncomfortable with the technology", "Lack of training opportunities", "Other barriers not listed above")
barriers_zoom_use = data.frame(response_options, barriers_zoom_use)
rownames(barriers_zoom_use) = NULL
colnames(barriers_zoom_use)[2] = "count"
barriers_zoom_use$percent = round(barriers_zoom_use$percent,2)
barriers_zoom_use$percent = paste0(barriers_zoom_use$percent*100, "%")
write.csv(barriers_zoom_use, "barriers_zoom_use.csv", row.names = FALSE)
barriers_zoom_use
```
other_zoom_barriers_use
Please list the other barrier(s).
Code later

facilitate_zoom
We are wondering if Zoom makes it easier to provide services to clients relative to in person?  If it does, please check all that apply.
```{r}
head(tech_cri_dat_complete[,76:82])
head(tech_cri_dat_complete)
facilitate_zoom = tech_cri_dat_complete[,76:82]
#facilitate_zoom = apply(facilitate_zoom, 2, as.factor)
facilitate_zoom = apply(facilitate_zoom, 2, sum)
facilitate_zoom = data.frame(facilitate_zoom)
facilitate_zoom$percent = facilitate_zoom$facilitate_zoom / sum(service_provided[2:3,2])
facilitate_zoom
response_options =  c("Increased work life balance", "Decreased commute time", "Increased schedule flexibility", "Increased access to difficult to reach clients", "Quicker access to clients", "Increased convenience", "Other factors not listed above")
facilitate_zoom = data.frame(response_options, facilitate_zoom)
rownames(facilitate_zoom) = NULL
colnames(facilitate_zoom)[2] = "count"
facilitate_zoom$percent = round(facilitate_zoom$percent,2)
facilitate_zoom$percent = paste0(facilitate_zoom$percent*100, "%")
write.csv(facilitate_zoom, "facilitate_zoom.csv", row.names = FALSE)
facilitate_zoom


```
other_facilitate_zoom
Lower than 10% not going to code it 

barriers_snap_md
Since you did not select SnapMD, we are wondering, what are the barriers to using SnapMD? (please check all that apply)
n = service_provided[c(1:3,6),2]
Not SnapMD selected
```{r}
head(tech_cri_dat_complete[,84:95])
head(tech_cri_dat_complete)
barriers_snap_md = tech_cri_dat_complete[,84:95]
#barriers_snap_md = apply(barriers_snap_md, 2, as.factor)
barriers_snap_md = apply(barriers_snap_md, 2, sum)
barriers_snap_md = data.frame(barriers_snap_md)
barriers_snap_md$percent = barriers_snap_md$barriers_snap_md / sum(service_provided[c(1:3,6),2])
barriers_snap_md
response_options =  c("Difficulty developing treatment plans", "Difficulty coordinating services across multiple providers", "Client's limited access to technology", "Client's limited access to private space", "Lack of clear policies for conducting televideo / teleaudio", "Lack of security for conducting televideo / teleaudio", "Decreased rapport with client(s)", "Difficulty gathering data from client(s)", "Difficulty accessing client(s) information", "Clients are uncomfortable with the technology", "Lack of training opportunities", "Other barriers not listed above")
barriers_snap_md = data.frame(response_options, barriers_snap_md)
rownames(barriers_snap_md) = NULL
colnames(barriers_snap_md)[2] = "count"
barriers_snap_md$percent = round(barriers_snap_md$percent,2)
barriers_snap_md$percent = paste0(barriers_snap_md$percent*100, "%")
write.csv(barriers_snap_md, "barriers_snap_md.csv", row.names = FALSE)
barriers_snap_md


```


other_snap_barriers
Other barriers not listed above
code later


barriers_zoom
Since you did not select Zoom, we are wondering, what are the barriers to using Zoom? (please check all that apply)
```{r}
head(tech_cri_dat_complete[,97:108])
head(tech_cri_dat_complete)
barriers_zoom = tech_cri_dat_complete[,97:108]
#barriers_zoom = apply(barriers_zoom, 2, as.factor)
barriers_zoom = apply(barriers_zoom, 2, sum)
barriers_zoom = data.frame(barriers_zoom)
barriers_zoom$percent = barriers_zoom$barriers_zoom / sum(service_provided[c(1, 4:6),2])
barriers_zoom
response_options =  c("Difficulty developing treatment plans", "Difficulty coordinating services across multiple providers", "Client's limited access to technology", "Client's limited access to private space", "Lack of clear policies for conducting televideo / teleaudio", "Lack of security for conducting televideo / teleaudio", "Decreased rapport with client(s)", "Difficulty gathering data from client(s)", "Difficulty accessing client(s) information", "Clients are uncomfortable with the technology", "Lack of training opportunities", "Other barriers not listed above")
barriers_zoom = data.frame(response_options, barriers_zoom)
rownames(barriers_zoom) = NULL
colnames(barriers_zoom)[2] = "count"
barriers_zoom$percent = round(barriers_zoom$percent,2)
barriers_zoom$percent = paste0(barriers_zoom$percent*100, "%")
write.csv(barriers_zoom, "barriers_zoom.csv", row.names = FALSE)
barriers_zoom

```
communicate
Televideo has helped me communicate with my client(s).
1, Strongly disagree | 2, Disagree | 3, Undecided | 4, Agree | 5, Strongly agree | 6, N/A

Let's do an average score and then plot by all four of them
```{r}
### Client survey data
#clincian_survey_dat

library(ggplot2)
head(tech_cri_dat_complete)
head(tech_cri_dat_complete[,110:113])
telehealth_sat_dat = clincian_survey_dat
n_telehealth_sat_dat = dim(telehealth_sat_dat)[1]
## Subset 6, because you are a dumbass!!!!  6 = N/A
telehealth_sat_dat = telehealth_sat_dat[,110:113]
telehealth_sat_dat[telehealth_sat_dat == 6] = NA
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
title_telehealth_sat = paste0("Telehealth satisfaction", " ", "n=", n_telehealth_sat_dat)
plot_telehealth_sat = ggplot(telehealth_sat_dat, aes(x = var_names,y = telehealth_sat_dat, fill = telehealth_sat_dat))+
  geom_bar(stat = "identity")+
  labs(title=title_telehealth_sat, x ="Outcome", y = "Average rating")+
  scale_y_continuous(limits = c(0,5))+
  labs(fill = "")
plot_telehealth_sat

```
comfort_televideo
What is your level of comfort with televideo?
1, Very uncomfortable | 2, Uncomfortable | 3, Somewhat uncomfortable | 4, Neither uncomfortable nor comfortable | 5, Somewhat comfortable | 6, Comfortable | 7, Very comfortable
```{r}

### Client survey data
#clincian_survey_dat
head(clincian_survey_dat[,114:120])
comfort_televideo_dat = na.omit(clincian_survey_dat$comfort_televideo)
comfort_televideo_dat = data.frame(comfort_televideo = comfort_televideo_dat)
n_comfort_televideo_dat = dim(comfort_televideo_dat)[1]
comfort_televideo_dat = data.frame(freq(comfort_televideo_dat$comfort_televideo))
## Get rid of total
comfort_televideo_dat = comfort_televideo_dat[-8,]
var_names =  rownames(comfort_televideo_dat)
comfort_televideo_dat$var_names = var_names
typeof(comfort_televideo_dat$Frequency)
comfort_televideo_dat$Percent = round(comfort_televideo_dat$Percent,2)
comfort_televideo_dat$Percent = paste0(comfort_televideo_dat$Percent, "%")
title_comfort_televideo_dat = paste0("What is your level of comfort with televideo?", " ", "n=", n_comfort_televideo_dat)
#comfort_televideo_dat$Frequency = paste0("n=",comfort_televideo_dat$Frequency)
plot_comfort_televideo = ggplot(comfort_televideo_dat, aes(x = var_names,y = Frequency, fill = var_names))+
  geom_bar(stat = "identity")+
  labs(title=title_comfort_televideo_dat, x ="Response option", y = "N")+
  scale_y_continuous(limits = c(0,n_comfort_televideo_dat))+
  theme(legend.position = "none")+
  geom_text_repel(label = comfort_televideo_dat$Percent, vjust = -.5)
plot_comfort_televideo
```
increase_comfort
Is there something Centerstone can do to increase your comfort level?  If so, please describe what Centerstone can do.
```{r}

```
interest_working_home
What is your level of interest in providing televideo services in the future?
1, Very disinterested | 2, Disinterested | 3, Somewhat disinterested | 4, Neither disinterested nor interested | 5, Somewhat interested | 6, Interested | 7, Very interested

Also include those who service_provided != 6
```{r}
clincian_survey_dat_int = subset(clincian_survey_dat, service_provided___6 != 1)
interest_working_home_dat = na.omit(clincian_survey_dat_int$interest_working_home)
interest_working_home_dat = data.frame(interest_working_home = interest_working_home_dat)
n_interest_working_home_dat = dim(interest_working_home_dat)[1]
interest_working_home_dat = data.frame(freq(interest_working_home_dat$interest_working_home))
## Get rid of total
interest_working_home_dat = interest_working_home_dat[-8,]
var_names =  rownames(interest_working_home_dat)
interest_working_home_dat$var_names = var_names
typeof(interest_working_home_dat$Frequency)
interest_working_home_dat$Percent = round(interest_working_home_dat$Percent,2)
interest_working_home_dat$Percent = paste0(interest_working_home_dat$Percent, "%")
title_interest_working_home_dat = paste0("What is your level of interest in providing televideo services in the future?", " ", "n=", n_interest_working_home_dat)
#interest_working_home_dat$Frequency = paste0("n=",interest_working_home_dat$Frequency)
plot_interest_working_home = ggplot(interest_working_home_dat, aes(x = var_names,y = Frequency, fill = var_names))+
  geom_bar(stat = "identity")+
  labs(title=title_interest_working_home_dat, x ="Response option", y = "N")+
  scale_y_continuous(limits = c(0,n_interest_working_home_dat))+
  theme(legend.position = "none")+
  geom_text_repel(label = interest_working_home_dat$Percent, vjust = -.5)
plot_interest_working_home

```
barriers_work_home
Are there barriers limiting your interest in providing televideo from home in the future?  If so, please list them.
```{r}

```
prefer_service
In the future, how would you prefer to provide services? (please check all that apply)
1, Televideo | 2, Teleaudio | 3, In-person

clincian_survey_dat
```{r}
n_clinician_survey = dim(clincian_survey_dat)[1]
head(tech_cri_dat_complete[,118:120])
prefer_service = tech_cri_dat_complete[,118:120]
#prefer_service = apply(prefer_service, 2, as.factor)
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
write.csv(prefer_service, "prefer_service.csv", row.names = FALSE)
prefer_service

```
ideal_features
Please select the features you would like to see in your future ideal televideo platform. (please check all that apply)
1, Integration with medical records | 2, Electronic assessment capabilities | 3, Ability to conduct group sessions | 4, Virtual walk-in capabilities | 5, Ability to collect signatures from clients | 6, Ability to send documents to clients | 7, Client's ability to schedule appointments | 8, Other feature(s) not listed here
If you select select prefer_service___1 == 1

```{r}
clincian_survey_ideal_dat = subset(clincian_survey_dat, prefer_service___1 == 1)
dim(clincian_survey_ideal_dat)
n_clinician_survey = dim(clincian_survey_ideal_dat)[1]
ideal_features = clincian_survey_ideal_dat[,121:128]
ideal_features = apply(ideal_features, 2, sum)
ideal_features = data.frame(ideal_features)
ideal_features$percent = ideal_features$ideal_features / n_clinician_survey
ideal_features
response_options =  c("Integration with medical records", "Electronic assessment capabilities", "Ability to conduct group sessions", "Virtual walk-in capabilities", "Ability to collect signatures from clients", "Ability to send documents to clients", "Client's ability to schedule appointments", "Other feature(s) not listed here")
ideal_features = data.frame(response_options, ideal_features)
rownames(ideal_features) = NULL
colnames(ideal_features)[2] = "count"
ideal_features$percent = round(ideal_features$percent,2)
ideal_features$percent = paste0(ideal_features$percent*100, "%")
write.csv(ideal_features, "ideal_features.csv", row.names = FALSE)
ideal_features

```
other_ideal_features
Code later

ideal_features_no
Would any of these features increase your preference to use televideo in the future? (please check all that apply)
[prefer_service(1)] <> '1' and ([job_title] <> '3' and [service_provided(6)] <> '1')
service_provided__1 == 0 
```{r}
clincian_survey_ideal_no_dat = subset(clincian_survey_dat, prefer_service___1 == 0)
dim(clincian_survey_ideal_no_dat)
n_clinician_survey = dim(clincian_survey_ideal_no_dat)[1]
ideal_features_no = clincian_survey_ideal_no_dat[,130:137]
ideal_features_no = apply(ideal_features_no, 2, sum)
ideal_features_no = data.frame(ideal_features_no)
ideal_features_no$percent = ideal_features_no$ideal_features_no / n_clinician_survey
ideal_features_no
response_options =  c("Integration with medical records", "Electronic assessment capabilities", "Ability to conduct group sessions", "Virtual walk-in capabilities", "Ability to collect signatures from clients", "Ability to send documents to clients", "Client's ability to schedule appointments", "Other feature(s) not listed here")
ideal_features_no = data.frame(response_options, ideal_features_no)
rownames(ideal_features_no) = NULL
colnames(ideal_features_no)[2] = "count"
ideal_features_no$percent = round(ideal_features_no$percent,2)
ideal_features_no$percent = paste0(ideal_features_no$percent*100, "%")
write.csv(ideal_features_no, "ideal_features_no.csv", row.names = FALSE)
ideal_features_no


```
other_ideal_features_nopref
Code later

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
```{r}
library(ggplot2)
head(clincian_survey_dat)
head(clincian_survey_dat[,139:147])
supervisor_dat = clincian_survey_dat[,139:147]
supervisor_dat[supervisor_dat == 6] = NA
n_supervisor_dat = dim(supervisor_dat)[1]
ess = apply(supervisor_dat[,1:3],1,mean, na.rm = TRUE)
iss = apply(supervisor_dat[,4:6],1,mean, na.rm = TRUE)
pos = apply(supervisor_dat[,7:9],1,mean, na.rm = TRUE)

supervisor_dat = data.frame(ess, iss, pos)
supervisor_dat = apply(supervisor_dat, 2, mean, na.rm = TRUE)
supervisor_dat = data.frame(supervisor_dat)
supervisor_dat
var_names = c("Emotional social support", "Instrumental social support", "Perceived organizational support")
supervisor_dat = data.frame(var_names, supervisor_dat)
rownames(supervisor_dat) = NULL
supervisor_dat
## Get rid of total
title_supervisor_dat = paste0("Supervison support", " ", "n=", n_supervisor_dat)
plot_ess = ggplot(supervisor_dat, aes(x = var_names,y = supervisor_dat, fill = supervisor_dat))+
  geom_bar(stat = "identity")+
  labs(title=title_supervisor_dat, x ="Outcome", y = "Average rating")+
  scale_y_continuous(limits = c(0,5))+
  labs(fill = "")
plot_ess

```

benefits
Code later
```{r}
head(clincian_survey_dat$benefits)

```
barriers 
Code later
```{r}
head(clincian_survey_dat$barriers)
```
something_else
code later
```{r}
head(clincian_survey_dat$something_else, 15)

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
state_dat$var_names = c("Indiana", "Florida", "Tennessee", "Illinois", "Another state")
state_dat$Percent = state_dat$Percent / 100
state_dat$Percent = round(state_dat$Percent, 2)*100
state_dat$Percent = paste0(state_dat$Percent, "%")
typeof(state_dat$Frequency)
title_state = paste0("Which state do you currently live in?", " ", "n=", n_state_dat)
plot_state = ggplot(state_dat, aes(x = var_names,y = Frequency, fill = var_names))+
  geom_bar(stat = "identity")+
  labs(title=title_state, y = "Count", x = "Response option")+
  scale_y_continuous(limits = c(0,n_state_dat))+
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
age_dat$var_names = c("18 to 25 years old", "26 to 34 years old", "35 to 44 years old", "45 to 54 years old", "55 to 64 years old", "65 to 74 years old", "75+")
age_dat$Percent = age_dat$Percent / 100
age_dat$Percent = round(age_dat$Percent, 2)*100
age_dat$Percent = paste0(age_dat$Percent, "%")
typeof(age_dat$Frequency)
title_age = paste0("What is your age?", " ", "n=", n_age_dat)
plot_age = ggplot(age_dat, aes(x = var_names,y = Frequency, fill = var_names))+
  geom_bar(stat = "identity")+
  labs(title=title_age, y = "Count", x = "Response option")+
  scale_y_continuous(limits = c(0,n_age_dat))+
  theme(legend.position = "none")+
  geom_text_repel(label = age_dat$Percent, vjust = -.5)
plot_age


```
race
What is your racial identity?
1, White | 2, Black or African American | 3, American Indian or Alaska Native | 4, Asian | 5, Native Hawaiian or Other Pacific Islander | 6, Multiracial | 7, Another racial identity | 8, Prefer not to respond
```{r}
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
