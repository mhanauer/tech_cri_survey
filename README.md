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
tech_cri_dat = read.csv("TelehealthSnapMDZoom_DATA_2020-05-05_1457.csv", header = TRUE)
tech_cri_dat_complete  = subset(tech_cri_dat, my_first_instrument_timestamp != "[not completed]")
head(tech_cri_dat_complete)
```
Get complete n and graphs for job title and situation 
```{r}
n_survey = dim(tech_cri_dat_complete)[1]
n_survey

job_title_dat = na.omit(tech_cri_dat_complete$job_title)
job_title_dat = data.frame(job_title = job_title_dat)

job_title_dat = data.frame(freq(job_title_dat$job_title))
## Get rid of total change to 6 later
job_title_dat = job_title_dat[-4,]
job_title_dat$var_names = c("Client Facing", "Client Facing Medical Provider", "Non Client Facing")
job_title_dat$Frequency = as.factor(job_title_dat$Frequency)
job_title_dat$Percent = job_title_dat$Percent / 100


job_title_dat$Frequency = paste0("n=",job_title_dat$Frequency)
plot_job_title = ggplot(job_title_dat, aes(x = var_names,y = Percent, fill = var_names))+
  geom_bar(stat = "identity")+
  labs(title="Count and percent of job title", y = "Percent", x = "Response option")+
  scale_y_continuous(labels = scales::percent, limits = c(0,1))+scale_fill_manual(values = c("red", "blue", "green"))+
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
## Get rid of total change to 6 later
situation_dat = situation_dat[-4,]
situation_dat$var_names = c("Client Facing", "Client Facing Medical Provider", "Non Client Facing")
situation_dat$Frequency = as.factor(situation_dat$Frequency)
situation_dat$Percent = situation_dat$Percent / 100


situation_dat$Frequency = paste0("n=",situation_dat$Frequency)
plot_situation = ggplot(situation_dat, aes(x = var_names,y = Percent, fill = var_names))+
  geom_bar(stat = "identity")+
  labs(title="Count and percent of job title", y = "Percent", x = "Response option")+
  scale_y_continuous(labels = scales::percent, limits = c(0,1))+scale_fill_manual(values = c("red", "blue", "green"))+
  theme(legend.position = "none")+
  geom_text_repel(label = situation_dat$Frequency, vjust = -.5)
plot_situation


```



Now get an example check all that apply option

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
home_productive$percent = home_productive$home_productive / n_survey

response_option =  c("I have no barriers and am working productively", "I have poor internet/connection", "I lack enabling technology equipment (Examples: monitor, headset, webcam, phone)", "My workspace is not ideal (lacks space, lacks privacy, inadequate furnishing)", "Other")
home_productive = data.frame(response_options, home_productive)
rownames(home_productive) = NULL
colnames(home_productive)[2] = "count"
home_productive
write.csv(home_productive, "home_productive.csv", row.names = FALSE)
```
Next
```{r}
head(tech_cri_dat_complete)
```



