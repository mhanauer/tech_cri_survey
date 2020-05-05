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
Need to rename all variables


