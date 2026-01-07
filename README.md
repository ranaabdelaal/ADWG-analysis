# Project C: Post-weaning Diarrhea in Pigs

## Overview
This repository contains R-based analysis of a randomized study on post-weaning diarrhea (PWD) in piglets. The study compares zinc oxide, nutraceuticals, and vaccination-based feeding strategies and evaluates their effects on growth performance (average daily weight gain, ADWG).

## Data
- Piglets live in pens (16 per pen)
- Five treatments (A–E) randomized across 40 pens
- Outcome variables: 
  - `ADWG0021`: 0–21 days post-weaning
  - `ADWG2150`: 21–50 days post-weaning
  - `ADWG0050`: 0–50 days post-weaning
- Load dataset in R:
```r
load("PWD.RData")
