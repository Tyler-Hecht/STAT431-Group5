library(rethinking)
setwd("C:/Users/tyler/Documents/Courses/23S/STAT 431/Project/STAT431-Group5/ordinal_models")

load("rethinking_ordinal_additive_model - all.file")
model.all = model
load("rethinking_ordinal_additive_model - no offense.file")
model.no_offense = model
load("rethinking_ordinal_additive_model - no race.file")
model.no_race = model
load("rethinking_ordinal_additive_model - no sex.file")
model.no_sex = model
load("rethinking_ordinal_additive_model - no veteran.file")
model.no_veteran = model

rm(model)

compare(model.all, model.no_offense, model.no_race, model.no_sex, model.no_veteran)
