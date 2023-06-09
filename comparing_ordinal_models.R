library(rethinking)
setwd("C:/Users/tyler/Documents/Courses/23S/STAT 431/Project/STAT431-Group5/ordinal_models")

load("rethinking_ordinal_additive_model - all.file")
model.all = model
load("rethinking_ordinal_additive_model - no race.file")
model.no_race = model
load("rethinking_ordinal_additive_model - no sex.file")
model.no_sex = model
load("rethinking_ordinal_additive_model - no veteran.file")
model.no_veteran = model
load("rethinking_ordinal_additive_model - no class.file")
model.no_class = model
load("rethinking_ordinal_additive_model - no offense.file")
model.no_offense = model
load("rethinking_ordinal_additive_model - no region.file")
model.no_region = model
load("rethinking_ordinal_additive_model - no age.file")
model.no_age = model

rm(model)

compare(model.all, model.no_race, model.no_sex, model.no_veteran,
        model.no_class, model.no_offense, model.no_region, model.no_age)
