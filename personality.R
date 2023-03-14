#This code was developed for personality research with Dr. Kinsey Bryant-Lees.
#Original code was inherited from Dr. Bryant-Lees and expanded upon to computate survey results for personality
#research regarding a new type of personality scale. While basic, this syntax accomplishes the calculation of
#reverse scored variables, crobach's alpha scores, and row means of each of the personality traits and their
#associated frames of reference. 

#thetas <- read.csv('~/Downloads/SIOP 2020 total outcomes and thetas.csv')
#names(thetas)[2] <- "ResponseId"
#data <- read.csv('~/Downloads/Personality Measurement and FOR - MTurk_October 5, 2021_14.17.csv')
#other <- read.csv('~/Downloads/Personality Measurement - IR Tree Format Only - MTurk_October 5, 2021_13.59.csv')

#IRdat <- data %>% filter(Q2.1_1 >= 1)
#IRdat <- IRdat[, c(1:248, 403:620)]

#all.dat <- rbind(IRdat, other)
#all.dat <- merge(all.dat, thetas, by = "ResponseId")

#Set working directory for opening files
setwd('C:\\School\\NKU\\Independent Study')

#Install Packages
library(dplyr) # This package is used for data manipulation and compututation of new variables, etc
library(psych) # This package is used to import the command for cronbach's alpha

#load data from downloads or wherever you set your working directory
all.dat <- read.csv("Combined IR Tree + FOR data.csv") #Original File
Q3.dat <- read.csv ("5-point scale personality and FOR (MTurk) - 10.25.22.csv") #Q3 File

# Q17_1 through Q17_72 = work frame of reference personality items
# Q18_1 through Q18_66 = social frame of reference personality items
# Q3_1 through Q3_77 = global frame of reference personality items

#This subsets data files so that you can look at social FOR items and work FOR items separately

social.FOR <- all.dat[,c(2, 250:315)]
work.FOR <- all.dat[, c(2, 316:387)]
global.FOR <- Q3.dat[, c(2, 18:94)]

#### Conscientiousness ####

#Conscientiousness work = Q17_1 through Q17_9
#Reverse code Q17_5 through Q17_9

work.FOR$Q17_5R <- 6-work.FOR$Q17_5
work.FOR$Q17_6R <- 6-work.FOR$Q17_6
work.FOR$Q17_7R <- 6-work.FOR$Q17_7
work.FOR$Q17_8R <- 6-work.FOR$Q17_8
work.FOR$Q17_9R <- 6-work.FOR$Q17_9

w.cons.alpha <- alpha(work.FOR[, c("Q17_1", "Q17_2", "Q17_3","Q17_4", "Q17_5R", "Q17_6R", "Q17_7R", "Q17_8R", "Q17_9R")])
w.cons.alpha
work.FOR$cons_work <- rowMeans(work.FOR[, c("Q17_1", "Q17_2", "Q17_3", "Q17_4", "Q17_5R", "Q17_6R", "Q17_7R", "Q17_8R", "Q17_9R")], na.rm=T)

#Conscientiousness social = Q18_1 through Q18_5

s.cons.alpha <- alpha(social.FOR[, c("Q18_1", "Q18_2", "Q18_3", "Q18_4", "Q18_5")])
s.cons.alpha
social.FOR$cons_soc <- rowMeans(social.FOR[, c("Q18_1", "Q18_2", "Q18_3", "Q18_4", "Q18_5")], na.rm = T)

#Conscientiousness global = Q3.1_1 through Q3.1_10
#Reverse code Q3.1_6 through Q3.1_10

global.FOR$Q3.1_6R <- 6-global.FOR$Q3.1_6
global.FOR$Q3.1_7R <- 6-global.FOR$Q3.1_7
global.FOR$Q3.1_8R <- 6-global.FOR$Q3.1_8
global.FOR$Q3.1_9R <- 6-global.FOR$Q3.1_9
global.FOR$Q3.1_10R <- 6-global.FOR$Q3.1_10

g.cons.alpha <- alpha(global.FOR[, c("Q3.1_1", "Q3.1_2", "Q3.1_3", "Q3.1_4", "Q3.1_5", "Q3.1_6R", "Q3.1_7R", "Q3.1_8R", "Q3.1_9R", "Q3.1_10R")])
g.cons.alpha
global.FOR$cons_glob <- rowMeans(global.FOR[, c("Q3.1_1", "Q3.1_2", "Q3.1_3", "Q3.1_4", "Q3.1_5", "Q3.1_6R", "Q3.1_7R", "Q3.1_8R", "Q3.1_9R", "Q3.1_10R")], na.rm=T)

#### PERFECTION ####

#Perfection work = Q17_10 through Q17_16
#Reverse code Q17_13 through Q17_16

work.FOR$Q17_13R <- 6-work.FOR$Q17_13
work.FOR$Q17_14R <- 6-work.FOR$Q17_14
work.FOR$Q17_15R <- 6-work.FOR$Q17_15
work.FOR$Q17_16R <- 6-work.FOR$Q17_16

w.perf.alpha <- alpha(work.FOR[, c("Q17_13R","Q17_14R", "Q17_15R", "Q17_16R")])
w.perf.alpha
work.FOR$perf_work <- rowMeans(work.FOR[, c("Q17_13R","Q17_14R", "Q17_15R", "Q17_16R")], na.rm=T)

#Perfection social = Q18_8 through Q18_13
#Reverse code Q18_11 through Q18_13

social.FOR$Q18_11R <- 6-social.FOR$Q18_11
social.FOR$Q18_12R <- 6-social.FOR$Q18_12
social.FOR$Q18_13R <- 6-social.FOR$Q18_13

s.perf.alpha <- alpha(social.FOR[, c("Q18_8","Q18_9","Q18_10", "Q18_11", "Q18_12", "Q18_13")])
s.perf.alpha
social.FOR$perf_soc <- rowMeans(social.FOR[, c("Q18_8","Q18_9","Q18_10", "Q18_11", "Q18_12", "Q18_13")], na.rm = T)

#Perfection global = Q3.1_11 through Q3.1_19
#Reverse code Q3.1_15 through Q3.1_19

global.FOR$Q3.1_15R <- 6-global.FOR$Q3.1_15
global.FOR$Q3.1_16R <- 6-global.FOR$Q3.1_16
global.FOR$Q3.1_17R <- 6-global.FOR$Q3.1_17
global.FOR$Q3.1_18R <- 6-global.FOR$Q3.1_18
global.FOR$Q3.1_19R <- 6-global.FOR$Q3.1_19

g.perf.alpha <- alpha(global.FOR[, c("Q3.1_11", "Q3.1_12", "Q3.1_15R", "Q3.1_16R", "Q3.1_17R", "Q3.1_18R", "Q3.1_19R")])
g.perf.alpha
global.FOR$perf_glob <- rowMeans(global.FOR[, c("Q3.1_11", "Q3.1_12", "Q3.1_15R", "Q3.1_16R", "Q3.1_17R", "Q3.1_18R", "Q3.1_19R")], na.rm=T)

#### SOCIAL DOMINANCE #####

#Dominance work = Q17_17 through Q17_25
#Reverse code Q17_22, Q17_24, Q17_25

work.FOR$Q17_22R <- 6-work.FOR$Q17_22
work.FOR$Q17_24R <- 6-work.FOR$Q17_24
work.FOR$Q17_25R <- 6-work.FOR$Q17_25

w.dom.alpha <- alpha(work.FOR[, c("Q17_17", "Q17_18", "Q17_19", "Q17_20","Q17_21", "Q17_23")])
w.dom.alpha
work.FOR$dom_work <- rowMeans(work.FOR[, c("Q17_17", "Q17_18", "Q17_19", "Q17_20", "Q17_21", "Q17_23")], na.rm=T)
#For work, remove items 21 and 24 # ?????

#Dominance social = Q18_14 through Q18_21
#Reverse code Q18_18, Q18_20, Q18_21

social.FOR$Q18_18R <- 6-social.FOR$Q18_18
social.FOR$Q18_20R <- 6-social.FOR$Q18_20
social.FOR$Q18_21R <- 6-social.FOR$Q18_21

s.dom.alpha <- alpha(social.FOR[, c("Q18_14", "Q18_15", "Q18_16", "Q18_17", "Q18_19","Q18_21")])
s.dom.alpha
social.FOR$dom_soc <- rowMeans(social.FOR[, c("Q18_14", "Q18_15", "Q18_16", "Q18_17",  "Q18_19", "Q18_21")], na.rm = T)
#For social, Remove item 13 # ?????

#Dominance global = Q3.1_20 through Q3.1_29
#Reverse code Q3.1_26, Q3.1_28, Q3.1_29

global.FOR$Q3.1_26R <- 6-global.FOR$Q3.1_26
global.FOR$Q3.1_28R <- 6-global.FOR$Q3.1_28
global.FOR$Q3.1_29R <- 6-global.FOR$Q3.1_29

g.dom.alpha <- alpha(global.FOR[, c("Q3.1_20", "Q3.1_21", "Q3.1_22", "Q3.1_23", "Q3.1_24", "Q3.1_25", "Q3.1_26R", "Q3.1_27", "Q3.1_28R")])
g.dom.alpha
global.FOR$dom_glob <- rowMeans(global.FOR[, c("Q3.1_20", "Q3.1_21", "Q3.1_22", "Q3.1_23", "Q3.1_24", "Q3.1_25", "Q3.1_26R", "Q3.1_27", "Q3.1_28R")], na.rm=T)

#### EMOTIONAL STABILITY ####
#Emotional work = Q17_26 through Q17_34
#Reverse code Q17_26, Q17_30 through Q17_34

work.FOR$Q17_26R <- 6-work.FOR$Q17_26
work.FOR$Q17_30R <- 6-work.FOR$Q17_30
work.FOR$Q17_31R <- 6-work.FOR$Q17_31
work.FOR$Q17_32R <- 6-work.FOR$Q17_32
work.FOR$Q17_33R <- 6-work.FOR$Q17_33
work.FOR$Q17_34R <- 6-work.FOR$Q17_34

w.emot.alpha <- alpha(work.FOR[, c("Q17_26R", "Q17_27", "Q17_28", "Q17_29", "Q17_30R", "Q17_31R", "Q17_32R", "Q17_33R", "Q17_34R")])
w.emot.alpha
work.FOR$emot_work <- rowMeans(work.FOR[, c("Q17_26R", "Q17_27", "Q17_28", "Q17_29", "Q17_30R", "Q17_31R", "Q17_32R", "Q17_33R", "Q17_34R")], na.rm = T)

#Emotional social = Q18_22 through Q18_30
#Reverse code Q18_22, Q18_26 through Q18_30

social.FOR$Q18_22R <- 6-social.FOR$Q18_22
social.FOR$Q18_26R <- 6-social.FOR$Q18_26
social.FOR$Q18_27R <- 6-social.FOR$Q18_27
social.FOR$Q18_28R <- 6-social.FOR$Q18_28
social.FOR$Q18_29R <- 6-social.FOR$Q18_29
social.FOR$Q18_30R <- 6-social.FOR$Q18_30

s.emot.alpha <- alpha(social.FOR[, c("Q18_22R", "Q18_23", "Q18_24", "Q18_25", "Q18_26R", "Q18_27R", "Q18_28R", "Q18_29R", "Q18_30R")])
s.emot.alpha
social.FOR$emot_soc <- rowMeans(social.FOR[, c("Q18_22R", "Q18_23", "Q18_24", "Q18_25", "Q18_26R", "Q18_27R", "Q18_28R", "Q18_29R", "Q18_30R")])

#Emotional global = Q3.1_30 through Q3.1_38
#Reverse code Q3.1_30, Q3.1_34 through Q3.1_38

global.FOR$Q3.1_30R <- 6-global.FOR$Q3.1_30
global.FOR$Q3.1_34R <- 6-global.FOR$Q3.1_34
global.FOR$Q3.1_35R <- 6-global.FOR$Q3.1_35
global.FOR$Q3.1_36R <- 6-global.FOR$Q3.1_36
global.FOR$Q3.1_37R <- 6-global.FOR$Q3.1_37
global.FOR$Q3.1_38R <- 6-global.FOR$Q3.1_38

g.emot.alpha <- alpha(global.FOR[, c("Q3.1_30R", "Q3.1_31", "Q3.1_32", "Q3.1_33", "Q3.1_34R", "Q3.1_35R", "Q3.1_36R", "Q3.1_37R", "Q3.1_38R")])
g.emot.alpha
global.FOR$emot_glob <- rowMeans(global.FOR[, c("Q3.1_30R", "Q3.1_31", "Q3.1_32", "Q3.1_33", "Q3.1_34R", "Q3.1_35R", "Q3.1_36R", "Q3.1_37R", "Q3.1_38R")], na.rm=T)

#### WARMTH ####
# Warmth work = Q17_35 and Q17_36, Q17_38 through Q17_44 
# Reverse code Q17_42 and Q17_43

work.FOR$Q17_42R <- 6-work.FOR$Q17_42
work.FOR$Q17_43R <- 6-work.FOR$Q17_43

w.warm.alpha <- alpha(work.FOR[, c("Q17_35", "Q17_36", "Q17_38", "Q17_39", "Q17_40", "Q17_41", "Q17_44")])
w.warm.alpha
work.FOR$warm_work <- rowMeans(work.FOR[, c("Q17_35", "Q17_36", "Q17_38", "Q17_39", "Q17_40", "Q17_41", "Q17_44")], na.rm = T)

# Warmth social = Q18_31, Q18_32, Q18_34 through Q18_40
# Reverse Code Q18_38 and Q18_39

social.FOR$Q18_38R <- 6-social.FOR$Q18_38
social.FOR$Q18_39R <- 6-social.FOR$Q18_39

s.warm.alpha <- alpha(social.FOR[, c("Q18_31", "Q18_32", "Q18_34", "Q18_35", "Q18_36", "Q18_37", "Q18_40")])
s.warm.alpha
social.FOR$warm_soc <- rowMeans(social.FOR[, c("Q18_31", "Q18_32", "Q18_34", "Q18_35", "Q18_36", "Q18_37", "Q18_40")])

# Warmth global = Q3.1_39 through Q3.1_48
# Reverse code Q3.1_46 and Q3.1_47

global.FOR$Q3.1_46R <- 6-global.FOR$Q3.1_46
global.FOR$Q3.1_47R <- 6-global.FOR$Q3.1_47

g.warm.alpha <- alpha(global.FOR[, c("Q3.1_39", "Q3.1_40", "Q3.1_41", "Q3.1_42", "Q3.1_43", "Q3.1_44", "Q3.1_45", "Q3.1_46R", "Q3.1_47R", "Q3.1_48")])
g.warm.alpha
global.FOR$warm_glob <- rowMeans(global.FOR[, c("Q3.1_39", "Q3.1_40", "Q3.1_41", "Q3.1_42", "Q3.1_43", "Q3.1_44", "Q3.1_45", "Q3.1_46R", "Q3.1_47R", "Q3.1_48")], na.rm=T)

#### Self-Reliance #####
#Reliance work = Q17_45 through Q17_54 
#Reverse Code Q17_52 through Q17_54

work.FOR$Q17_52R <- 6-work.FOR$Q17_52
work.FOR$Q17_53R <- 6-work.FOR$Q17_53
work.FOR$Q17_54R <- 6-work.FOR$Q17_54

w.rel.alpha <- alpha(work.FOR[, c("Q17_45", "Q17_46", "Q17_47", "Q17_48", "Q17_49", "Q17_50", "Q17_51", "Q17_52R", "Q17_53R", "Q17_54R")])
w.rel.alpha
work.FOR$rel_work <- rowMeans(work.FOR[, c("Q17_45", "Q17_46", "Q17_47", "Q17_48", "Q17_49", "Q17_50", "Q17_51", "Q17_52R", "Q17_53R", "Q17_54R")], na.rm = T)

#Reliance social = Q18_41 through Q18_49
#Reverse Code Q18_48 and Q18_49

social.FOR$Q18_48R <- 6-social.FOR$Q18_48
social.FOR$Q18_49R <- 6-social.FOR$Q18_49

s.rel.alpha <- alpha(social.FOR[, c("Q18_41", "Q18_42", "Q18_43", "Q18_44", "Q18_46", "Q18_47")])
s.rel.alpha
social.FOR$rel_soc <- rowMeans(social.FOR[, c("Q18_41", "Q18_42", "Q18_43", "Q18_44", "Q18_46", "Q18_47")])

# Reliance global = Q3.1_49 through Q3.1_58
# Reverse code Q3.1_56 through Q3.1_58

global.FOR$Q3.1_56R <- 6-global.FOR$Q3.1_56
global.FOR$Q3.1_57R <- 6-global.FOR$Q3.1_57
global.FOR$Q3.1_58R <- 6-global.FOR$Q3.1_58

g.rel.alpha <- alpha(global.FOR[, c("Q3.1_49", "Q3.1_50", "Q3.1_51", "Q3.1_52", "Q3.1_53", "Q3.1_54", "Q3.1_55", "Q3.1_56R", "Q3.1_58R")])
g.rel.alpha
global.FOR$rel_glob <- rowMeans(global.FOR[, c("Q3.1_49", "Q3.1_50", "Q3.1_51", "Q3.1_52", "Q3.1_53", "Q3.1_54", "Q3.1_55", "Q3.1_56R", "Q3.1_58R")], na.rm=T)

##### Social Boldness #####
#Boldness work = Q17_55 through Q17_64
#Reverse Code Q17_60 through Q17_64

work.FOR$Q17_60R <- 6-work.FOR$Q17_60
work.FOR$Q17_61R <- 6-work.FOR$Q17_61
work.FOR$Q17_62R <- 6-work.FOR$Q17_62
work.FOR$Q17_63R <- 6-work.FOR$Q17_63
work.FOR$Q17_64R <- 6-work.FOR$Q17_64

w.bold.alpha <- alpha(work.FOR[, c("Q17_55", "Q17_56", "Q17_58", "Q17_59", "Q17_60R", "Q17_61R", "Q17_62R", "Q17_63R", "Q17_64R")])
w.bold.alpha
work.FOR$bold_work <- rowMeans(work.FOR[, c("Q17_55", "Q17_56", "Q17_58", "Q17_59", "Q17_60R", "Q17_61R", "Q17_62R", "Q17_63R", "Q17_64R")], na.rm = T)

#Boldness social = Q18_50 through Q18_59
#Reverse Code Q18_55 through Q18_59

social.FOR$Q18_55R <- 6-social.FOR$Q18_55
social.FOR$Q18_56R <- 6-social.FOR$Q18_56
social.FOR$Q18_57R <- 6-social.FOR$Q18_57
social.FOR$Q18_58R <- 6-social.FOR$Q18_58
social.FOR$Q18_59R <- 6-social.FOR$Q18_59

s.bold.alpha <- alpha(social.FOR[, c("Q18_50", "Q18_51", "Q18_52", "Q18_53", "Q18_54", "Q18_55R", "Q18_56R", "Q18_57R", "Q18_58R", "Q18_59R")])
s.bold.alpha
social.FOR$bold_soc <- rowMeans(social.FOR[, c("Q18_50", "Q18_51", "Q18_52", "Q18_53", "Q18_54", "Q18_55R", "Q18_56R", "Q18_57R", "Q18_58R", "Q18_59R")])

# Boldness global = Q3.1_59 through Q3.1_68
# Reverse code Q3.1_64 through Q3.1_68

global.FOR$Q3.1_64R <- 6-global.FOR$Q3.1_64
global.FOR$Q3.1_65R <- 6-global.FOR$Q3.1_65
global.FOR$Q3.1_66R <- 6-global.FOR$Q3.1_66
global.FOR$Q3.1_67R <- 6-global.FOR$Q3.1_67
global.FOR$Q3.1_68R <- 6-global.FOR$Q3.1_68

g.bold.alpha <- alpha(global.FOR[, c("Q3.1_59", "Q3.1_60", "Q3.1_61", "Q3.1_62", "Q3.1_63", "Q3.1_64R", "Q3.1_65R", "Q3.1_66R", "Q3.1_67R", "Q3.1_68R")])
g.bold.alpha
global.FOR$bold_glob <- rowMeans(global.FOR[, c("Q3.1_59", "Q3.1_60", "Q3.1_61", "Q3.1_62", "Q3.1_63", "Q3.1_64R", "Q3.1_65R", "Q3.1_66R", "Q3.1_67R", "Q3.1_68R")], na.rm=T)

##### Openness #####
#Openness work = Q17_65 through Q17_72 
#Reverse Code Q17_69 and Q17_72

work.FOR$Q17_69R <- 6-work.FOR$Q17_69
work.FOR$Q17_72R <- 6-work.FOR$Q17_72

w.open.alpha <- alpha(work.FOR[, c("Q17_65", "Q17_66", "Q17_67", "Q17_68", "Q17_70", "Q17_71")])
w.open.alpha
work.FOR$open_work <- rowMeans(work.FOR[, c("Q17_65", "Q17_66", "Q17_67", "Q17_68", "Q17_70", "Q17_71")], na.rm = T)

#Openness social = Q18_60 through Q18_66
#Reverse Code Q18_63 and Q18_66

social.FOR$Q18_63R <- 6-social.FOR$Q18_63
social.FOR$Q18_66R <- 6-social.FOR$Q18_66

s.open.alpha <- alpha(social.FOR[, c("Q18_60", "Q18_61", "Q18_62", "Q18_63", "Q18_64", "Q18_65", "Q18_66")])
s.open.alpha
social.FOR$open_soc <- rowMeans(social.FOR[, c("Q18_60", "Q18_61", "Q18_62", "Q18_63", "Q18_64", "Q18_65", "Q18_66")])

# Openness global = Q3.1_69 through Q3.1_77
# Reverse code Q3.1_73 and Q3.1_77

global.FOR$Q3.1_73R <- 6-global.FOR$Q3.1_73
global.FOR$Q3.1_77R <- 6-global.FOR$Q3.1_77

g.open.alpha <- alpha(global.FOR[,c("Q3.1_69", "Q3.1_70", "Q3.1_71", "Q3.1_72", "Q3.1_73R", "Q3.1_74", "Q3.1_75", "Q3.1_76", "Q3.1_77R")])
g.open.alpha
global.FOR$open_glob <- rowMeans(global.FOR[, c("Q3.1_69", "Q3.1_70", "Q3.1_71", "Q3.1_72", "Q3.1_73R", "Q3.1_74", "Q3.1_75", "Q3.1_76", "Q3.1_77R")], na.rm=T)

#This syntax is used to export the results of analysis to a dataframe in a CSV file.

exportDataFrame1 <- data.frame (work.FOR$cons_work, social.FOR$cons_soc,work.FOR$dom_work,social.FOR$dom_soc,work.FOR$emot_work, social.FOR$emot_soc, work.FOR$warm_work, social.FOR$warm_soc, work.FOR$rel_work, social.FOR$rel_soc, work.FOR$bold_work, social.FOR$bold_soc, work.FOR$open_work, social.FOR$open_soc)
exportDataFrame1

exportDataFrame2 <- data.frame (global.FOR$cons_glob, global.FOR$dom_glob, global.FOR$emot_glob, global.FOR$warm_glob, global.FOR$rel_glob, global.FOR$bold_glob, global.FOR$open_glob)
exportDataFrame2

write.csv(exportDataFrame1, "C:\\School\\NKU\\Independent Study\\workandsoc.csv", row.names=FALSE)
write.csv(exportDataFrame2, "C:\\School\\NKU\\Independent Study\\glob.csv", row.names=FALSE)