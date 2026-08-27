library(crayon)
library(magrittr)
library(tidyverse)
library(patchwork)
library(effectsize)
library(lme4)
library(lmerTest)
library(car)
library(emmeans)
library(effects)
library(igraph)
library(aricode)
library(future.apply)
library(openxlsx)


#### prep ----

setwd('/Users/zhshi/GoogleDrive/# Documents/2026/OUD rest/R')
source('R.Analysis_Prep.R')

datBehav <- read.csv("R.Source_Behav.csv", na.strings = c("NA","NaN","nan","#N/A","")) %>% 
	dplyr::mutate(across(c(Subj, Study, Grp, HxOD, Sex, Race, OpiUD, AlcUD, StmUD, CanUD, IsSmoker, Medication), as.factor))
datSegNet <- read.csv("R.Data_SegNet.csv",na.strings = c("NA","NaN","nan","#N/A","")) %>%
	dplyr::mutate(across(c(Subj), as.factor))
datSegNodal <- read.csv("R.Data_SegNodal.csv",na.strings = c("NA","NaN","nan","#N/A","")) %>%
	dplyr::mutate(across(c(Subj), as.factor))
datGraph <- read.csv("R.Data_Graph.csv",na.strings = c("NA","NaN","nan","#N/A","")) %>%
	dplyr::mutate(across(c(Subj), as.factor))

stopifnot(
	identical(datBehav$Subj, datSegNet$Subj),
	identical(datBehav$Subj, datSegNodal$Subj),
	identical(datBehav$Subj, datGraph$Subj)
)

dat <- datBehav %>% 
	dplyr::left_join(datSegNet, by="Subj") %>%
	dplyr::left_join(datSegNodal, by="Subj") %>%
	dplyr::left_join(datGraph, by="Subj")

rm(datBehav, datSegNet, datSegNodal, datSegNodal)

dat %<>% dplyr::filter(!(FD_mean > 0.5))


#### descrip ----

varIgnore <- names(dat)[!names(dat) %in% c('Study','Age','Sex','Race','FD_mean','OpiUD','AlcUD','CanUD','StmUD','IsSmoker','YrsOpUse','Medication','HxOD')]
Tb.Descrip <- shiRDescriptivePrint(dat,GrpVar="Grp",Ignore=varIgnore,NumTest="t.test",CatTest="chisq.test")


#### func def ----

func_FdrBy <- function(p, pname, pgrp=NULL) {
	pfdr <- length(p) %>% numeric() + NA
	if (is.null(pgrp)) { return(pfdr) }
	for (i in 1:length(pgrp)) {
		idx <- pname %in% pgrp[[i]]
		pfdr[idx] <- p.adjust(p[idx], method="fdr")
	}
	return(pfdr)
}

func_VarList <- function(dat, pattern) {
	return(grep(paste0(pattern), names(dat), value=T))
}


#### var def ----

C <- c("Age","Sex","Race","FD_mean","StmUD","IsSmoker","RestTr","RestDur")
RFX <- "(1|Study)"

Var <- c(
	dat %>% func_VarList("^SegOverall_"),
	dat %>% func_VarList("^SegIndivid_"),
	dat %>% func_VarList("^SegNodal_"),
	dat %>% func_VarList("^Graph_")
)

VarGrp <- list(
		dat %>% func_VarList("^SegIndivid_As"),
		dat %>% func_VarList("^SegIndivid_Sm"),
		dat %>% func_VarList("^SegNodal_AsCo"),
		dat %>% func_VarList("^SegNodal_AsDm"),
		dat %>% func_VarList("^SegNodal_AsVa"),
		dat %>% func_VarList("^SegNodal_SmHa"),
		dat %>% func_VarList("^SegNodal_SmVi")
)


#### Grp x Age ----

mdl.AgeInt <- list()
Tb.AgeInt = data.frame(
	Var                  = length(Var) %>% character(),
	F_AgeInt             = length(Var) %>% numeric(),
	df1_AgeInt           = length(Var) %>% numeric(),
	df2_AgeInt           = length(Var) %>% numeric(),
	p_AgeInt             = length(Var) %>% numeric(),
	pprint_AgeInt        = length(Var) %>% character(),
	pFDR_AgeInt          = length(Var) %>% numeric(),
	pFDRprint_AgeInt     = length(Var) %>% character(),
	eta2_AgeInt          = length(Var) %>% numeric(),
	emtr_Age_HC          = length(Var) %>% numeric(),
	emtr_Age_HC_LCI      = length(Var) %>% numeric(),
	emtr_Age_HC_UCI      = length(Var) %>% numeric(),
	emtr_Age_OUD         = length(Var) %>% numeric(),
	emtr_Age_OUD_LCI     = length(Var) %>% numeric(),
	emtr_Age_OUD_UCI     = length(Var) %>% numeric(),
	STRING = length(Var) %>% character()
)

for (i in 1:length(Var)) {
	
	mdl.AgeInt[[Var[i]]] <- lmer(shiRFormula(Var[i],c("Grp*Age",C,RFX)), data = dat)
	anv <- mdl.AgeInt[[Var[i]]] %>% anova()
	emtr <- mdl.AgeInt[[Var[i]]] %>% emmeans::emtrends(~Grp,"Age") %>% summary()
	eta2 <- effectsize::eta_squared(mdl.AgeInt[[Var[i]]], partial=T)
	
	Tb.AgeInt$Var                [i] <- Var[i]
	Tb.AgeInt$F_AgeInt           [i] <- anv["Grp:Age","F value"]
	Tb.AgeInt$df1_AgeInt         [i] <- anv["Grp:Age","NumDF"]
	Tb.AgeInt$df2_AgeInt         [i] <- anv["Grp:Age","DenDF"]
	Tb.AgeInt$p_AgeInt           [i] <- anv["Grp:Age","Pr(>F)"]
	Tb.AgeInt$eta2_AgeInt        [i] <- eta2 %>% dplyr::filter(Parameter=="Grp:Age") %>% {.$Eta2_partial}
	Tb.AgeInt$emtr_Age_HC        [i] <- emtr %>% dplyr::filter(Grp=="HC") %>% {.$Age.trend}
	Tb.AgeInt$emtr_Age_HC_LCI    [i] <- emtr %>% dplyr::filter(Grp=="HC") %>% {.$lower.CL}
	Tb.AgeInt$emtr_Age_HC_UCI    [i] <- emtr %>% dplyr::filter(Grp=="HC") %>% {.$upper.CL}
	Tb.AgeInt$emtr_Age_OUD       [i] <- emtr %>% dplyr::filter(Grp=="OUD") %>% {.$Age.trend}
	Tb.AgeInt$emtr_Age_OUD_LCI   [i] <- emtr %>% dplyr::filter(Grp=="OUD") %>% {.$lower.CL}
	Tb.AgeInt$emtr_Age_OUD_UCI   [i] <- emtr %>% dplyr::filter(Grp=="OUD") %>% {.$upper.CL}
	
}

Tb.AgeInt$pFDR_AgeInt <- func_FdrBy(p = Tb.AgeInt$p_AgeInt, pname = Tb.AgeInt$Var, pgrp = VarGrp)
Tb.AgeInt$pprint_AgeInt <- shiRFormatPval(Tb.AgeInt$p_AgeInt)
Tb.AgeInt$pFDRprint_AgeInt <- shiRFormatPval(Tb.AgeInt$pFDR_AgeInt)
Tb.AgeInt %<>% dplyr::mutate(STRING=gsub(', pFDRNA','',gsub('-','–',sprintf(
	"F(%d,%.2f)=%.2f, p%s, pFDR%s; %.2f [%.2f,%.2f] ×10⁻³; %.2f [%.2f,%.2f] ×10⁻³",
	df1_AgeInt, df2_AgeInt, F_AgeInt, pprint_AgeInt, pFDRprint_AgeInt, emtr_Age_HC*1000, emtr_Age_HC_LCI*1000, emtr_Age_HC_UCI*1000, emtr_Age_OUD*1000, emtr_Age_OUD_LCI*1000, emtr_Age_OUD_UCI*1000
))))


#### Grp x Sex ----

mdl.SexInt <- list()
Tb.SexInt = data.frame(
	Var               = length(Var) %>% character(),
	F_SexInt          = length(Var) %>% numeric(),
	df1_SexInt        = length(Var) %>% numeric(),
	df2_SexInt        = length(Var) %>% numeric(),
	p_SexInt          = length(Var) %>% numeric(),
	pprint_SexInt     = length(Var) %>% character(),
	pFDR_SexInt       = length(Var) %>% numeric(),
	pFDRprint_SexInt  = length(Var) %>% character(),
	eta2_SexInt       = length(Var) %>% numeric(),
	emm_HC_M          = length(Var) %>% numeric(),
	emm_HC_M_LCI      = length(Var) %>% numeric(),
	emm_HC_M_HCI      = length(Var) %>% numeric(),
	emm_HC_F          = length(Var) %>% numeric(),
	emm_HC_F_LCI      = length(Var) %>% numeric(),
	emm_HC_F_HCI      = length(Var) %>% numeric(),
	emm_HC_MvF        = length(Var) %>% numeric(),
	emm_HC_MvF_LCI    = length(Var) %>% numeric(),
	emm_HC_MvF_HCI    = length(Var) %>% numeric(),
	emm_OUD_M         = length(Var) %>% numeric(),
	emm_OUD_M_LCI     = length(Var) %>% numeric(),
	emm_OUD_M_HCI     = length(Var) %>% numeric(),
	emm_OUD_F         = length(Var) %>% numeric(),
	emm_OUD_F_LCI     = length(Var) %>% numeric(),
	emm_OUD_F_HCI     = length(Var) %>% numeric(),
	emm_OUD_MvF       = length(Var) %>% numeric(),
	emm_OUD_MvF_LCI   = length(Var) %>% numeric(),
	emm_OUD_MvF_HCI   = length(Var) %>% numeric(),
	STRING = length(Var) %>% character()
)

for (i in 1:length(Var)) {
	
	mdl.SexInt[[Var[i]]] <- lmer(shiRFormula(Var[i],c("Grp*Sex",C,RFX)), data = dat)
	anv <- mdl.SexInt[[Var[i]]] %>% anova()
	emm <- mdl.SexInt[[Var[i]]] %>% emmeans::emmeans(c("Grp","Sex"))
	ctr <- emm %>% emmeans::contrast(list(HC=c(-1,0,1,0),OUD=c(0,-1,0,1))) %>% confint() %>% summary()
	emm <- emm %>% summary()
	eta2 <- effectsize::eta_squared(mdl.SexInt[[Var[i]]], partial=T)
	
	Tb.SexInt$Var              [i] <- Var[i]
	Tb.SexInt$F_SexInt         [i] <- anv["Grp:Sex","F value"]
	Tb.SexInt$df1_SexInt       [i] <- anv["Grp:Sex","NumDF"]
	Tb.SexInt$df2_SexInt       [i] <- anv["Grp:Sex","DenDF"]
	Tb.SexInt$p_SexInt         [i] <- anv["Grp:Sex","Pr(>F)"]
	Tb.SexInt$eta2_SexInt      [i] <- eta2 %>% dplyr::filter(Parameter=="Grp:Sex") %>% {.$Eta2_partial}
	Tb.SexInt$emm_HC_M         [i] <- emm %>% dplyr::filter(Grp=="HC" & Sex=="Male"  ) %>% {.$emmean}
	Tb.SexInt$emm_HC_M_LCI     [i] <- emm %>% dplyr::filter(Grp=="HC" & Sex=="Male"  ) %>% {.$lower.CL}
	Tb.SexInt$emm_HC_M_HCI     [i] <- emm %>% dplyr::filter(Grp=="HC" & Sex=="Male"  ) %>% {.$upper.CL}
	Tb.SexInt$emm_HC_F         [i] <- emm %>% dplyr::filter(Grp=="HC" & Sex=="Female") %>% {.$emmean}
	Tb.SexInt$emm_HC_F_LCI     [i] <- emm %>% dplyr::filter(Grp=="HC" & Sex=="Female") %>% {.$lower.CL}
	Tb.SexInt$emm_HC_F_HCI     [i] <- emm %>% dplyr::filter(Grp=="HC" & Sex=="Female") %>% {.$upper.CL}
	Tb.SexInt$emm_HC_MvF       [i] <- ctr %>% dplyr::filter(contrast=="HC") %>% {.$estimate}
	Tb.SexInt$emm_HC_MvF_LCI   [i] <- ctr %>% dplyr::filter(contrast=="HC") %>% {.$lower.CL}
	Tb.SexInt$emm_HC_MvF_HCI   [i] <- ctr %>% dplyr::filter(contrast=="HC") %>% {.$upper.CL}
	Tb.SexInt$emm_OUD_M        [i] <- emm %>% dplyr::filter(Grp=="OUD" & Sex=="Male"  ) %>% {.$emmean}
	Tb.SexInt$emm_OUD_M_LCI    [i] <- emm %>% dplyr::filter(Grp=="OUD" & Sex=="Male"  ) %>% {.$lower.CL}
	Tb.SexInt$emm_OUD_M_HCI    [i] <- emm %>% dplyr::filter(Grp=="OUD" & Sex=="Male"  ) %>% {.$upper.CL}
	Tb.SexInt$emm_OUD_F        [i] <- emm %>% dplyr::filter(Grp=="OUD" & Sex=="Female") %>% {.$emmean}
	Tb.SexInt$emm_OUD_F_LCI    [i] <- emm %>% dplyr::filter(Grp=="OUD" & Sex=="Female") %>% {.$lower.CL}
	Tb.SexInt$emm_OUD_F_HCI    [i] <- emm %>% dplyr::filter(Grp=="OUD" & Sex=="Female") %>% {.$upper.CL}
	Tb.SexInt$emm_OUD_MvF      [i] <- ctr %>% dplyr::filter(contrast=="OUD") %>% {.$estimate}
	Tb.SexInt$emm_OUD_MvF_LCI  [i] <- ctr %>% dplyr::filter(contrast=="OUD") %>% {.$lower.CL}
	Tb.SexInt$emm_OUD_MvF_HCI  [i] <- ctr %>% dplyr::filter(contrast=="OUD") %>% {.$upper.CL}
	
}
Tb.SexInt$pFDR_SexInt <- func_FdrBy(p = Tb.SexInt$p_SexInt, pname = Tb.SexInt$Var, pgrp = VarGrp)
Tb.SexInt$pprint_SexInt <- shiRFormatPval(Tb.SexInt$p_SexInt)
Tb.SexInt$pFDRprint_SexInt <- shiRFormatPval(Tb.SexInt$pFDR_SexInt)
Tb.SexInt %<>% dplyr::mutate(STRING=gsub(', pFDRNA','',gsub('-','–',sprintf(
	"F(%d,%.2f)=%.2f, p%s, pFDR%s; %.2f [%.2f,%.2f]; %.2f [%.2f,%.2f]; %.2f [%.2f,%.2f]; %.2f [%.2f,%.2f]; %.2f [%.2f,%.2f]; %.2f [%.2f,%.2f]",
	df1_SexInt, df2_SexInt, F_SexInt, pprint_SexInt, pFDRprint_SexInt, emm_HC_M, emm_HC_M_LCI, emm_HC_M_HCI, emm_HC_F, emm_HC_F_LCI, emm_HC_F_HCI, emm_HC_MvF, emm_HC_MvF_LCI, emm_HC_MvF_HCI, emm_OUD_M, emm_OUD_M_LCI, emm_OUD_M_HCI, emm_OUD_F, emm_OUD_F_LCI, emm_OUD_F_HCI, emm_OUD_MvF, emm_OUD_MvF_LCI, emm_OUD_MvF_HCI
))))


#### Grp ----

mdl.Main <- list()
Tb.Main = data.frame(
	Var             = length(Var) %>% character(),
	F_Grp           = length(Var) %>% numeric(),
	df1_Grp         = length(Var) %>% numeric(),
	df2_Grp         = length(Var) %>% numeric(),
	p_Grp           = length(Var) %>% numeric(),
	pprint_Grp      = length(Var) %>% character(),
	pFDR_Grp        = length(Var) %>% numeric(),
	pFDRprint_Grp   = length(Var) %>% character(),
	eta2_Grp        = length(Var) %>% numeric(),
	F_Age           = length(Var) %>% numeric(),
	df1_Age         = length(Var) %>% numeric(),
	df2_Age         = length(Var) %>% numeric(),
	p_Age           = length(Var) %>% numeric(),
	pprint_Age      = length(Var) %>% character(),
	pFDR_Age        = length(Var) %>% numeric(),
	pFDRprint_Age   = length(Var) %>% character(),
	eta2_Age        = length(Var) %>% numeric(),
	F_Sex           = length(Var) %>% numeric(),
	df1_Sex         = length(Var) %>% numeric(),
	df2_Sex         = length(Var) %>% numeric(),
	p_Sex           = length(Var) %>% numeric(),
	pprint_Sex      = length(Var) %>% character(),
	pFDR_Sex        = length(Var) %>% numeric(),
	pFDRprint_Sex   = length(Var) %>% character(),
	eta2_Sex        = length(Var) %>% numeric(),
	emm_HC          = length(Var) %>% numeric(),
	emm_HC_LCI      = length(Var) %>% numeric(),
	emm_HC_HCI      = length(Var) %>% numeric(),
	emm_OUD         = length(Var) %>% numeric(),
	emm_OUD_LCI     = length(Var) %>% numeric(),
	emm_OUD_HCI     = length(Var) %>% numeric(),
	slp_Age         = length(Var) %>% numeric(),
	slp_Age_LCI     = length(Var) %>% numeric(),
	slp_Age_UCI     = length(Var) %>% numeric(),
	emm_M           = length(Var) %>% numeric(),
	emm_M_LCI       = length(Var) %>% numeric(),
	emm_M_HCI       = length(Var) %>% numeric(),
	emm_F           = length(Var) %>% numeric(),
	emm_F_LCI       = length(Var) %>% numeric(),
	emm_F_HCI       = length(Var) %>% numeric(),
	STRING = length(Var) %>% character()
)

for (i in 1:length(Var)) {
	
	mdl.Main[[Var[i]]] <- lmer(shiRFormula(Var[i],c("Grp",C,RFX)), data = dat)
	anv <- mdl.Main[[Var[i]]] %>% anova()
	emm <- mdl.Main[[Var[i]]] %>% emmeans::emmeans(~Grp) %>% summary()
	slp <- mdl.Main[[Var[i]]] %>% {c(fixef(.)["Age"],confint(.,parm="Age"))}
	emm2 <- mdl.Main[[Var[i]]] %>% emmeans::emmeans(~Sex) %>% summary()
	eta2 <- effectsize::eta_squared(mdl.Main[[Var[i]]], partial=T)
	
	Tb.Main$Var           [i] <- Var[i]
	Tb.Main$F_Grp         [i] <- anv["Grp","F value"]
	Tb.Main$df1_Grp       [i] <- anv["Grp","NumDF"]
	Tb.Main$df2_Grp       [i] <- anv["Grp","DenDF"]
	Tb.Main$p_Grp         [i] <- anv["Grp","Pr(>F)"]
	Tb.Main$eta2_Grp      [i] <- eta2 %>% dplyr::filter(Parameter=="Grp") %>% {.$Eta2_partial}
	Tb.Main$F_Age         [i] <- anv["Age","F value"]
	Tb.Main$df1_Age       [i] <- anv["Age","NumDF"]
	Tb.Main$df2_Age       [i] <- anv["Age","DenDF"]
	Tb.Main$p_Age         [i] <- anv["Age","Pr(>F)"]
	Tb.Main$eta2_Age      [i] <- eta2 %>% dplyr::filter(Parameter=="Age") %>% {.$Eta2_partial}
	Tb.Main$F_Sex         [i] <- anv["Sex","F value"]
	Tb.Main$df1_Sex       [i] <- anv["Sex","NumDF"]
	Tb.Main$df2_Sex       [i] <- anv["Sex","DenDF"]
	Tb.Main$p_Sex         [i] <- anv["Sex","Pr(>F)"]
	Tb.Main$eta2_Sex      [i] <- eta2 %>% dplyr::filter(Parameter=="Sex") %>% {.$Eta2_partial}
	Tb.Main$emm_HC        [i] <- emm %>% dplyr::filter(Grp=="HC") %>% {.$emmean}
	Tb.Main$emm_HC_LCI    [i] <- emm %>% dplyr::filter(Grp=="HC") %>% {.$lower.CL}
	Tb.Main$emm_HC_HCI    [i] <- emm %>% dplyr::filter(Grp=="HC") %>% {.$upper.CL}
	Tb.Main$emm_OUD       [i] <- emm %>% dplyr::filter(Grp=="OUD") %>% {.$emmean}
	Tb.Main$emm_OUD_LCI   [i] <- emm %>% dplyr::filter(Grp=="OUD") %>% {.$lower.CL}
	Tb.Main$emm_OUD_HCI   [i] <- emm %>% dplyr::filter(Grp=="OUD") %>% {.$upper.CL}
	Tb.Main$slp_Age       [i] <- slp[1]
	Tb.Main$slp_Age_LCI   [i] <- slp[2]
	Tb.Main$slp_Age_UCI   [i] <- slp[3]
	Tb.Main$emm_M         [i] <- emm2 %>% dplyr::filter(Sex=="Male") %>% {.$emmean}
	Tb.Main$emm_M_LCI     [i] <- emm2 %>% dplyr::filter(Sex=="Male") %>% {.$lower.CL}
	Tb.Main$emm_M_HCI     [i] <- emm2 %>% dplyr::filter(Sex=="Male") %>% {.$upper.CL}
	Tb.Main$emm_F         [i] <- emm2 %>% dplyr::filter(Sex=="Female") %>% {.$emmean}
	Tb.Main$emm_F_LCI     [i] <- emm2 %>% dplyr::filter(Sex=="Female") %>% {.$lower.CL}
	Tb.Main$emm_F_HCI     [i] <- emm2 %>% dplyr::filter(Sex=="Female") %>% {.$upper.CL}
	
}
Tb.Main$pFDR_Grp <- func_FdrBy(p = Tb.Main$p_Grp, pname = Tb.Main$Var, pgrp = VarGrp)
Tb.Main$pprint_Grp <- shiRFormatPval(Tb.Main$p_Grp)
Tb.Main$pFDRprint_Grp <- shiRFormatPval(Tb.Main$pFDR_Grp)
Tb.Main$pFDR_Age <- func_FdrBy(p = Tb.Main$p_Age, pname = Tb.Main$Var, pgrp = VarGrp)
Tb.Main$pprint_Age <- shiRFormatPval(Tb.Main$p_Age)
Tb.Main$pFDRprint_Age <- shiRFormatPval(Tb.Main$pFDR_Age)
Tb.Main$pFDR_Sex <- func_FdrBy(p = Tb.Main$p_Sex, pname = Tb.Main$Var, pgrp = VarGrp)
Tb.Main$pprint_Sex <- shiRFormatPval(Tb.Main$p_Sex)
Tb.Main$pFDRprint_Sex <- shiRFormatPval(Tb.Main$pFDR_Sex)
Tb.Main %<>% dplyr::mutate(STRING=gsub(', pFDRNA','',gsub('-','–',sprintf(
	"F(%d,%.2f)=%.2f, p%s, pFDR%s; %.2f [%.2f,%.2f]; %.2f [%.2f,%.2f]; F(%d,%.2f)=%.2f, p%s, pFDR%s; %.2f [%.2f,%.2f] ×10⁻³; F(%d,%.2f)=%.2f, p%s, pFDR%s; %.2f [%.2f,%.2f]; %.2f [%.2f,%.2f]",
	df1_Grp, df2_Grp, F_Grp, pprint_Grp, pFDRprint_Grp, emm_HC, emm_HC_LCI, emm_HC_HCI, emm_OUD, emm_OUD_LCI, emm_OUD_HCI, df1_Age, df2_Age, F_Age, pprint_Age, pFDRprint_Age, slp_Age*1000, slp_Age_LCI*1000, slp_Age_UCI*1000, df1_Sex, df2_Sex, F_Sex, pprint_Sex, pFDRprint_Sex, emm_M, emm_M_LCI, emm_M_HCI, emm_F, emm_F_LCI, emm_F_HCI
))))


#### OD ----

mdl.HxOD <- list()
Tb.HxOD = data.frame(
	Var             = length(Var) %>% character(),
	F_HxOD          = length(Var) %>% numeric(),
	df1_HxOD        = length(Var) %>% numeric(),
	df2_HxOD        = length(Var) %>% numeric(),
	p_HxOD          = length(Var) %>% numeric(),
	pprint_HxOD     = length(Var) %>% character(),
	pFDR_HxOD       = length(Var) %>% numeric(),
	pFDRprint_HxOD  = length(Var) %>% character(),
	eta2_HxOD       = length(Var) %>% numeric(),
	emm_nOD         = length(Var) %>% numeric(),
	emm_nOD_LCI     = length(Var) %>% numeric(),
	emm_nOD_HCI     = length(Var) %>% numeric(),
	emm_OD          = length(Var) %>% numeric(),
	emm_OD_LCI      = length(Var) %>% numeric(),
	emm_OD_HCI      = length(Var) %>% numeric(),
	STRING = length(Var) %>% character()
)

dat_od <- dat %>% dplyr::filter(HxOD==0 | HxOD==1)
dat_od$HxOD %<>% droplevels()

for (i in 1:length(Var)) {
	
	mdl.HxOD[[Var[i]]] <- lmer(shiRFormula(Var[i],c("HxOD",C,RFX)), data = dat_od)
	anv <- mdl.HxOD[[Var[i]]] %>% anova()
	emm <- mdl.HxOD[[Var[i]]] %>% emmeans::emmeans(~HxOD) %>% summary()
	eta2 <- effectsize::eta_squared(mdl.HxOD[[Var[i]]], partial=T)
	
	Tb.HxOD$Var          [i] <- Var[i]
	Tb.HxOD$F_HxOD       [i] <- anv["HxOD","F value"]
	Tb.HxOD$df1_HxOD     [i] <- anv["HxOD","NumDF"]
	Tb.HxOD$df2_HxOD     [i] <- anv["HxOD","DenDF"]
	Tb.HxOD$p_HxOD       [i] <- anv["HxOD","Pr(>F)"]
	Tb.HxOD$eta2_HxOD    [i] <- eta2 %>% dplyr::filter(Parameter=="HxOD") %>% {.$Eta2_partial}
	Tb.HxOD$emm_nOD      [i] <- emm %>% dplyr::filter(HxOD==0) %>% {.$emmean}
	Tb.HxOD$emm_nOD_LCI  [i] <- emm %>% dplyr::filter(HxOD==0) %>% {.$lower.CL}
	Tb.HxOD$emm_nOD_HCI  [i] <- emm %>% dplyr::filter(HxOD==0) %>% {.$upper.CL}
	Tb.HxOD$emm_OD       [i] <- emm %>% dplyr::filter(HxOD==1) %>% {.$emmean}
	Tb.HxOD$emm_OD_LCI   [i] <- emm %>% dplyr::filter(HxOD==1) %>% {.$lower.CL}
	Tb.HxOD$emm_OD_HCI   [i] <- emm %>% dplyr::filter(HxOD==1) %>% {.$upper.CL}
	
}
Tb.HxOD$pFDR_HxOD <- func_FdrBy(p = Tb.HxOD$p_HxOD, pname = Tb.HxOD$Var, pgrp = VarGrp)
Tb.HxOD$pprint_HxOD <- shiRFormatPval(Tb.HxOD$p_HxOD)
Tb.HxOD$pFDRprint_HxOD <- shiRFormatPval(Tb.HxOD$pFDR_HxOD)
Tb.HxOD %<>% dplyr::mutate(STRING=gsub(', pFDRNA','',gsub('-','–',sprintf(
	"F(%d,%.2f)=%.2f, p%s, pFDR%s; %.2f [%.2f,%.2f]; %.2f [%.2f,%.2f]",
	df1_HxOD, df2_HxOD, F_HxOD, pprint_HxOD, pFDRprint_HxOD, emm_nOD, emm_nOD_LCI, emm_nOD_HCI, emm_OD, emm_OD_LCI, emm_OD_HCI
))))


#### Med ----

mdl.Med <- list()
Tb.Med = data.frame(
	Var             = length(Var) %>% character(),
	F_Med           = length(Var) %>% numeric(),
	df1_Med         = length(Var) %>% numeric(),
	df2_Med         = length(Var) %>% numeric(),
	p_Med           = length(Var) %>% numeric(),
	pprint_Med      = length(Var) %>% character(),
	pFDR_Med        = length(Var) %>% numeric(),
	pFDRprint_Med   = length(Var) %>% character(),
	eta2_Med        = length(Var) %>% numeric(),
	emm_BUP         = length(Var) %>% numeric(),
	emm_BUP_LCI     = length(Var) %>% numeric(),
	emm_BUP_HCI     = length(Var) %>% numeric(),
	emm_MTD         = length(Var) %>% numeric(),
	emm_MTD_LCI     = length(Var) %>% numeric(),
	emm_MTD_HCI     = length(Var) %>% numeric(),
	STRING = length(Var) %>% character()
)

dat_med <- dat %>% dplyr::filter(Medication=="Methadone" | Medication=="Buprenorphine")
dat_med$Medication %<>% droplevels()

for (i in 1:length(Var)) {
	
	mdl.Med[[Var[i]]] <- lmer(shiRFormula(Var[i],c("Medication",C,RFX)), data = dat_med)
	anv <- mdl.Med[[Var[i]]] %>% anova()
	emm <- mdl.Med[[Var[i]]] %>% emmeans::emmeans(~Medication) %>% summary()
	eta2 <- effectsize::eta_squared(mdl.Med[[Var[i]]], partial=T)
	
	Tb.Med$Var          [i] <- Var[i]
	Tb.Med$F_Med        [i] <- anv["Medication","F value"]
	Tb.Med$df1_Med      [i] <- anv["Medication","NumDF"]
	Tb.Med$df2_Med      [i] <- anv["Medication","DenDF"]
	Tb.Med$p_Med        [i] <- anv["Medication","Pr(>F)"]
	Tb.Med$eta2_Med     [i] <- eta2 %>% dplyr::filter(Parameter=="Medication") %>% {.$Eta2_partial}
	Tb.Med$emm_BUP      [i] <- emm %>% dplyr::filter(Medication=="Buprenorphine") %>% {.$emmean}
	Tb.Med$emm_BUP_LCI  [i] <- emm %>% dplyr::filter(Medication=="Buprenorphine") %>% {.$lower.CL}
	Tb.Med$emm_BUP_HCI  [i] <- emm %>% dplyr::filter(Medication=="Buprenorphine") %>% {.$upper.CL}
	Tb.Med$emm_MTD      [i] <- emm %>% dplyr::filter(Medication=="Methadone") %>% {.$emmean}
	Tb.Med$emm_MTD_LCI  [i] <- emm %>% dplyr::filter(Medication=="Methadone") %>% {.$lower.CL}
	Tb.Med$emm_MTD_HCI  [i] <- emm %>% dplyr::filter(Medication=="Methadone") %>% {.$upper.CL}
	
}
Tb.Med$pFDR_Med <- func_FdrBy(p = Tb.Med$p_Med, pname = Tb.Med$Var, pgrp = VarGrp)
Tb.Med$pprint_Med <- shiRFormatPval(Tb.Med$p_Med)
Tb.Med$pFDRprint_Med <- shiRFormatPval(Tb.Med$pFDR_Med)
Tb.Med %<>% dplyr::mutate(STRING=gsub(', pFDRNA','',gsub('-','–',sprintf(
	"F(%d,%.2f)=%.2f, p%s, pFDR%s; %.2f [%.2f,%.2f]; %.2f [%.2f,%.2f]",
	df1_Med, df2_Med, F_Med, pprint_Med, pFDRprint_Med, emm_BUP, emm_BUP_LCI, emm_BUP_HCI, emm_MTD, emm_MTD_LCI, emm_MTD_HCI
))))


#### Yrs ----

mdl.Yrs <- list()
Tb.Yrs = data.frame(
	Var                = length(Var) %>% character(),
	F_YrsOpUse         = length(Var) %>% numeric(),
	df1_YrsOpUse       = length(Var) %>% numeric(),
	df2_YrsOpUse       = length(Var) %>% numeric(),
	p_YrsOpUse         = length(Var) %>% numeric(),
	pprint_YrsOpUse    = length(Var) %>% character(),
	pFDR_YrsOpUse      = length(Var) %>% numeric(),
	pFDRprint_YrsOpUse = length(Var) %>% character(),
	eta2_YrsOpUse      = length(Var) %>% numeric(),
	slp_YrsOpUse       = length(Var) %>% numeric(),
	slp_YrsOpUse_LCI   = length(Var) %>% numeric(),
	slp_YrsOpUse_UCI   = length(Var) %>% numeric(),
	STRING = length(Var) %>% character()
)

dat_yrs <- dat %>% dplyr::filter(!is.na(YrsOpUse) & Grp=="OUD")

for (i in 1:length(Var)) {
	
	mdl.Yrs[[Var[i]]] <- lmer(shiRFormula(Var[i],c("YrsOpUse",C,RFX)), data = dat_yrs)
	anv <- mdl.Yrs[[Var[i]]] %>% anova()
	slp <- mdl.Yrs[[Var[i]]] %>% {c(fixef(.)["YrsOpUse"],confint(.,parm="YrsOpUse"))}
	eta2 <- effectsize::eta_squared(mdl.Yrs[[Var[i]]], partial=T)
	
	Tb.Yrs$Var                [i] <- Var[i]
	Tb.Yrs$F_YrsOpUse         [i] <- anv["YrsOpUse","F value"]
	Tb.Yrs$df1_YrsOpUse       [i] <- anv["YrsOpUse","NumDF"]
	Tb.Yrs$df2_YrsOpUse       [i] <- anv["YrsOpUse","DenDF"]
	Tb.Yrs$p_YrsOpUse         [i] <- anv["YrsOpUse","Pr(>F)"]
	Tb.Yrs$eta2_YrsOpUse      [i] <- eta2 %>% dplyr::filter(Parameter=="YrsOpUse") %>% {.$Eta2_partial}
	Tb.Yrs$slp_YrsOpUse       [i] <- slp[1]
	Tb.Yrs$slp_YrsOpUse_LCI   [i] <- slp[2]
	Tb.Yrs$slp_YrsOpUse_UCI   [i] <- slp[3]
	
}
Tb.Yrs$pFDR_YrsOpUse <- func_FdrBy(p = Tb.Yrs$p_YrsOpUse, pname = Tb.Yrs$Var, pgrp = VarGrp)
Tb.Yrs$pprint_YrsOpUse <- shiRFormatPval(Tb.Yrs$p_YrsOpUse)
Tb.Yrs$pFDRprint_YrsOpUse <- shiRFormatPval(Tb.Yrs$pFDR_YrsOpUse)
Tb.Yrs %<>% dplyr::mutate(STRING=gsub(', pFDRNA','',gsub('-','–',sprintf(
	"F(%d,%.2f)=%.2f, p%s, pFDR%s; %.2f [%.2f,%.2f] ×10⁻³",
	df1_YrsOpUse, df2_YrsOpUse, F_YrsOpUse, pprint_YrsOpUse, pFDRprint_YrsOpUse, slp_YrsOpUse*1000, slp_YrsOpUse_LCI*1000, slp_YrsOpUse_UCI*1000
))))


#### print ----

obj_names <- c(
	"Tb.Descrip",
	"Tb.AgeInt",
	"Tb.SexInt",
	"Tb.Main",
	"Tb.Med",
	"Tb.HxOD",
	"Tb.Yrs"
)

openxlsx::write.xlsx(mget(obj_names), "R.Out.xlsx")