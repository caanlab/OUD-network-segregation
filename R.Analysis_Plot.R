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
library(tibble)
library(ragg)


#### prep ----

setwd('/Users/zhshi/Documents/2026/OUD rest/R')
source('R.Analysis_Prep.R')

datBehav <- read.csv("R.Source_Behav.csv", na.strings = c("NA","NaN","nan","#N/A","")) %>% 
	dplyr::mutate(across(c(Subj, Study, Grp, HxOD, Sex, Race, OpiUD, AlcUD, StmUD, CanUD, IsSmoker, Medication), as.factor))
datSegNet <- read.csv("R.Data_SegNet.csv",na.strings = c("NA","NaN","nan","#N/A","")) %>%
	dplyr::mutate(across(c(Subj), as.factor))

stopifnot(
	identical(datBehav$Subj, datSegNet$Subj)
)

dat <- datBehav %>% 
	dplyr::left_join(datSegNet, by="Subj")

rm(datBehav, datSegNet)

dat %<>% dplyr::filter(!(FD_mean > 0.5))


#### var def ----

C <- c("Age","Sex","Race","FD_mean","StmUD","IsSmoker","RestTr","RestDur")
RFX <- "(1|Study)"


#### prep ----

xxPlotDat <- tibble::tribble(
	~No , ~Var              ,  ~NetName            , ~Grp_pVal   , ~Grp_pTxt , ~Yrs_xy  , ~Yrs_pVal   , ~Yrs_slp     , ~Yrs_pTxt    , ~Age_xy   , ~AgeOUD_pVal , ~AgeOUD_slp  , ~AgeOUD_pTxt , ~AgeHC_pVal , ~AgeHC_slp   , ~AgeHC_pTxt  , ~Age_pVal   , ~Age_slp     , ~Age_pTxt   , ~Grp_yTick   , ~Yrs_yTick   , ~Age_yTick ,
	1   , "SegOverall_As"   ,  "Association"       , 0.000548581 , "∗∗∗"     , "L1R"    , 0.046443291 , -2.206191830 , "×10⁻³ ∗"    , "L1RL0R"  , 0.002139983  , -2.302676710 , "×10⁻³ ∗∗"   , 0.004441041 , -1.973342907 , "×10⁻³ ∗∗"   , 1.49976E-05 , -2.138454872 , "×10⁻³ ∗∗∗" , seq(-9,9,.1) , seq(-9,9,.1) , seq(-9,9,.1)  ,
	2   , "SegOverall_Sm"   ,  "Sensorimotor"      , 0.000362234 , "∗∗∗"     , "L3R"    , 0.265372739 ,  1.580902576 , "×10⁻³ ns"   , "U0RL2Ln" , 0.002793338  , -2.653721026 , "×10⁻³ ∗∗"   , 0.095784140 , -1.390321151 , "×10⁻³ ns"   , 0.001908674 , -1.896186272 , "×10⁻³ ∗∗"  , seq(-9,9,.1) , seq(-9,9,.1) , seq(-9,9,.1)  ,
	3   , "SegIndivid_AsCo" ,  "Cingulo-opercular" , 0.002551990 , "∗∗"      , "L1R"    , 0.914949403 , -0.210927039 , "×10⁻³ ns"   , NA        , NA           , NA           , NA           , NA          , NA           , NA           , NA          , NA           , NA          , seq(-9,9,.1) , seq(-9,9,.1) , seq(-9,9,.1)  ,
	4   , "SegIndivid_AsDa" ,  "Dorsal attention"  , 0.917175921 , "ns"      , "L2R"    , 0.034851465 , -3.927896344 , "×10⁻³ ∗"    , NA        , NA           , NA           , NA           , NA          , NA           , NA           , NA          , NA           , NA          , seq(-9,9,.1) , seq(-9,9,.1) , seq(-9,9,.1)  ,
	5   , "SegIndivid_AsDm" ,  "Default mode"      , 0.012895401 , "∗"       , "L1L"    , 0.029252133 , -3.694783726 , "×10⁻³ ∗"    , NA        , NA           , NA           , NA           , NA          , NA           , NA           , NA          , NA           , NA          , seq(-9,9,.1) , seq(-9,9,.1) , seq(-9,9,.1)  ,
	6   , "SegIndivid_AsFp" ,  "Fronto-parietal"   , 0.126823334 , "ns"      , "L1R"    , 0.068548564 , -2.827642678 , "×10⁻³ ns"   , NA        , NA           , NA           , NA           , NA          , NA           , NA           , NA          , NA           , NA          , seq(-9,9,.1) , seq(-9,9,.1) , seq(-9,9,.1)  ,
	7   , "SegIndivid_AsSn" ,  "Salience"          , 0.084355541 , "ns"      , "L1R"    , 0.674766478 ,  0.777829318 , "×10⁻³ ns"   , NA        , NA           , NA           , NA           , NA          , NA           , NA           , NA          , NA           , NA          , seq(-9,9,.1) , seq(-9,9,.1) , seq(-9,9,.1)  ,
	8   , "SegIndivid_AsVa" ,  "Ventral attention" , 0.000504080 , "∗∗∗"     , "L1R"    , 0.198780399 , -3.046288722 , "×10⁻³ ns"   , NA        , NA           , NA           , NA           , NA          , NA           , NA           , NA          , NA           , NA          , seq(-9,9,.2) , seq(-9,9,.1) , seq(-9,9,.1)  ,
	9   , "SegIndivid_SmAu" ,  "Auditory"          , 0.101762068 , "ns"      , "L1R"    , 0.169114649 ,  2.814595163 , "×10⁻³ ns"   , NA        , NA           , NA           , NA           , NA          , NA           , NA           , NA          , NA           , NA          , seq(-9,9,.1) , seq(-9,9,.1) , seq(-9,9,.1)  ,
	10  , "SegIndivid_SmHa" ,  "Hand"              , 0.001192214 , "∗∗"      , "L0R"    , 0.414397584 ,  1.529206578 , "×10⁻³ ns"   , NA        , NA           , NA           , NA           , NA          , NA           , NA           , NA          , NA           , NA          , seq(-9,9,.1) , seq(-9,9,.1) , seq(-9,9,.1)  ,
	11  , "SegIndivid_SmMo" ,  "Mouth"             , 0.117461623 , "ns"      , "L1R"    , 0.377754790 ,  2.877385434 , "×10⁻³ ns"   , NA        , NA           , NA           , NA           , NA          , NA           , NA           , NA          , NA           , NA          , seq(-9,9,.2) , seq(-9,9,.2) , seq(-9,9,.1)  ,
	12  , "SegIndivid_SmVi" ,  "Visual"            , 0.015579173 , "∗"       , "L2R"    , 0.942830885 ,  0.135404609 , "×10⁻³ ns"   , NA        , NA           , NA           , NA           , NA          , NA           , NA           , NA          , NA           , NA          , seq(-9,9,.2) , seq(-9,9,.1) , seq(-9,9,.1)  ,
	13  , "SegIndivid_OtCb" ,  "Cerebellum"        , 0.137861779 , "ns"      , "U0R"    , 0.995960608 ,  0.023038586 , "×10⁻³ ns"   , NA        , NA           , NA           , NA           , NA          , NA           , NA           , NA          , NA           , NA          , list(NA)     , list(NA)     , list(NA)   ,
	14  , "SegIndivid_OtMr" ,  "Memory"            , 0.479250025 , "ns"      , "U0R"    , 0.731610556 ,  1.071121411 , "×10⁻³ ns"   , NA        , NA           , NA           , NA           , NA          , NA           , NA           , NA          , NA           , NA          , list(NA)     , list(NA)     , list(NA)   ,
	15  , "SegIndivid_OtSu" ,  "Subcortical"       , 0.001784585 , "∗∗"      , "U0R"    , 0.475308882 ,  1.574211183 , "×10⁻³ ns"   , NA        , NA           , NA           , NA           , NA          , NA           , NA           , NA          , NA           , NA          , list(NA)     , list(NA)     , list(NA)  
)


oudrest_plot <- function(i, dat, xxPlotDat, C, szInd) {
	
	xxColor_OUD = "#C34C28"
	xxColor_HC = "#2273FF"
	# c(" [1]","#F8766D","#F37C58","#ED813E","#E68613","#DE8C00","#D69100","#CD9600","#C29A00")
	# c(" [9]","#B79F00","#ABA300","#9DA700","#8EAB00","#7CAE00","#66B200","#49B500","#0BB702")
	# c("[17]","#00BA38","#00BC52","#00BE67","#00BF7A","#00C08B","#00C19A","#00C1A9","#00C0B7")
	# c("[25]","#00BFC4","#00BDD1","#00BBDC","#00B8E7","#00B4F0","#00AFF8","#00A9FF","#22A3FF")
	# c("[33]","#619CFF","#8494FF","#9F8CFF","#B584FF","#C77CFF","#D674FD","#E36EF6","#ED68ED")
	# c("[41]","#F564E3","#FB61D8","#FF61CC","#FF62BF","#FF64B0","#FF68A1","#FF6C91","#FC7180")
	
	
	xxFontSize_PlotTitle = c(90, 50)/2.8
	xxFontSize_AxisTitle = c(90, 50)/2.8
	xxFontSize_AxisTxt = c(90, 50)/2.8
	xxFontSize_LegTxt = c(90, 50)/2.8
	xxFontSize_SigTxt = c(80, 44)/3
	
	xxNumFormat_Y = c("%.1f", "%.1f")
	
	xxColumn_Alpha = c(.4, .4)
	xxColumn_Width = c(.8, .8)
	xxColumn_LineWidth = c(1.5, .75)/1.2
	
	xxColPoint_PointSize = c(2.5, 1.2)/.9
	xxColPoint_Alpha = c(1, 1)
	xxColPoint_Jitter = c(.15, .15)
	
	xxColError_Width = c(.25, .25)/1.5
	xxColError_LineWidth = c(1.5, .75)/1
	
	xxScatter_PointSize = c(2.5, 1.2)/.9
	xxScatter_PointAlpha = c(.7, .7)
	
	xxTrend_LineSize = c(1.5, .75)/1.2
	xxTrend_RibbonAlpha = c(.3, .3)
	
	xxSig_LineWidth = c(1.5, .75)/1.2
	
	xVar <- xxPlotDat$Var[i]
	xTitle <- xxPlotDat$NetName[i]
	
	#### grp ----
	
	xdat <- dat %>% dplyr::select(all_of(c(xVar, "Grp", C, "Study"))) %>% tidyr::drop_na()
	
	xMdl <- lmer(shiRFormula(xVar, c("Grp", C, RFX)), data = xdat)
	xEmm <- xMdl %>% emmeans::emmeans(~Grp) %>% summary()
	
	xdat %<>% dplyr::mutate(y = residuals(xMdl))
	xdat$y[xdat$Grp=="HC"] %<>% {. + xEmm$emmean[xEmm$Grp=="HC"]}
	xdat$y[xdat$Grp=="OUD"] %<>% {. + xEmm$emmean[xEmm$Grp=="OUD"]}
	
	xdat_summ <- data.frame(
		Grp = xEmm$Grp,
		y = xEmm$emmean,
		se = xEmm$SE
	)
	
	levels(xdat$Grp)[levels(xdat$Grp) == "HC"] <- "Non-OUD"
	levels(xdat_summ$Grp)[levels(xdat_summ$Grp) == "HC"] <- "Non-OUD"
	
	xLevel <- c("OUD","Non-OUD")
	
	xxGrp <- ggplot() +
		geom_col(
			data = xdat_summ,
			aes(x = Grp, y = y, fill = Grp),
			width = xxColumn_Width[szInd],
			linewidth = xxColumn_LineWidth[szInd],
			alpha = xxColumn_Alpha[szInd],
			color = "black",
			position = position_dodge(width = .8)
		) +
		geom_point(
			data = xdat,
			aes(x = Grp, y = y, color = Grp),
			size = xxColPoint_PointSize[szInd],
			alpha = xxColPoint_Alpha[szInd],
			stroke = 0,
			position = position_jitter(width = xxColPoint_Jitter[szInd])
		) +
		geom_errorbar(
			data = xdat_summ,
			aes(x = Grp, ymin = y-se, ymax = y+se),
			width = xxColError_Width[szInd],
			linewidth = xxColError_LineWidth[szInd],
			position = position_dodge(width = .8)
		) +
		scale_color_manual(values = c(xxColor_OUD,xxColor_HC), breaks = xLevel) +
		scale_fill_manual(values = c(xxColor_OUD,xxColor_HC), breaks = xLevel) +
		scale_x_discrete(limits = xLevel) +
		theme_grey() +
		ggtitle(xTitle) +
		labs(
			y = "Network segregation"
		) +
		guides(color = F, fill = F) +
		theme(
			panel.grid.minor = element_blank(),
			axis.title.y = element_text(size = xxFontSize_AxisTitle[szInd], color = "black"),
			axis.title.x = element_blank(),
			axis.text.y = element_text(size = xxFontSize_AxisTxt[szInd], color = "black"),
			axis.text.x = element_text(size = xxFontSize_AxisTxt[szInd], color = "black"),
			plot.title = element_text(size = xxFontSize_PlotTitle[szInd], color = "black", face = "bold", hjust = .5),
			legend.position = "bottom",
			legend.box = "horizontal",
			legend.direction = "horizontal",
			legend.title = element_blank(),
			legend.text = element_text(size = xxFontSize_LegTxt[szInd], color = "black"),
			legend.key = element_rect(fill = "white"),
		)
	xxYmin <- min(c(xdat$y,0))
	xxYmax <- max(xdat$y)
	xxYgap <-  xxYmax - xxYmin
	xxGrp <- xxGrp +
		coord_cartesian(ylim = c(xxYmin, xxYmax)) +
		scale_y_continuous(
			labels = function(x) sprintf(xxNumFormat_Y[szInd], x),
			expand = expansion(mult = .02),
			breaks = if (is.na(xxPlotDat$Grp_yTick[i])) waiver() else xxPlotDat$Grp_yTick[[i]]
		)
	
	if (!is.na(xxPlotDat$Grp_pVal[i])) {
		xSegX <- c(1, 2)
		xSegY <- c(xxYmax + xxYgap*.045, xxYmax + xxYgap*.065)
		xTxtX <- mean(xSegX)
		xTxtY <- xxYmax + xxYgap*.065
		xTxtLab <- xxPlotDat$Grp_pTxt[i]
		xIsAsterisk <- all(unique(strsplit(xTxtLab,"")[[1]])=="*")
		xxGrp <- xxGrp +
			scale_y_continuous(
				labels = function(x) sprintf(xxNumFormat_Y[szInd], x), 
				expand = expansion(mult = c(.02, .13)),
				breaks = if (is.na(xxPlotDat$Grp_yTick[i])) waiver() else xxPlotDat$Grp_yTick[[i]]
			) +
			annotate("segment", x = xSegX[1], xend = xSegX[2], y = xSegY[2], yend = xSegY[2], linewidth = xxSig_LineWidth[szInd]) +
			annotate("segment", x = xSegX[1], xend = xSegX[1], y = xSegY[1], yend = xSegY[2], linewidth = xxSig_LineWidth[szInd]) +
			annotate("segment", x = xSegX[2], xend = xSegX[2], y = xSegY[1], yend = xSegY[2], linewidth = xxSig_LineWidth[szInd]) +
			annotate("text",
							 x = xTxtX, y = xTxtY, label = xTxtLab, parse = F, 
							 size = ifelse(xIsAsterisk, xxFontSize_SigTxt[szInd]/.pt * 1.5, xxFontSize_SigTxt[szInd]/.pt),
							 vjust = ifelse(xIsAsterisk, .2, -.3),
							 family = "STIX Two Math"
			)
	}
	
	#### yrs ----
	
	xdat <- dat %>% dplyr::select(all_of(c(xVar, "Grp", C, "YrsOpUse", "Study")))
	xdat %<>% dplyr::filter(Grp == "OUD") %>% tidyr::drop_na()
	
	xMdl <- lmer(shiRFormula(xVar, c("YrsOpUse", C, RFX)), data = xdat)
	xMdl@call$data <- xdat
	xFx <- effects::Effect("YrsOpUse", mod = xMdl, xlevels = list(YrsOpUse = seq(from = min(xdat$YrsOpUse), to = max(xdat$YrsOpUse), by = .005))) %>% as.data.frame() %>% dplyr::mutate(Grp = "OUD") # get ribbon and stuff
	xFx0 <- effects::Effect("YrsOpUse", mod = xMdl, xlevels = list(YrsOpUse = unique(xdat$YrsOpUse))) %>% as.data.frame()
	xdat[[xVar]] <- residuals(xMdl)
	for (k in 1:nrow(xdat)) {
		xdat[[xVar]][k] <- xdat[[xVar]][k] + xFx0 %>% dplyr::filter(YrsOpUse==xdat$YrsOpUse[k]) %>% {.$fit}
	}
	
	xdat2 <- data.frame(
		YrsOpUse = range(xdat$YrsOpUse),
		fit = rep(xEmm %>% dplyr::filter(Grp=="HC") %>% {.$emmean}, 2),
		se = rep(0, 2),
		lower = rep(0, 2),
		upper = rep(0, 2),
		Grp = rep("Non-OUD reference", 2)
	)
	xFx <- rbind(xFx, xdat2)
	
	xxYrs <- ggplot() +
		geom_point(
			data = xdat,
			aes(x = YrsOpUse, y = !!sym(xVar), color = Grp),
			size = xxScatter_PointSize[szInd],
			alpha = xxScatter_PointAlpha[szInd],
			stroke = 0
		) +
		geom_line(
			data = xFx,
			aes(x = YrsOpUse, y = fit, color = Grp),
			size = xxTrend_LineSize[szInd]
		) +
		geom_ribbon(
			data = xFx,
			aes(x = YrsOpUse, ymin = lower, ymax = upper, fill = Grp, alpha = Grp),
		) +
		scale_color_manual(values = c(xxColor_OUD,xxColor_HC), breaks = c("OUD","Non-OUD reference")) +
		scale_fill_manual(values = c(xxColor_OUD,xxColor_HC), breaks = c("OUD","Non-OUD reference")) +
		scale_alpha_manual(values = c(xxTrend_RibbonAlpha[szInd],0), breaks = c("OUD","Non-OUD reference")) +
		theme_grey() +
		labs(
			x = "Years of opioid use",
			y = "Network segregation"
		) +
		ggtitle(xTitle) +
		theme(
			panel.grid.minor = element_blank(),
			axis.title.y = element_text(size = xxFontSize_AxisTitle[szInd], color = "black"),
			axis.title.x = element_text(size = xxFontSize_AxisTitle[szInd], color = "black"),
			axis.text.y = element_text(size = xxFontSize_AxisTxt[szInd], color = "black"),
			axis.text.x = element_text(size = xxFontSize_AxisTxt[szInd], color = "black"),
			plot.title = element_text(size = xxFontSize_PlotTitle[szInd], color = "black", face = "bold", hjust = .5),
			legend.position = "bottom",
			legend.box = "horizontal",
			legend.direction = "horizontal",
			legend.title = element_blank(),
			legend.text = element_text(size = xxFontSize_LegTxt[szInd], color = "black"),
			legend.key = element_rect(fill = "white"),
		)
	xxYmin <- min(xdat[[xVar]])
	xxYmax <- max(xdat[[xVar]])
	xxYgap <- xxYmax - xxYmin
	xxXmin <- min(xdat$YrsOpUse)
	xxXmax <- max(xdat$YrsOpUse)
	xxXgap <- xxXmax - xxXmin
	xxYrs <- xxYrs +
		coord_cartesian(ylim = c(xxYmin, xxYmax), xlim = c(xxXmin, xxXmax)) +
		scale_y_continuous(
			labels = function(x) sprintf(xxNumFormat_Y[szInd], x),
			expand = expansion(mult = .02),
			breaks = if (is.na(xxPlotDat$Yrs_yTick[i])) waiver() else xxPlotDat$Yrs_yTick[[i]]
		) +
		scale_x_continuous(
			expand = expansion(mult = .02)
		)
	
	if (!is.na(xxPlotDat$Yrs_xy[i])) {
		xTxtX <- xxPlotDat$Yrs_xy[i] %>% substr(3,3)
		xTxtY <- xxPlotDat$Yrs_xy[i] %>% substr(1,1)
		xTxtYdodge <- xxPlotDat$Yrs_xy[i] %>% substr(2,2) %>% as.numeric()
		xTxtYgap <- .05 * xxYgap
		xTxtLab <- paste(sprintf("β=%.2f%s", xxPlotDat$Yrs_slp[i], xxPlotDat$Yrs_pTxt[i])) %>% {gsub("-", "–", .)}
		xxYrs <- xxYrs +
			annotate("text", 
							 x = ifelse(xTxtX == "R", xxXmax, xxXmin),
							 y = ifelse(xTxtY == "U", xxYmax - xTxtYdodge * xTxtYgap, xxYmin + xTxtYdodge * xTxtYgap),
							 hjust = ifelse(xTxtX == "R", 1, 0),
							 vjust = ifelse(xTxtY == "U", 1, 0),
							 label = xTxtLab, 
							 color = xxColor_OUD,
							 parse = F, size = xxFontSize_SigTxt[szInd]/.pt, family = "STIX Two Math"
			)
	}
	
	#### age ----
	
	xdat <- dat %>% dplyr::select(all_of(c(xVar, "Grp", C, "Study"))) %>% tidyr::drop_na()
	
	xMdl <- lmer(shiRFormula(xVar, c("Grp*Age", C, RFX)), data = xdat)
	xMdl@call$data <- xdat
	xFx <- effects::Effect(c("Grp", "Age"), mod = xMdl, xlevels = list(Age = seq(from = 18, to = 62, by = .05))) %>% as.data.frame() # get ribbon and stuff
	xFx0 <- effects::Effect(c("Grp", "Age"), mod = xMdl, xlevels = list(Age = unique(xdat$Age), Grp = unique(xdat$Grp))) %>% as.data.frame() %>% droplevels() # get ribbon and stuff
	xdat[[xVar]] <- residuals(xMdl)
	for (k in 1:nrow(xdat)) {
		xdat[[xVar]][k] <- xdat[[xVar]][k] + xFx0 %>% dplyr::filter(Grp==xdat$Grp[k] & Age==xdat$Age[k]) %>% {.$fit}
	}
	
	xLevelOld <- c("OUD", "HC")
	xLevelNew <- c("OUD", "Non-OUD")
	
	xxAge <- ggplot() +
		geom_point(
			data = xdat,
			aes(x = Age, y = !!sym(xVar), color = Grp),
			size = xxScatter_PointSize[szInd],
			alpha = xxScatter_PointAlpha[szInd],
			stroke = 0
		) +
		geom_line(
			data = xFx,
			aes(x = Age, y = fit, color = Grp),
			size = xxTrend_LineSize[szInd]
		) +
		geom_ribbon(
			data = xFx,
			aes(x = Age, ymin = lower, ymax = upper, fill = Grp),
			alpha = xxTrend_RibbonAlpha[szInd]
		) +
		scale_color_manual(values = c(xxColor_OUD,xxColor_HC), breaks = xLevelOld, labels = xLevelNew) +
		scale_fill_manual(values = c(xxColor_OUD,xxColor_HC),	breaks = xLevelOld,	labels = xLevelNew) +
		theme_grey() +
		ggtitle(xTitle) +
		labs(
			x = "Age (years)",
			y = "Network segregation"
		) +
		theme(
			panel.grid.minor = element_blank(),
			axis.title.y = element_text(size = xxFontSize_AxisTitle[szInd], color = "black"),
			axis.title.x = element_text(size = xxFontSize_AxisTitle[szInd], color = "black"),
			axis.text.y = element_text(size = xxFontSize_AxisTxt[szInd], color = "black"),
			axis.text.x = element_text(size = xxFontSize_AxisTxt[szInd], color = "black"),
			plot.title = element_text(size = xxFontSize_PlotTitle[szInd], color = "black", face = "bold", hjust = .5),
			legend.position = "bottom",
			legend.box = "horizontal",
			legend.direction = "horizontal",
			legend.title = element_blank(),
			legend.text = element_text(size = xxFontSize_LegTxt[szInd], color = "black"),
			legend.key = element_rect(fill = "white")
		)
	xxYmin <- min(xdat[[xVar]])
	xxYmax <- max(xdat[[xVar]])
	xxYgap <- xxYmax - xxYmin
	xxXmin <- min(xdat$Age)
	xxXmax <- max(xdat$Age)
	xxXgap <- xxXmax - xxXmin
	xxAge <- xxAge +
		coord_cartesian(ylim = c(xxYmin, xxYmax), xlim = c(xxXmin, xxXmax)) +
		scale_y_continuous(
			labels = function(x) sprintf(xxNumFormat_Y[szInd], x), 
			expand = expansion(mult = .02),
			breaks = if (is.na(xxPlotDat$Age_yTick[i])) waiver() else xxPlotDat$Age_yTick[[i]]
		) +
		scale_x_continuous(
			expand = expansion(mult = .02)
		)
	
	if (!is.na(xxPlotDat$Age_xy[i])) {
		xTxtX <- xxPlotDat$Age_xy[i] %>% substr(3,3) # L or R
		xTxtY <- xxPlotDat$Age_xy[i] %>% substr(1,1) # L0 or L1 or U0 or U1, 1 for adding a one-line gap
		xTxtYdodge <- xxPlotDat$Age_xy[i] %>% substr(2,2) %>% as.numeric()
		xTxtX3 <- xxPlotDat$Age_xy[i] %>% substr(6,6) # L or R
		xTxtY3 <- xxPlotDat$Age_xy[i] %>% substr(4,4) # L0 or L1 or U0 or U1, 1 for adding a one-line gap
		xTxtY3dodge <- xxPlotDat$Age_xy[i] %>% substr(5,5) %>% as.numeric()
		xTxtLab1 <- sprintf("β=%.2f%s", xxPlotDat$AgeOUD_slp[i], xxPlotDat$AgeOUD_pTxt[i]) %>% {gsub("-", "–", .)}
		xTxtLab2 <- sprintf("β=%.2f%s", xxPlotDat$AgeHC_slp[i], xxPlotDat$AgeHC_pTxt[i]) %>% {gsub("-", "–", .)}
		xTxtNewline <- nchar(xxPlotDat$Age_xy[i]) == 7
		xTxtLab3 <- sprintf("%sβ=%.2f%s", ifelse(xTxtNewline,sprintf("pooled:\n"),"pooled: "), xxPlotDat$Age_slp[i], xxPlotDat$Age_pTxt[i]) %>% {gsub("-", "–", .)}
		xTxtYgap <- .05 * xxYgap
		xTxtY1 <- ifelse(xTxtY == "U", xxYmax - xTxtYgap * (0 + xTxtYdodge), xxYmin + xTxtYgap * (1 + xTxtYdodge))
		xTxtY2 <- ifelse(xTxtY == "U", xxYmax - xTxtYgap * (1 + xTxtYdodge), xxYmin + xTxtYgap * (0 + xTxtYdodge))
		xTxtY3 <- ifelse(xTxtY3 == "U", xxYmax - xTxtYgap * (0 + xTxtY3dodge), xxYmin + xTxtYgap * (0 + xTxtY3dodge))
		xxAge <- xxAge +
			annotate("text", 
							 x = ifelse(xTxtX == "R", xxXmax, xxXmin),
							 y = xTxtY1,
							 hjust = ifelse(xTxtX == "R", 1, 0),
							 vjust = ifelse(xTxtY == "U", 1, 0),
							 label = xTxtLab1,
							 color = xxColor_OUD,
							 parse = F, size = xxFontSize_SigTxt[szInd]/.pt, family = "STIX Two Math"
			) + 
			annotate("text", 
							 x = ifelse(xTxtX == "R", xxXmax, xxXmin),
							 y = xTxtY2,
							 hjust = ifelse(xTxtX == "R", 1, 0),
							 vjust = ifelse(xTxtY == "U", 1, 0),
							 label = xTxtLab2,
							 color = xxColor_HC,
							 parse = F, size = xxFontSize_SigTxt[szInd]/.pt, family = "STIX Two Math"
			) +
			annotate("text", 
							 x = ifelse(xTxtX3 == "R", xxXmax, xxXmin),
							 y = xTxtY3,
							 hjust = ifelse(xTxtX3 == "R", 1, 0),
							 vjust = ifelse(xTxtY3 == "U", 1, 0),
							 label = xTxtLab3,
							 color = "gray25",
							 lineheight = .8,
							 parse = F, size = xxFontSize_SigTxt[szInd]/.pt, family = "STIX Two Math"
			)
	}
	
	#### done ----
	
	return(list(xxGrp=xxGrp,xxYrs=xxYrs,xxAge=xxAge))
	
}


#### loop ----

GG <- list()

for (i in 1:2) { GG[[i]] <- oudrest_plot(i, dat, xxPlotDat, C, 1) }
for (i in 3:12) { GG[[i]] <- oudrest_plot(i, dat, xxPlotDat, C, 2) }


#### merge ----

PLOT_FUN_1 <- function(ggIn) {
	gg <- (ggIn[[1]] + ggIn[[2]]) +
		plot_annotation(tag_levels = "A",	tag_prefix = "(",	tag_suffix = ")") +
		plot_layout(guides = "collect") & theme(
			legend.position = "bottom",
			legend.box = "horizontal",
			legend.direction = "horizontal",
			plot.tag = element_text(family = "Arial Black", size = 90/2.8),
			plot.tag.position = c(.1, 1)
		)
	return(gg)
}

PLOT_FUN_2 <- function(ggIn) {
	gg <- (ggIn[[1]] + ggIn[[2]] + ggIn[[3]] + ggIn[[4]] + ggIn[[5]] + ggIn[[6]] + ggIn[[7]] + ggIn[[8]] + ggIn[[9]] + ggIn[[10]]) +
		plot_annotation(tag_levels = "A",	tag_prefix = "(",	tag_suffix = ")") +
		plot_layout(guides = "collect", ncol = 5, nrow = 2) & theme(
			legend.position = "bottom", 
			legend.box = "horizontal", 
			legend.direction = "horizontal",
			plot.tag = element_text(family = "Arial Black", size = 50/2.8),
			plot.tag.position = c(.1, 1)
		)
	return(gg)
}


#### print ----

gg1_Age <- PLOT_FUN_1( lapply(GG[1:2], function(x){x$xxAge}) )
gg1_Yrs <- PLOT_FUN_1( lapply(GG[1:2], function(x){x$xxYrs}) )
gg1_Grp <- PLOT_FUN_1( lapply(GG[1:2], function(x){x$xxGrp}) )

# gg2_Age <- PLOT_FUN_2( lapply(GG[3:12], function(x){x$xxAge}) )
gg2_Yrs <- PLOT_FUN_2( lapply(GG[3:12], function(x){x$xxYrs}) )
gg2_Grp <- PLOT_FUN_2( lapply(GG[3:12], function(x){x$xxGrp}) )

ggsave("R.PlotOverall_Age.tif", plot=gg1_Age, units="in", width=15, height=9, dpi=300, compression = 'lzw', device = ragg::agg_tiff)
ggsave("R.PlotOverall_Yrs.tif", plot=gg1_Yrs, units="in", width=15, height=9, dpi=300, compression = 'lzw', device = ragg::agg_tiff)
ggsave("R.PlotOverall_Grp.tif", plot=gg1_Grp, units="in", width=15, height=9, dpi=300, compression = 'lzw', device = ragg::agg_tiff)

# ggsave("R.PlotIndivid_Age.tif", plot=gg2_Age, units="in", width=15, height=9, dpi=300, compression = 'lzw', device = ragg::agg_tiff)
ggsave("R.PlotIndivid_Yrs.tif", plot=gg2_Yrs, units="in", width=15, height=9, dpi=300, compression = 'lzw', device = ragg::agg_tiff)
ggsave("R.PlotIndivid_Grp.tif", plot=gg2_Grp, units="in", width=15, height=9, dpi=300, compression = 'lzw', device = ragg::agg_tiff)
