source('oudrestR.Init.R')

#### data import ----

load("oudrestR.Data.DemClinCov.RData")

load("oudrestR.Data.Seg_Power264.RData")
load("oudrestR.Data.Tsnr_Power264.RData")
load("oudrestR.Data.Graph_Power264.RData")
datSegNet_Power264 <- func_DatJoin(datBehav, datSegNet, datTsnr) %>% dplyr::filter(!(FD_mean > 0.5))
datSegNodal_Power264 <- func_DatJoin(datBehav, datSegNodal) %>% dplyr::filter(!(FD_mean > 0.5))
datGraph_Power264 <- func_DatJoin(datBehav, datGraph) %>% dplyr::filter(!(FD_mean > 0.5))
attr(datGraph_Power264, "Sparsity") <- GraphSparsity

load("oudrestR.Data.Seg_Power264Combat.RData")
load("oudrestR.Data.Graph_Power264Combat.RData")
datSegNet_Power264Combat <- func_DatJoin(datBehav, datSegNet) %>% dplyr::filter(!(FD_mean > 0.5))
datGraph_Power264Combat <- func_DatJoin(datBehav, datGraph) %>% dplyr::filter(!(FD_mean > 0.5))
attr(datGraph_Power264Combat, "Sparsity") <- GraphSparsity

load("oudrestR.Data.Seg_Schaefer1000.RData")
load("oudrestR.Data.Graph_Schaefer1000.RData")
datSegNet_Schaefer1000 <- func_DatJoin(datBehav, datSegNet) %>% dplyr::filter(!(FD_mean > 0.5))
datGraph_Schaefer1000 <- func_DatJoin(datBehav, datGraph) %>% dplyr::filter(!(FD_mean > 0.5))
attr(datGraph_Schaefer1000, "Sparsity") <- GraphSparsity

rm(datSegNet,datSegNodal,datGraph,datTsnr,GraphSparsity)


#### var def ----

C <- c("Age","Sex","Race","FD_mean","StmUD","IsSmoker","RestTr","RestDur")

varSegNet_Power264 <- c(
	datSegNet_Power264 %>% func_VarList("^SegOverall_"),
	datSegNet_Power264 %>% func_VarList("^SegIndivid_")
)
varSegNodal_Power264 <- c(
	datSegNodal_Power264 %>% func_VarList("^SegNodal_")
)
varGraph_Power264 <- c(
	datGraph_Power264 %>% func_VarList("^Graph_")
)
varTsnr_Power264 <- c(
	datSegNet_Power264 %>% func_VarList("^TsnrOverall_"),
	datSegNet_Power264 %>% func_VarList("^TsnrIndivid_")
)
stopifnot(identical(varSegNet_Power264,gsub("Tsnr","Seg",varTsnr_Power264,fixed=F)))

varSegNet_Power264Combat <- c(
	datSegNet_Power264Combat %>% func_VarList("^SegOverall_"),
	datSegNet_Power264Combat %>% func_VarList("^SegIndivid_")
)
varGraph_Power264Combat <- c(
	datGraph_Power264Combat %>% func_VarList("^Graph_")
)

varSegNet_Schaefer1000 <- c(
	datSegNet_Schaefer1000 %>% func_VarList("^SegOverall_"),
	datSegNet_Schaefer1000 %>% func_VarList("^SegIndivid_")
)
varGraph_Schaefer1000 <- c(
	datGraph_Schaefer1000 %>% func_VarList("^Graph_")
)

fdrSegNet_Power264 <- list(
	datSegNet_Power264 %>% func_VarList("^SegIndivid_As"),
	datSegNet_Power264 %>% func_VarList("^SegIndivid_Sm")
)
fdrSegNodal_Power264 <- list(
	datSegNodal_Power264 %>% func_VarList("SegNodal_AsCo"),
	datSegNodal_Power264 %>% func_VarList("SegNodal_AsDa"),
	datSegNodal_Power264 %>% func_VarList("SegNodal_AsDm"),
	datSegNodal_Power264 %>% func_VarList("SegNodal_AsFp"),
	datSegNodal_Power264 %>% func_VarList("SegNodal_AsSn"),
	datSegNodal_Power264 %>% func_VarList("SegNodal_AsVa"),
	datSegNodal_Power264 %>% func_VarList("SegNodal_OtCb"),
	datSegNodal_Power264 %>% func_VarList("SegNodal_OtMr"),
	datSegNodal_Power264 %>% func_VarList("SegNodal_OtSu"),
	datSegNodal_Power264 %>% func_VarList("SegNodal_SmAu"),
	datSegNodal_Power264 %>% func_VarList("SegNodal_SmHa"),
	datSegNodal_Power264 %>% func_VarList("SegNodal_SmMo"),
	datSegNodal_Power264 %>% func_VarList("SegNodal_SmVi")
)
fdrSegNet_Power264Combat <- list(
	datSegNet_Power264Combat %>% func_VarList("^SegIndivid_As"),
	datSegNet_Power264Combat %>% func_VarList("^SegIndivid_Sm")
)
fdrSegNet_Schaefer1000 <- list(
	datSegNet_Schaefer1000 %>% func_VarList("^SegIndivid_")
)


#### stat ----

##### Descriptive ~ Grp ----
Tb.Descrip <- func_Stat_Descrip(datBehav)

##### Power264, SegNet ~ Grp + C ----
Tb.Grp.SegNet <- func_Stat_mdl.Grp(
	datSegNet_Power264,
	varSegNet_Power264,
	fdrSegNet_Power264,
	C
)$Tb

##### Power264, SegNodal ~ Grp + C ----
Tb.Grp.SegNodal <- func_Stat_mdl.Grp(
	datSegNodal_Power264,
	varSegNodal_Power264,
	fdrSegNodal_Power264,
	C
)$Tb

##### Power264, Graph ~ Grp + C ----
Tb.Grp.Graph <- func_Stat_mdl.Grp(
	datGraph_Power264,
	varGraph_Power264,
	NULL,
	C
)$Tb

##### Power264Combat, SegNet ~ Grp + C ----
Tb.GrpCombat.SegNet <- func_Stat_mdl.Grp(
	datSegNet_Power264Combat,
	varSegNet_Power264Combat,
	fdrSegNet_Power264Combat,
	C
)$Tb

##### Power264Combat, Graph ~ Grp + C ----
Tb.GrpCombat.Graph <- func_Stat_mdl.Grp(
	datGraph_Power264Combat,
	varGraph_Power264Combat,
	NULL,
	C
)$Tb

##### Schaefer1000, SegNet ~ Grp + C ----
Tb.GrpSchaefer.SegNet <- func_Stat_mdl.Grp(
	datSegNet_Schaefer1000,
	varSegNet_Schaefer1000,
	fdrSegNet_Schaefer1000,
	C
)$Tb

##### Schaefer1000, Graph ~ Grp + C ----
Tb.GrpSchaefer.Graph <- func_Stat_mdl.Grp(
	datGraph_Schaefer1000,
	varGraph_Schaefer1000,
	NULL,
	C
)$Tb

##### Power264, SegNet ~ Yrs + C ----
Tb.Yrs.SegNet <- func_Stat_mdl.Yrs(
	datSegNet_Power264,
	varSegNet_Power264,
	fdrSegNet_Power264,
	C
)$Tb

##### Power264, SegNodal ~ Yrs + C ----
Tb.Yrs.SegNodal <- func_Stat_mdl.Yrs(
	datSegNodal_Power264,
	varSegNodal_Power264,
	fdrSegNodal_Power264,
	C
)$Tb

##### Power264, SegNet ~ Medication + C ----
Tb.Med.SegNet <- func_Stat_mdl.Med(
	datSegNet_Power264,
	varSegNet_Power264,
	fdrSegNet_Power264,
	C
)$Tb

##### Power264, SegNet ~ Days + C ----
Tb.Days.SegNet <- func_Stat_mdl.Days(
	datSegNet_Power264,
	varSegNet_Power264,
	fdrSegNet_Power264,
	C
)$Tb

##### Power264, SegNet ~ HxOD + C ----
Tb.HxOD.SegNet <- func_Stat_mdl.HxOD(
	datSegNet_Power264,
	varSegNet_Power264,
	fdrSegNet_Power264,
	C
)$Tb

##### Power264, SegNet ~ Grp*Age + C ----
Tb.GrpAgeInt.SegNet <- func_Stat_mdl.GrpAgeInt(
	datSegNet_Power264,
	varSegNet_Power264,
	fdrSegNet_Power264,
	C
)$Tb

##### Power264, SegNet ~ Grp*Sex + C ----
Tb.GrpSexInt.SegNet <- func_Stat_mdl.GrpSexInt(
	datSegNet_Power264,
	varSegNet_Power264,
	fdrSegNet_Power264,
	C
)$Tb

##### Power264, Tsnr ~ Net, SegNet ~ Tsnr*Net ----
x <- func_Stat_Tsnr(datSegNet_Power264)
Tb.TsnrLong.anv <- x$Tb_anv
Tb.TsnrLong.em <- x$Tb_em

##### Power264, SegNet ~ Grp*Tsnr + C ----
Tb.GrpTsnrInt.SegNet <- func_Stat_mdl.GrpTsnrInt(
	datSegNet_Power264,
	varSegNet_Power264,
	fdrSegNet_Power264,
	C
)$Tb

##### Power264, SegNet ~ Yrs*Tsnr + C ----
Tb.YrsTsnrInt.SegNet <- func_Stat_mdl.YrsTsnrInt(
	datSegNet_Power264,
	varSegNet_Power264,
	fdrSegNet_Power264,
	C
)$Tb

##### Power264, SegNet ~ Grp + C (Robust) ----
Tb.GrpRob.SegNet <- func_Stat_mdl.Grp(
	datSegNet_Power264,
	varSegNet_Power264,
	fdrSegNet_Power264,
	C,
	OutlierCtr = "rob"
)$Tb

##### Power264, SegNet ~ Grp + C (Resid2) ----
Tb.GrpResid2.SegNet <- func_Stat_mdl.Grp(
	datSegNet_Power264,
	varSegNet_Power264,
	fdrSegNet_Power264,
	C,
	OutlierCtr = 2
)$Tb

##### Power264, SegNet ~ Grp + C (Resid4) ----
Tb.GrpResid4.SegNet <- func_Stat_mdl.Grp(
	datSegNet_Power264,
	varSegNet_Power264,
	fdrSegNet_Power264,
	C,
	OutlierCtr = 4
)$Tb

##### Power264, SegNet ~ Yrs + C (Robust) ----
Tb.YrsRob.SegNet <- func_Stat_mdl.Yrs(
	datSegNet_Power264,
	varSegNet_Power264,
	fdrSegNet_Power264,
	C,
	OutlierCtr = "rob"
)$Tb

##### Power264, SegNet ~ Yrs + C (Resid2) ----
Tb.YrsResid2.SegNet <- func_Stat_mdl.Yrs(
	datSegNet_Power264,
	varSegNet_Power264,
	fdrSegNet_Power264,
	C,
	OutlierCtr = 2
)$Tb

##### Power264, SegNet ~ Yrs + C (Resid4) ----
Tb.YrsResid4.SegNet <- func_Stat_mdl.Yrs(
	datSegNet_Power264,
	varSegNet_Power264,
	fdrSegNet_Power264,
	C,
	OutlierCtr = 4
)$Tb

##### Descriptive ~ YrsMiss ----
Tb.Descrip.YrsMiss <- func_Stat_Descrip(
	datBehav %>% dplyr::filter(Grp=="OUD") %>% dplyr::mutate(YrsMiss=as.factor(is.na(YrsOpUse))),
	"YrsMiss"
)

##### Power264, SegNet ~ Yrs + Medication + C ----
Tb.YrsMed.SegNet <- func_Stat_mdl.Yrs(
	datSegNet_Power264,
	varSegNet_Power264,
	fdrSegNet_Power264,
	c("Medication",C)
)$Tb

##### Power264, SegNet ~ Yrs + C (IPW) ----
xOut <- func_Stat_Ipw(
	datSegNet_Power264 %>% dplyr::mutate(YrsMiss = as.integer(is.na(YrsOpUse))) %>% dplyr::filter(Grp=="OUD"),
	LgtY = "YrsMiss",
	LgtX = c("Study","Sex","Race","Medication","AlcUD")
)
Tb.YrsIpw <- data.frame(
	Hoslem_Chi2 = xOut$HoslemStat$statistic,
	df = xOut$HoslemStat$parameter,
	p.value = xOut$HoslemStat$p.value,
	xOut$WeiSum %>% t() %>% as.data.frame()
)
Tb.YrsIpw.SegNet <- func_Stat_mdl.Yrs(
	datSegNet_Power264,
	varSegNet_Power264,
	fdrSegNet_Power264,
	C,
	Weight = xOut$datWeight
)$Tb

##### Descriptive ~ HxODMiss ----
Tb.Descrip.HxODMiss <- func_Stat_Descrip(
	datBehav %>% dplyr::filter(Grp=="OUD") %>% dplyr::mutate(HxODMiss=as.factor(is.na(HxOD))),
	"HxODMiss"
)

##### Power264, SegNet ~ HxOD + C (IPW) ----
xOut <- func_Stat_Ipw(
	datSegNet_Power264 %>% dplyr::mutate(HxODMiss = as.integer(is.na(HxOD))) %>% dplyr::filter(Grp=="OUD" & Study!="St5" & Study!="St7"),
	LgtY = "HxODMiss",
	LgtX = c("Study","Sex","Race","Medication","AlcUD")
)
Tb.HxODIpw <- data.frame(
	Hoslem_Chi2 = xOut$HoslemStat$statistic,
	df = xOut$HoslemStat$parameter,
	p.value = xOut$HoslemStat$p.value,
	xOut$WeiSum %>% t() %>% as.data.frame()
)
Tb.HxODIpw.SegNet <- func_Stat_mdl.HxOD(
	datSegNet_Power264,
	varSegNet_Power264,
	fdrSegNet_Power264,
	C,
	Weight = xOut$datWeight
)$Tb


#### output ----

list(
	Tb.Descrip            = Tb.Descrip            ,
	Tb.Grp.SegNet         = Tb.Grp.SegNet         ,
	Tb.Grp.SegNodal       = Tb.Grp.SegNodal       ,
	Tb.Grp.Graph          = Tb.Grp.Graph          ,
	Tb.GrpCombat.SegNet   = Tb.GrpCombat.SegNet   ,
	Tb.GrpCombat.Graph    = Tb.GrpCombat.Graph    ,
	Tb.GrpSchaefer.SegNet = Tb.GrpSchaefer.SegNet ,
	Tb.GrpSchaefer.Graph  = Tb.GrpSchaefer.Graph  ,
	Tb.Yrs.SegNet         = Tb.Yrs.SegNet         ,
	Tb.Yrs.SegNodal       = Tb.Yrs.SegNodal       ,
	Tb.Med.SegNet         = Tb.Med.SegNet         ,
	Tb.Days.SegNet        = Tb.Days.SegNet        ,
	Tb.HxOD.SegNet        = Tb.HxOD.SegNet        ,
	Tb.GrpAgeInt.SegNet   = Tb.GrpAgeInt.SegNet   ,
	Tb.GrpSexInt.SegNet   = Tb.GrpSexInt.SegNet   ,
	Tb.TsnrLong.anv       = Tb.TsnrLong.anv       ,
	Tb.TsnrLong.em        = Tb.TsnrLong.em        ,
	Tb.GrpTsnrInt.SegNet  = Tb.GrpTsnrInt.SegNet  ,
	Tb.YrsTsnrInt.SegNet  = Tb.YrsTsnrInt.SegNet  ,
	Tb.GrpRob.SegNet      = Tb.GrpRob.SegNet      ,
	Tb.GrpResid2.SegNet   = Tb.GrpResid2.SegNet   ,
	Tb.GrpResid4.SegNet   = Tb.GrpResid4.SegNet   ,
	Tb.YrsRob.SegNet      = Tb.YrsRob.SegNet      ,
	Tb.YrsResid2.SegNet   = Tb.YrsResid2.SegNet   ,
	Tb.YrsResid4.SegNet   = Tb.YrsResid4.SegNet   ,
	Tb.Descrip.YrsMiss    = Tb.Descrip.YrsMiss    ,
	Tb.YrsMed.SegNet      = Tb.YrsMed.SegNet      ,
	Tb.YrsIpw             = Tb.YrsIpw             ,
	Tb.YrsIpw.SegNet      = Tb.YrsIpw.SegNet      ,
	Tb.Descrip.HxODMiss   = Tb.Descrip.HxODMiss   ,
	Tb.HxODIpw            = Tb.HxODIpw            ,
	Tb.HxODIpw.SegNet     = Tb.HxODIpw.SegNet     
) %>% openxlsx::write.xlsx(file = "oudrestR.Summ.xlsx")
