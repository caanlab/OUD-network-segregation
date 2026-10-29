# R version 4.5.2
library(crayon) # version 1.5.3
library(magrittr) # version 2.0.4
library(tidyverse) # version 2.0.0
library(tibble) # version 3.3.1
library(ragg) # version 1.5.0
library(patchwork) # version 1.3.2
library(lme4) # version 2.0.6
library(lmerTest) # version 3.2.1
library(MASS) # version 7.3.65
library(robustlmm) # version 3.5.0.2
library(car) # version 3.1.5
library(effectsize) # version 1.0.1
library(emmeans) # version 2.0.1
library(effects) # version 4.2.5
library(r2glmm) # version 0.1.3
library(ResourceSelection) # version 0.3.6
library(neuroCombat) # version 1.0.14
library(igraph) # version 2.3.0
library(aricode) # version 1.0.3
library(future.apply) # version 1.20.2
library(openxlsx) # version 4.2.8.1


rm(list=ls())

setwd('./')

options(
	contrasts = c("contr.sum", "contr.poly"),
	emmeans = list(pbkrtest.limit = 50000),
	future.globals.maxSize = 4 * 1024^3,
	pillar.sigfig = 6
)


#### basic func def ----

####
shiRIf <- function(test,y,n) { if (test) {return(y)} else {return(n)} }

####
shiRFormula <- function(y,x) {
	pt1 <- paste(y,"~")
	pt2 <- paste(x,collapse="+")
	return(paste(pt1,pt2))
}

####
shiRDescriptive <- function(df, GrpVar=NULL) {
	FUNC_LIST <- list(
		"N"    = function(x)sum(!is.na(x)),
		"MEAN" = function(x)mean(x,na.rm=T),
		"SD"   = function(x)sd(x,na.rm=T),
		"SE"   = function(x)(sd(x,na.rm=T))/sqrt(sum(!is.na(x))),
		"MED"  = function(x)quantile(x,probs=0.50,na.rm=T),
		"Q25"  = function(x)quantile(x,probs=0.25,na.rm=T),
		"Q75"  = function(x)quantile(x,probs=0.70,na.rm=T),
		"MIN"  = function(x)min(x,na.rm=T),
		"MAX"  = function(x)max(x,na.rm=T),
		"SUM"  = function(x)sum(x,na.rm=T)
	)
	if (length(GrpVar)==0) {
		df %<>% lapply( shiRFact2Num ) %>% tibble::as.tibble()
		df %<>% dplyr::select(-c(names(df)[sapply(df,function(x)all(is.na(x)))]))
		Descr <- data.frame(Var=names(df))
		for (i in 1:length(FUNC_LIST)) {
			Descr[[names(FUNC_LIST)[i]]]<- sapply(df,FUNC_LIST[[i]])
		}
	} else {
		BY_LIST <- df %>% dplyr::select(!!sym(GrpVar))
		df %<>% dplyr::select(-!!sym(GrpVar)) %>% lapply( shiRFact2Num ) %>% tibble::as.tibble()
		df %<>% dplyr::select(-c(names(df)[sapply(df,function(x)all(is.na(x)))]))
		Descr <- aggregate(x=df, by=BY_LIST, FUN=FUNC_LIST[[1]]) %>% reshape2::melt(id.vars=GrpVar, value.name=names(FUNC_LIST)[1])
		for (i in 2:length(FUNC_LIST)) {
			Descr %<>% cbind( aggregate(x=df, by=BY_LIST, FUN=FUNC_LIST[[i]]) %>% reshape2::melt(id.vars=GrpVar, value.name=names(FUNC_LIST)[i]) %>% dplyr::select(-c(GrpVar,"variable")) )
		}
		Descr %<>% dplyr::rename("Var" = "variable")
	}
	return(Descr)
}

####
shiRFact2Num <- function(xFact) {
	if (is.factor(xFact))  {
		if (lapply(levels(xFact),is.character) %>% unlist %>% any) {
			xFact <- as.numeric(xFact)
		} else {
			xFact <- as.numeric(as.character(xFact))
		}
	}
	return(xFact)
}

####
shiRFormatPval <- function(pval,sep="") {
	pstring <- rep("",length(pval))
	for (i in 1:length(pval)) {
		pstring[i] <- ifelse(
			pval[i]>=0.995,paste(">","0.99",sep=sep),ifelse(
				pval[i]>=0.0995,paste("=",format(round(pval[i], 2), nsmall = 2),sep=sep),ifelse(
					pval[i]>=0.001,paste("=",format(round(pval[i], 3), nsmall = 3),sep=sep),paste("<","0.001",sep=sep)
				)))
	}
	return(pstring)
}

####
shiRDescriptivePrint <- function(
		df, # data frame
		GrpVar = NULL, # which group variable(s)
		Ignore = NULL, # which variables to be ignored (e.g. subject IDs)
		DoTest = T, # whether to run group comparisons (always false unless exactly 2 groups and NumTest and CatTest are correctly specified)
		NumTest = "wilcox.test", # or t.test
		CatTest = "fisher.test", # or chisq.test
		digits = 2 # for rounding purpose, must be non-negative integer, may be Inf for no rounding
) {
	
	sym=dplyr::sym
	
	if (digits == Inf) {
		ROUND <- function(num)sprintf("%g",num)
	} else {
		ROUND <- function(num)sprintf(paste0("%.",digits,"f"),num)
	}
	
	GROUP = ".shiRDescriptivePrint.Grp"
	
	if (length(GrpVar)==0) {
		df %<>% dplyr::mutate(!!sym(GROUP):="All")
		df[[GROUP]] %<>% factor()
	} else if (length(GrpVar)==1) {
		df %<>% dplyr::mutate(!!sym(GROUP):=!!sym(GrpVar[1]))
	} else {
		df %<>% dplyr::mutate(!!sym(GROUP):=!!sym(GrpVar[1]))
		for (i in 2:length(GrpVar)) {
			df[[GROUP]] %<>% interaction(df[[GrpVar[i]]])
		}
	}
	
	df[[GROUP]] <- droplevels(df[[GROUP]])
	NGRP <- nlevels(df[[GROUP]])
	GRPLEV <- levels(df[[GROUP]])
	
	DOTEST <- DoTest && NGRP == 2
	
	VarList <- names(df) %>% setdiff(c(GrpVar,GROUP,Ignore))
	
	for (i in 1:length(VarList)) {
		if (!is.numeric(df[[VarList[i]]])) {
			df[[VarList[i]]] %<>% factor()
		}
	}
	ISFACTOR <- sapply(df %>% dplyr::select(all_of(VarList)),is.factor)
	
	# header
	
	N <- dplyr::count(df,!!sym(GROUP))$n
	TABLE <- "Variable"
	for (grp in 1:NGRP) {
		xTAB <- GRPLEV[grp] %>% paste0(" (n=",N[grp],")")
		TABLE %<>% c(xTAB)
	}
	
	# each variable
	for (i in 1:length(VarList)) {
		xdf <- df %>% dplyr::filter(!is.na(!!sym(VarList[i])))
		# variable name
		xTAB <- c(VarList[i], rep("",NGRP))
		TABLE %<>% rbind(xTAB)
		# no. of subjects
		xxtab <- table(xdf[[GROUP]])
		xN <- as.integer(xxtab[levels(xdf[[GROUP]])])
		xN[is.na(xN)] <- 0
		if (max(abs(N-xN))>0) {	TABLE %<>% rbind(c("No. of participants", xN)) }
		# stats for factor
		if (ISFACTOR[i]) {
			# column 0
			xTAB <- paste0(levels(df[[VarList[i]]]),", No. (%)")
			# column for each group
			for (grp in 1:NGRP) {
				xTAB1 <- dplyr::count(xdf %>% dplyr::filter(!!sym(GROUP)==GRPLEV[grp]), !!sym(VarList[i]), .drop=F)$n
				xTAB1 %<>% paste0(" (",(xTAB1/xN[grp]*100) %>% ROUND,"%)")
				xTAB %<>% cbind(xTAB1)
			}
			# stats for numerical
		} else {
			# column 0
			xTAB <- c("Mean (SD)","Median (IQR)")
			# column for each group
			for (grp in 1:NGRP) {
				xx <- xdf[[VarList[i]]][xdf[[GROUP]]==GRPLEV[grp]]
				xTAB1 <- c(mean(xx) %>% ROUND, median(xx) %>% ROUND)
				xTAB2 <- c(sd(xx) %>% ROUND,paste0(quantile(xx,.25) %>% ROUND,"–",quantile(xx,.75) %>% ROUND))
				xTAB1 %<>% paste0(" (", xTAB2, ")")
				xTAB %<>% cbind(xTAB1)
			}
		}
		# add to main table
		TABLE %<>% rbind(xTAB)
	}
	
	rownames(TABLE) <- NULL
	colnames(TABLE) <- NULL
	
	if ((tolower(NumTest) != "t.test" && tolower(NumTest) != "wilcox.test") || (tolower(CatTest) != "chisq.test" && tolower(CatTest) != "fisher.test")) {
		DOTEST = F
	}
	if (!DOTEST) {
		TABLE0 <- TABLE
		TABLE0[!sapply(TABLE0[,2],function(x)nchar(x)==0),1] %<>% {paste0("  ",.)}
		TABLE0[1,1] <- "Variable"
		print.table(TABLE0)
		return(TABLE)
	}
	
	if (tolower(NumTest) == "t.test") {
		NUMTEST = function(x,y)t.test(x,y)
	} else if (tolower(NumTest) == "wilcox.test") {
		NUMTEST = function(x,y)wilcox.test(x,y)
	}
	
	if (tolower(CatTest) == "chisq.test") {
		CATTEST = function(x,y)chisq.test(x,y)
	} else if (tolower(CatTest) == "fisher.test") {
		CATTEST <- function(x, y) {
			tryCatch(
				fisher.test(x, y),
				error = function(e) {
					fisher.test(x, y, simulate.p.value=T, B=100000)
				}
			)
		}
	}
	
	xTAB <- "p-value"
	xCNT <- 0
	for (i in 2:nrow(TABLE)) {
		if (TABLE[i,2] != "") {xTAB %<>% c("")} else {
			xCNT <- xCNT+1
			xVAR <- TABLE[i,1]
			if (ISFACTOR[xCNT]) {
				xSTAT <- tryCatch(CATTEST(df[[GROUP]],df[[xVAR]])$p.value,error=function(e)NA)
			} else {
				xSTAT <- tryCatch(NUMTEST(df[[xVAR]][df[[GROUP]]==GRPLEV[1]],df[[xVAR]][df[[GROUP]]==GRPLEV[2]])$p.value,error=function(e)NA)
			}
			xTAB %<>% c(paste0("p",shiRFormatPval(xSTAT)))
		}
	}
	
	TABLE %<>% cbind(xTAB)
	
	rownames(TABLE) <- NULL
	colnames(TABLE) <- NULL
	
	TABLE0 <- TABLE
	TABLE0[!sapply(TABLE0[,2],function(x)nchar(x)==0),1] %<>% {paste0("  ",.)}
	TABLE0[1,1] <- "Variable"
	print.table(TABLE0)
	return(TABLE)
	
}


#### study func def ----

####
func_DatJoin <- function(..., by = "Subj") {
	
	lstDat <- list(...)
	
	stopifnot(length(lstDat)>0)
	for (i in seq_along(lstDat)) {
		stopifnot(all(by %in% names(lstDat[[i]])))
		stopifnot(identical(lstDat[[1]][[by]], lstDat[[i]][[by]]))
	}
	
	dat <- lstDat[[1]]
	if (length(lstDat)>1) {	for (i in 2:length(lstDat)) {
		dat <- dplyr::left_join(dat, lstDat[[i]], by = by)
	}}
	
	return(dat)
	
}

####
func_VarList <- function(dat, pattern) {
	return(grep(paste0(pattern), names(dat), value=T))
}

####
func_FdrBy <- function(p, pname, pgrp=NULL) {
	pfdr <- length(p) %>% numeric() + NA
	if (is.null(pgrp)) { return(pfdr) }
	for (i in 1:length(pgrp)) {
		idx <- pname %in% pgrp[[i]]
		pfdr[idx] <- p.adjust(p[idx], method="fdr")
	}
	return(pfdr)
}

####
func_MeanPos <- function(x, ...) {
	return(mean(x[is.finite(x) & x>0], ...))
}

####
func_Edge2Mat <- function(datEdge) {
	
	EdgeCol <- setdiff(colnames(datEdge), "Subj")
	
	NodeAll <- EdgeCol %>% stringr::str_remove("^Edge_") %>% stringr::str_split("_") %>%
		unlist() %>% unique() %>% sort()
	
	NodeIdx <- data.frame(node = NodeAll,	idx = 1:length(NodeAll))
	
	nodes <- EdgeCol %>% stringr::str_remove("^Edge_") %>% stringr::str_split_fixed("_", 2)
	idx1 <- NodeIdx$idx[match(nodes[, 1], NodeIdx$node)]
	idx2 <- NodeIdx$idx[match(nodes[, 2], NodeIdx$node)]
	nNode <- nrow(NodeIdx)
	
	plan(multisession)
	AdjLst <- future_lapply(
		seq_len(nrow(datEdge)),
		function(i) {
			
			mat <- matrix(0, nNode, nNode)
			rownames(mat) <- NodeIdx$node
			colnames(mat) <- NodeIdx$node
			
			EdgeVec <- as.numeric(datEdge[i, EdgeCol])
			
			mat[cbind(idx1, idx2)] <- EdgeVec
			mat[cbind(idx2, idx1)] <- EdgeVec
			
			mat_miss <- mat
			diag(mat_miss) <- NA
			missingNodeIdx <- which(apply(mat_miss, 2, function(x) { all(is.na(x)) }))
			if (length(missingNodeIdx)==0) { 
				missingNode <- NULL
				stopifnot(all(!is.na(mat)))
			}	else {
				missingNode <- NodeIdx$node[missingNodeIdx]
				stopifnot(all(!is.na(mat[-missingNodeIdx,-missingNodeIdx])))
			}
			
			diag(mat) <- 0
			return(list(
				mat = mat,
				missing = missingNode
			))
			
		}
	)
}

####
func_ThresDens <- function(mat, density) {
	
	vals <- mat[upper.tri(mat)]
	nKeep <- round(length(vals) * density)
	cutoff <- sort(vals, T)[nKeep]
	mat[mat < cutoff] <- 0
	
	density_actual <- mean(mat[lower.tri(mat, diag=F)] != 0, na.rm=T)
	
	return(list(
		mat = mat,
		density_actual = density_actual
	))
	
}

####
func_LouvainRep <- function(g, nRun = 100) { # binary adj matrix
	
	PartAll <- list()
	QAll <- c()
	
	for (i in 1:nRun) {
		clu <- igraph::cluster_louvain(g)
		PartAll[[i]] <- igraph::membership(clu)
		QAll[i] <- igraph::modularity(g, membership(clu))
	}
	
	best <- which.max(QAll)
	
	return(list(
		PartBest = PartAll[[best]],
		QBest = QAll[best],
		QMean = mean(QAll)
	))
	
}

####
func_Prep_Combat <- function(datEdge, datBehav, fname_Edge_Combat.RData) {
	
	stopifnot(identical(datBehav$Subj, datEdge$Subj))
	
	datmat <- datEdge %>% select(-Subj) %>% as.matrix() %>% t()
	batch <- datBehav$Scanner %>% as.integer()
	mod <- model.matrix(~Grp, data=datBehav)
	
	datmatbat <- neuroCombat(dat=datmat, batch=batch, mod=mod)$dat.combat
	
	datEdgeCombat <- cbind(
		data.frame(Subj=datEdge$Subj),
		as.data.frame(t(datmatbat))
	)
	names(datEdgeCombat) <- names(datEdge)
	
	datEdge <- datEdgeCombat
	save(datEdge, file = fname_Edge_Combat.RData)
	
}

####
func_Prep_Graph <- function(datEdge, fname_Graph.RData) {
	
	#### prep
	
	Part0 <- colnames(datEdge) %>% setdiff("Subj") %>% stringr::str_remove("^Edge_") %>%	stringr::str_split("_") %>%
		unlist() %>% unique() %>%	sort() %>% substr(1,4) # first 4 char indicate network assignment
	names(Part0) <- NodeAll
	
	AdjLst <- func_Edge2Mat(datEdge)
	
	#### main
	
	Sparsity <- seq(.01, .5, by=.01)
	NodeName <- names(Part0)
	GraphOut <- vector("list", length(Sparsity))
	
	plan(multisession)
	for (s in seq_along(Sparsity)) {
		cat(Sparsity[s], "\n")
		
		GraphOut[[s]] <- list()
		GraphOut[[s]]$Sparsity <- Sparsity[s]
		
		GraphOut[[s]]$Out <- future_lapply(
			seq_along(AdjLst),
			function(i) {
				
				out <- list()
				out$Subj <- datEdge$Subj[i]
				mat <- AdjLst[[i]]$mat
				NodeMissing <- AdjLst[[i]]$missing
				stopifnot(identical(colnames(mat),NodeName))
				stopifnot(identical(rownames(mat),NodeName))
				
				mat[mat<0] <- 0
				diag(mat) <- 0
				if (is.null(NodeMissing)) {
					matAvail <- mat
					p0Avail <- Part0
				} else {
					matAvail <- mat[!NodeName %in% NodeMissing,	!NodeName %in% NodeMissing,	drop=F]
					p0Avail <- Part0[!NodeName %in% NodeMissing]
				}
				stopifnot(all(!is.na(matAvail)))
				
				xMatDen <- func_ThresDens(matAvail, Sparsity[s])
				matAvail <- xMatDen$mat
				matAvail[matAvail>0] <- 1
				out$SparsityActual <- xMatDen$density_actual
				matAvail_g <- matAvail %>% igraph::graph_from_adjacency_matrix(mode="undirected", diag=F)
				
				out$nNode <- matAvail_g %>% igraph::components() %>% {.$csize} %>% max()
				
				lv <- func_LouvainRep(matAvail_g, nRun=100)
				out$Qmean <- lv$QMean
				out$Qbest <- lv$QBest
				
				out$NMI <- aricode::NMI(unlist(lv$PartBest) %>% as.character(), p0Avail %>% as.character())
				
				out$EffGlo <- igraph::global_efficiency(matAvail_g, directed=F)
				out$EffLoc <- igraph::average_local_efficiency(matAvail_g, directed=F)
				
				out$CharPathLen <- igraph::mean_distance(matAvail_g, unconnected=T, directed=F)
				
				return(out)
				
			}
		)
		
	}
	
	#### output
	
	SparList <- data.frame(
		MinLargestCompPerc = sapply(GraphOut, function(m) min(unlist(lapply(m$Out, "[[", "nNode")))) / length(NodeName),
		Sparsity = sapply(GraphOut, "[[", "Sparsity")
	) 
	SparMin <- SparList %>% 
		dplyr::filter(MinLargestCompPerc>0.5) %>% 
		dplyr::filter(MinLargestCompPerc==min(MinLargestCompPerc)) %>% 
		{.$Sparsity}
	SparIdx <- which(SparList$Sparsity == SparMin)
	
	datGraph <- data.frame(Subj = datEdge$Subj)
	datGraph[["Graph_Qmean"      ]] <- sapply(seq_along(GraphOut[[1]]$Out), function(y) { mean(sapply(GraphOut[SparIdx:length(GraphOut)], function(x) { x$Out[[y]]$Qmean       }), na.rm=T) })
	datGraph[["Graph_Qbest"      ]] <- sapply(seq_along(GraphOut[[1]]$Out), function(y) { mean(sapply(GraphOut[SparIdx:length(GraphOut)], function(x) { x$Out[[y]]$Qbest       }), na.rm=T) })
	datGraph[["Graph_NMI"        ]] <- sapply(seq_along(GraphOut[[1]]$Out), function(y) { mean(sapply(GraphOut[SparIdx:length(GraphOut)], function(x) { x$Out[[y]]$NMI         }), na.rm=T) })
	datGraph[["Graph_EffGlo"     ]] <- sapply(seq_along(GraphOut[[1]]$Out), function(y) { mean(sapply(GraphOut[SparIdx:length(GraphOut)], function(x) { x$Out[[y]]$EffGlo      }), na.rm=T) })
	datGraph[["Graph_EffLoc"     ]] <- sapply(seq_along(GraphOut[[1]]$Out), function(y) { mean(sapply(GraphOut[SparIdx:length(GraphOut)], function(x) { x$Out[[y]]$EffLoc      }), na.rm=T) })
	datGraph[["Graph_CharPathLen"]] <- sapply(seq_along(GraphOut[[1]]$Out), function(y) { mean(sapply(GraphOut[SparIdx:length(GraphOut)], function(x) { x$Out[[y]]$CharPathLen }), na.rm=T) })
	
	GraphSparsity <- SparList$Sparsity[SparIdx:length(GraphOut)]
	
	save(datGraph, GraphSparsity, file = fname_Graph.RData)
	
}

####
func_Prep_Seg <- function(datEdge, NetCat, fname_Seg.RData) {
	# Power264:	      NetCat = list(As="As",Sm="Sm")
	# Schaefer1000:	  NetCat = list(All="")
	
	#### prep
	
	datEdge %<>% dplyr::select(!contains("UnKn"))
	
	AdjLst <- func_Edge2Mat(datEdge)
	
	#### main
	
	SegNode <- vector("list", length(AdjLst))
	SegNet <- vector("list", length(AdjLst))
	
	for (i in seq_along(AdjLst)) {
		cat("main:", i, "out of", length(AdjLst), "at", format(Sys.time()), "\n")
		
		mat <- AdjLst[[i]]$mat
		rowNet <- substr(rownames(mat), 1, 4)
		colNet <- substr(colnames(mat), 1, 4)
		wthNode <- sapply(seq_len(nrow(mat)), function(i) { func_MeanPos(mat[i, which(colNet==rowNet[i])], na.rm=T) })
		btwNode <- sapply(seq_len(nrow(mat)), function(i) { func_MeanPos(mat[i, which(colNet!=rowNet[i])], na.rm=T) })
		
		SegNode[[i]] <- (wthNode - btwNode) / wthNode
		names(SegNode[[i]]) <- paste0("SegNodal_", rownames(mat))
		
		NetName <- unique(rowNet)
		wthNet <- numeric(length(NetName))
		btwNet <- numeric(length(NetName))
		xSegNet <- numeric(length(NetName))
		
		for (j in seq_along(NetName)) {
			net <- NetName[j]
			wthNet[j] <- mat[rowNet==net, colNet==net, drop=F] %>% func_MeanPos(na.rm=T)
			btwNet[j] <- mat[rowNet==net, colNet!=net, drop=F] %>% func_MeanPos(na.rm=T)
			xSegNet[j] <- (wthNet[j] - btwNet[j]) / wthNet[j]
		}
		
		SegNet[[i]] <- xSegNet
		names(SegNet[[i]]) <- paste0("SegIndivid_",NetName)
		
	}
	
	stopifnot(all(sapply(SegNode, function(x) { identical(names(x), names(SegNode[[1]])) })))
	stopifnot(all(sapply(SegNet, function(x) { identical(names(x), names(SegNet[[1]])) })))
	
	SegOverall <- data.frame(Subj = datEdge$Subj)
	for (i in seq_along(NetCat)) {
		SegOverall[[paste0("SegOverall_",names(NetCat)[i])]] = rowMeans(DAT_Seg %>% dplyr::select(dplyr::starts_with(paste0("SegIndivid_",NetCat[[i]]))))
	}
	
	#### output
	
	datSegNodal <- cbind(
		data.frame(Subj = datEdge$Subj),
		do.call(rbind, SegNode) %>% as.data.frame()
	)
	
	datSetNet <- cbind(
		SegOverall,
		do.call(rbind, SegNet) %>% as.data.frame()
	)
	
	save(datSetNet, datSegNodal, file = fname_Seg.RData)

}

####
func_Stat_Descrip <- function(
		dat, 
		Grp = "Grp", 
		Var = c('Study','Age','Sex','Race','FD_mean','OpiUD','AlcUD','CanUD','StmUD','IsSmoker','YrsOpUse','DaysAbstin','Medication','HxOD'),
		NumTest="t.test",
		CatTest="chisq.test"
) {
	varIgnore <- names(dat)[!names(dat) %in% Var]
	Tb.Descrip <- shiRDescriptivePrint(dat, GrpVar=Grp, Ignore=varIgnore, NumTest=NumTest, CatTest=CatTest)
	return(Tb.Descrip)
}

####
func_Stat_Ipw <- function(Dat, LgtY, LgtX, Trunc = 0.025) {
	
	dat_lgt <- Dat[,c("Subj",LgtY,LgtX)] %>% na.omit()
	
	mdl.lgt <- glm(shiRFormula(LgtY,LgtX), data = dat_lgt, family = binomial(link="logit"))
	HoslemStat <- ResourceSelection::hoslem.test(x = dat_lgt[[LgtY]], y = fitted(mdl.lgt))
	
	dat_lgt %<>%
		dplyr::mutate(prob = predict(mdl.lgt, type="response")) %>%
		dplyr::mutate(!!sym(LgtY) := as.numeric(!!sym(LgtY))) %>% 
		dplyr::mutate(invwt = !!sym(LgtY)/prob + (1-!!sym(LgtY))/(1-prob))
	
	WeiSum <- c(
		WeiSum_Top10pct = dat_lgt %>% dplyr::slice_max(invwt,prop=0.1) %>% {sum(.$invwt)},
		WeiSum_HalfAll = sum(dat_lgt$invwt,na.rm=T) / 2
	)
	
	lgt_alpha <- quantile(dat_lgt$invwt,c(Trunc,1-Trunc))
	dat_lgt$invwt[dat_lgt$invwt<lgt_alpha[1]] <- lgt_alpha[1]
	dat_lgt$invwt[dat_lgt$invwt>lgt_alpha[2]] <- lgt_alpha[2]
	
	DatOut <- Dat %>% 
		dplyr::select(Subj) %>% 
		dplyr::left_join(dat_lgt %>% dplyr::select(invwt,Subj) %>% na.omit(), by="Subj")
	
	return(list(
		datWeight = DatOut,
		HoslemStat = HoslemStat,
		WeiSum = WeiSum
	))
	
}

####
func_Stat_Model <- function(Formula, Dat, OutlierCtr = NULL, Weight = NULL) {
	
	if (!is.null(Weight)) {
		Dat <- dplyr::left_join(Weight, Dat, by="Subj")
		Weight <- Dat$invwt
	}
	
	if (is.null(OutlierCtr)) { OutlierCtr <- NA }
	if (identical(OutlierCtr %>% tolower() %>% substr(1,3), "rob")) {
		MDL <- shiRIf(grepl("\\|",Formula), robustlmm::rlmer, MASS::rlm)
		OutlierCtr <- NA
	} else {
		MDL <- shiRIf(grepl("\\|",Formula), lmer, lm)
	}
	
	if (is.na(OutlierCtr) | is.infinite(OutlierCtr)) {
		mdl <- MDL(as.formula(Formula), data = Dat, weights = Weight)
		excl <- 0
	} else {
		mdl <- MDL(as.formula(Formula), data = Dat, weights = Weight)
		idx <- mdl %>% rstudent() %>% abs() %>% {is.na(.) | .<=OutlierCtr}
		excl <- sum(!idx)
		if (excl!=0) { mdl <- MDL(as.formula(Formula), data = Dat[idx,], weights = Weight[idx])	}
	}
	
	return(list(mdl = mdl, excl = excl))
	
}

#### Grp x Age
func_Stat_mdl.GrpAgeInt <- function(dat, Var, VarClust, C, RFX = "(1|Study)", OutlierCtr = NULL, Weight = NULL) {
	
	doRob <- identical(OutlierCtr %>% tolower() %>% substr(1,3), "rob")
	mdl <- list()
	Tb = data.frame(
		Var              = length(Var) %>% character(),
		Excl             = length(Var) %>% numeric(),
		F_AgeInt         = length(Var) %>% numeric(),
		df1_AgeInt       = length(Var) %>% numeric(),
		df2_AgeInt       = length(Var) %>% numeric(),
		p_AgeInt         = length(Var) %>% numeric(),
		pprint_AgeInt    = length(Var) %>% character(),
		pFDR_AgeInt      = length(Var) %>% numeric(),
		pFDRprint_AgeInt = length(Var) %>% character(),
		eta2_AgeInt      = length(Var) %>% numeric(),
		spR2_AgeInt      = length(Var) %>% numeric(),
		gvif_AgeInt      = length(Var) %>% numeric(),
		emtr_Age_HC      = length(Var) %>% numeric(),
		emtr_Age_HC_LCI  = length(Var) %>% numeric(),
		emtr_Age_HC_UCI  = length(Var) %>% numeric(),
		emtr_Age_OUD     = length(Var) %>% numeric(),
		emtr_Age_OUD_LCI = length(Var) %>% numeric(),
		emtr_Age_OUD_UCI = length(Var) %>% numeric(),
		STRING = length(Var) %>% character()
	)
	
	dat_agec <- dat %>% dplyr::mutate(Age = Age - mean(Age, na.rm=T))
	
	for (i in 1:length(Var)) {
		
		paste0(" ", as.character(match.call()[[1]]), " --- ", Var[i], " \n") %>% crayon::black() %>% crayon::bgYellow() %>% cat()
		
		mdl <- func_Stat_Model(
			shiRFormula(Var[i],c("Grp*Age",C,RFX)), 
			Dat = dat_agec,
			OutlierCtr = OutlierCtr,
			Weight = Weight
		)
		mdl[[Var[i]]] <- mdl$mdl
		excl <- mdl$excl
		
		anv <- mdl[[Var[i]]] %>% anova(ddf=shiRIf(doRob,"satterthwaite","Satterthwaite"))
		if (!doRob) { eta2 <- anv %>% effectsize::eta_squared(partial=T) }
		if (!doRob) { spR2 <- mdl[[Var[i]]] %>% r2glmm::r2beta(method="nsj") }
		if (!doRob) { gvif <- mdl[[Var[i]]] %>% car::vif() }
		emtr <- mdl[[Var[i]]] %>% emmeans::emtrends(~Grp,"Age") %>% summary(infer=c(T,T))
		
		Tb$Var              [i] <- Var[i]
		Tb$Excl             [i] <- excl
		Tb$F_AgeInt         [i] <- anv["Grp:Age","F value"]
		Tb$df1_AgeInt       [i] <- anv["Grp:Age","NumDF"]
		Tb$df2_AgeInt       [i] <- anv["Grp:Age","DenDF"]
		Tb$p_AgeInt         [i] <- anv["Grp:Age","Pr(>F)"]
		Tb$eta2_AgeInt      [i] <- if (doRob) { NA } else { eta2 %>% dplyr::filter(Parameter=="Grp:Age") %>% {.$Eta2_partial} }
		Tb$spR2_AgeInt      [i] <- if (doRob) { NA } else { spR2 %>% dplyr::filter(Effect=="Grp:Age") %>% {.$Rsq} }
		Tb$gvif_AgeInt      [i] <- if (doRob) { NA } else { gvif["Grp:Age","GVIF"] }
		Tb$emtr_Age_HC      [i] <- emtr %>% dplyr::filter(Grp=="HC") %>% {.$Age.trend}
		Tb$emtr_Age_HC_LCI  [i] <- emtr %>% dplyr::filter(Grp=="HC") %>% {.$lower.CL}
		Tb$emtr_Age_HC_UCI  [i] <- emtr %>% dplyr::filter(Grp=="HC") %>% {.$upper.CL}
		Tb$emtr_Age_OUD     [i] <- emtr %>% dplyr::filter(Grp=="OUD") %>% {.$Age.trend}
		Tb$emtr_Age_OUD_LCI [i] <- emtr %>% dplyr::filter(Grp=="OUD") %>% {.$lower.CL}
		Tb$emtr_Age_OUD_UCI [i] <- emtr %>% dplyr::filter(Grp=="OUD") %>% {.$upper.CL}
		
	}
	
	Tb$pFDR_AgeInt <- func_FdrBy(p = Tb$p_AgeInt, pname = Tb$Var, pgrp = VarClust)
	Tb$pprint_AgeInt <- shiRFormatPval(Tb$p_AgeInt)
	Tb$pFDRprint_AgeInt <- shiRFormatPval(Tb$pFDR_AgeInt)
	Tb %<>% dplyr::mutate(STRING=gsub(', pFDRNA','',gsub('-','–',sprintf(
		"F(%d,%.2f)=%.2f, p%s, pFDR%s, partial η²=%.2f, R²=%.2f%%; %.2f [%.2f,%.2f] ×10⁻³; %.2f [%.2f,%.2f] ×10⁻³",
		df1_AgeInt, df2_AgeInt, F_AgeInt, pprint_AgeInt, pFDRprint_AgeInt, eta2_AgeInt, spR2_AgeInt*100, emtr_Age_HC*1000, emtr_Age_HC_LCI*1000, emtr_Age_HC_UCI*1000, emtr_Age_OUD*1000, emtr_Age_OUD_LCI*1000, emtr_Age_OUD_UCI*1000
	))))
	
	return(list(
		mdl = mdl,
		Tb = Tb
	))
	
}

#### Grp x Sex
func_Stat_mdl.GrpSexInt = function(dat, Var, VarClust, C, RFX = "(1|Study)", OutlierCtr = NULL, Weight = NULL) {
	
	doRob <- identical(OutlierCtr %>% tolower() %>% substr(1,3), "rob")
	mdl <- list()
	Tb = data.frame(
		Var              = length(Var) %>% character(),
		Excl             = length(Var) %>% numeric(),
		F_SexInt         = length(Var) %>% numeric(),
		df1_SexInt       = length(Var) %>% numeric(),
		df2_SexInt       = length(Var) %>% numeric(),
		p_SexInt         = length(Var) %>% numeric(),
		pprint_SexInt    = length(Var) %>% character(),
		pFDR_SexInt      = length(Var) %>% numeric(),
		pFDRprint_SexInt = length(Var) %>% character(),
		eta2_SexInt      = length(Var) %>% numeric(),
		spR2_SexInt      = length(Var) %>% numeric(),
		gvif_SexInt      = length(Var) %>% numeric(),
		emm_HC_M         = length(Var) %>% numeric(),
		emm_HC_M_LCI     = length(Var) %>% numeric(),
		emm_HC_M_HCI     = length(Var) %>% numeric(),
		emm_HC_F         = length(Var) %>% numeric(),
		emm_HC_F_LCI     = length(Var) %>% numeric(),
		emm_HC_F_HCI     = length(Var) %>% numeric(),
		emm_HC_MvF       = length(Var) %>% numeric(),
		emm_HC_MvF_LCI   = length(Var) %>% numeric(),
		emm_HC_MvF_HCI   = length(Var) %>% numeric(),
		emm_OUD_M        = length(Var) %>% numeric(),
		emm_OUD_M_LCI    = length(Var) %>% numeric(),
		emm_OUD_M_HCI    = length(Var) %>% numeric(),
		emm_OUD_F        = length(Var) %>% numeric(),
		emm_OUD_F_LCI    = length(Var) %>% numeric(),
		emm_OUD_F_HCI    = length(Var) %>% numeric(),
		emm_OUD_MvF      = length(Var) %>% numeric(),
		emm_OUD_MvF_LCI  = length(Var) %>% numeric(),
		emm_OUD_MvF_HCI  = length(Var) %>% numeric(),
		STRING = length(Var) %>% character()
	)
	
	for (i in 1:length(Var)) {
		
		paste0(" ", as.character(match.call()[[1]]), " --- ", Var[i], " \n") %>% crayon::black() %>% crayon::bgYellow() %>% cat()
		
		mdl <- func_Stat_Model(
			shiRFormula(Var[i],c("Grp*Sex",C,RFX)),
			Dat = dat,
			OutlierCtr = OutlierCtr,
			Weight = Weight
		)
		
		mdl[[Var[i]]] <- mdl$mdl
		excl <- mdl$excl
		
		anv <- mdl[[Var[i]]] %>% anova(ddf=shiRIf(doRob,"satterthwaite","Satterthwaite"))
		if (!doRob) { eta2 <- anv %>% effectsize::eta_squared(partial=T) }
		if (!doRob) { spR2 <- mdl[[Var[i]]] %>% r2glmm::r2beta(method="nsj") }
		if (!doRob) { gvif <- mdl[[Var[i]]] %>% car::vif() }
		emm <- mdl[[Var[i]]] %>% emmeans::emmeans(c("Grp","Sex"))
		ctr <- emm %>% emmeans::contrast(list(HC=c(-1,0,1,0),OUD=c(0,-1,0,1))) %>% summary(infer=c(T,T))
		emm <- emm %>% summary(infer=c(T,T))
		
		Tb$Var             [i] <- Var[i]
		Tb$Excl            [i] <- excl
		Tb$F_SexInt        [i] <- anv["Grp:Sex","F value"]
		Tb$df1_SexInt      [i] <- anv["Grp:Sex","NumDF"]
		Tb$df2_SexInt      [i] <- anv["Grp:Sex","DenDF"]
		Tb$p_SexInt        [i] <- anv["Grp:Sex","Pr(>F)"]
		Tb$eta2_SexInt     [i] <- if (doRob) { NA } else { eta2 %>% dplyr::filter(Parameter=="Grp:Sex") %>% {.$Eta2_partial} }
		Tb$spR2_SexInt     [i] <- if (doRob) { NA } else { spR2 %>% dplyr::filter(Effect=="Grp:Sex") %>% {.$Rsq} }
		Tb$gvif_SexInt     [i] <- if (doRob) { NA } else { gvif["Grp:Sex","GVIF"] }
		Tb$emm_HC_M        [i] <- emm %>% dplyr::filter(Grp=="HC" & Sex=="Male"  ) %>% {.$emmean}
		Tb$emm_HC_M_LCI    [i] <- emm %>% dplyr::filter(Grp=="HC" & Sex=="Male"  ) %>% {.$lower.CL}
		Tb$emm_HC_M_HCI    [i] <- emm %>% dplyr::filter(Grp=="HC" & Sex=="Male"  ) %>% {.$upper.CL}
		Tb$emm_HC_F        [i] <- emm %>% dplyr::filter(Grp=="HC" & Sex=="Female") %>% {.$emmean}
		Tb$emm_HC_F_LCI    [i] <- emm %>% dplyr::filter(Grp=="HC" & Sex=="Female") %>% {.$lower.CL}
		Tb$emm_HC_F_HCI    [i] <- emm %>% dplyr::filter(Grp=="HC" & Sex=="Female") %>% {.$upper.CL}
		Tb$emm_HC_MvF      [i] <- ctr %>% dplyr::filter(contrast=="HC") %>% {.$estimate}
		Tb$emm_HC_MvF_LCI  [i] <- ctr %>% dplyr::filter(contrast=="HC") %>% {.$lower.CL}
		Tb$emm_HC_MvF_HCI  [i] <- ctr %>% dplyr::filter(contrast=="HC") %>% {.$upper.CL}
		Tb$emm_OUD_M       [i] <- emm %>% dplyr::filter(Grp=="OUD" & Sex=="Male"  ) %>% {.$emmean}
		Tb$emm_OUD_M_LCI   [i] <- emm %>% dplyr::filter(Grp=="OUD" & Sex=="Male"  ) %>% {.$lower.CL}
		Tb$emm_OUD_M_HCI   [i] <- emm %>% dplyr::filter(Grp=="OUD" & Sex=="Male"  ) %>% {.$upper.CL}
		Tb$emm_OUD_F       [i] <- emm %>% dplyr::filter(Grp=="OUD" & Sex=="Female") %>% {.$emmean}
		Tb$emm_OUD_F_LCI   [i] <- emm %>% dplyr::filter(Grp=="OUD" & Sex=="Female") %>% {.$lower.CL}
		Tb$emm_OUD_F_HCI   [i] <- emm %>% dplyr::filter(Grp=="OUD" & Sex=="Female") %>% {.$upper.CL}
		Tb$emm_OUD_MvF     [i] <- ctr %>% dplyr::filter(contrast=="OUD") %>% {.$estimate}
		Tb$emm_OUD_MvF_LCI [i] <- ctr %>% dplyr::filter(contrast=="OUD") %>% {.$lower.CL}
		Tb$emm_OUD_MvF_HCI [i] <- ctr %>% dplyr::filter(contrast=="OUD") %>% {.$upper.CL}
		
	}
	Tb$pFDR_SexInt <- func_FdrBy(p = Tb$p_SexInt, pname = Tb$Var, pgrp = VarClust)
	Tb$pprint_SexInt <- shiRFormatPval(Tb$p_SexInt)
	Tb$pFDRprint_SexInt <- shiRFormatPval(Tb$pFDR_SexInt)
	Tb %<>% dplyr::mutate(STRING=gsub(', pFDRNA','',gsub('-','–',sprintf(
		"F(%d,%.2f)=%.2f, p%s, pFDR%s, partial η²=%.2f, R²=%.2f%%; %.2f [%.2f,%.2f]; %.2f [%.2f,%.2f]; %.2f [%.2f,%.2f]; %.2f [%.2f,%.2f]; %.2f [%.2f,%.2f]; %.2f [%.2f,%.2f]",
		df1_SexInt, df2_SexInt, F_SexInt, pprint_SexInt, pFDRprint_SexInt, eta2_SexInt, spR2_SexInt*100, emm_HC_M, emm_HC_M_LCI, emm_HC_M_HCI, emm_HC_F, emm_HC_F_LCI, emm_HC_F_HCI, emm_HC_MvF, emm_HC_MvF_LCI, emm_HC_MvF_HCI, emm_OUD_M, emm_OUD_M_LCI, emm_OUD_M_HCI, emm_OUD_F, emm_OUD_F_LCI, emm_OUD_F_HCI, emm_OUD_MvF, emm_OUD_MvF_LCI, emm_OUD_MvF_HCI
	))))
	
	return(list(
		mdl = mdl,
		Tb = Tb
	))
	
}

#### Grp
func_Stat_mdl.Grp = function(dat, Var, VarClust, C, RFX = "(1|Study)", OutlierCtr = NULL, Weight = NULL) {
	
	doRob <- identical(OutlierCtr %>% tolower() %>% substr(1,3), "rob")
	mdl <- list()
	Tb = data.frame(
		Var           = length(Var) %>% character(),
		Excl          = length(Var) %>% numeric(),
		F_Grp         = length(Var) %>% numeric(),
		df1_Grp       = length(Var) %>% numeric(),
		df2_Grp       = length(Var) %>% numeric(),
		p_Grp         = length(Var) %>% numeric(),
		pprint_Grp    = length(Var) %>% character(),
		pFDR_Grp      = length(Var) %>% numeric(),
		pFDRprint_Grp = length(Var) %>% character(),
		eta2_Grp      = length(Var) %>% numeric(),
		spR2_Grp      = length(Var) %>% numeric(),
		gvif_Grp      = length(Var) %>% numeric(),
		F_Age         = length(Var) %>% numeric(),
		df1_Age       = length(Var) %>% numeric(),
		df2_Age       = length(Var) %>% numeric(),
		p_Age         = length(Var) %>% numeric(),
		pprint_Age    = length(Var) %>% character(),
		pFDR_Age      = length(Var) %>% numeric(),
		pFDRprint_Age = length(Var) %>% character(),
		eta2_Age      = length(Var) %>% numeric(),
		spR2_Age      = length(Var) %>% numeric(),
		gvif_Age      = length(Var) %>% numeric(),
		F_Sex         = length(Var) %>% numeric(),
		df1_Sex       = length(Var) %>% numeric(),
		df2_Sex       = length(Var) %>% numeric(),
		p_Sex         = length(Var) %>% numeric(),
		pprint_Sex    = length(Var) %>% character(),
		pFDR_Sex      = length(Var) %>% numeric(),
		pFDRprint_Sex = length(Var) %>% character(),
		eta2_Sex      = length(Var) %>% numeric(),
		spR2_Sex      = length(Var) %>% numeric(),
		gvif_Sex      = length(Var) %>% numeric(),
		emm_HC        = length(Var) %>% numeric(),
		emm_HC_LCI    = length(Var) %>% numeric(),
		emm_HC_HCI    = length(Var) %>% numeric(),
		emm_OUD       = length(Var) %>% numeric(),
		emm_OUD_LCI   = length(Var) %>% numeric(),
		emm_OUD_HCI   = length(Var) %>% numeric(),
		slp_Age       = length(Var) %>% numeric(),
		slp_Age_LCI   = length(Var) %>% numeric(),
		slp_Age_UCI   = length(Var) %>% numeric(),
		emm_M         = length(Var) %>% numeric(),
		emm_M_LCI     = length(Var) %>% numeric(),
		emm_M_HCI     = length(Var) %>% numeric(),
		emm_F         = length(Var) %>% numeric(),
		emm_F_LCI     = length(Var) %>% numeric(),
		emm_F_HCI     = length(Var) %>% numeric(),
		STRING = length(Var) %>% character()
	)
	
	for (i in 1:length(Var)) {
		
		paste0(" ", as.character(match.call()[[1]]), " --- ", Var[i], " \n") %>% crayon::black() %>% crayon::bgYellow() %>% cat()
		
		mdl <- func_Stat_Model(
			shiRFormula(Var[i],c("Grp",C,RFX)),
			Dat = dat,
			OutlierCtr = OutlierCtr,
			Weight = Weight
		)
		
		mdl[[Var[i]]] <- mdl$mdl
		excl <- mdl$excl
		
		anv <- mdl[[Var[i]]] %>% anova(ddf=shiRIf(doRob,"satterthwaite","Satterthwaite"))
		if (!doRob) { eta2 <- anv %>% effectsize::eta_squared(partial=T) }
		if (!doRob) { spR2 <- mdl[[Var[i]]] %>% r2glmm::r2beta(method="nsj") }
		if (!doRob) { gvif <- mdl[[Var[i]]] %>% car::vif() }
		emm <- mdl[[Var[i]]] %>% emmeans::emmeans(~Grp) %>% summary(infer=c(T,T))
		slp <- mdl[[Var[i]]] %>% {c(fixef(.)["Age"],confint(.,parm="Age"))}
		emm2 <- mdl[[Var[i]]] %>% emmeans::emmeans(~Sex) %>% summary(infer=c(T,T))
		
		Tb$Var         [i] <- Var[i]
		Tb$Excl        [i] <- excl
		Tb$F_Grp       [i] <- anv["Grp","F value"]
		Tb$df1_Grp     [i] <- anv["Grp","NumDF"]
		Tb$df2_Grp     [i] <- anv["Grp","DenDF"]
		Tb$p_Grp       [i] <- anv["Grp","Pr(>F)"]
		Tb$eta2_Grp    [i] <- if (doRob) { NA } else { eta2 %>% dplyr::filter(Parameter=="Grp") %>% {.$Eta2_partial} }
		Tb$spR2_Grp    [i] <- if (doRob) { NA } else { spR2 %>% dplyr::filter(Effect=="Grp") %>% {.$Rsq} }
		Tb$gvif_Grp    [i] <- if (doRob) { NA } else { gvif["Grp","GVIF"] }
		Tb$F_Age       [i] <- anv["Age","F value"]
		Tb$df1_Age     [i] <- anv["Age","NumDF"]
		Tb$df2_Age     [i] <- anv["Age","DenDF"]
		Tb$p_Age       [i] <- anv["Age","Pr(>F)"]
		Tb$eta2_Age    [i] <- if (doRob) { NA } else { eta2 %>% dplyr::filter(Parameter=="Age") %>% {.$Eta2_partial} }
		Tb$spR2_Age    [i] <- if (doRob) { NA } else { spR2 %>% dplyr::filter(Effect=="Age") %>% {.$Rsq} }
		Tb$gvif_Age    [i] <- if (doRob) { NA } else { gvif["Age","GVIF"] }
		Tb$F_Sex       [i] <- anv["Sex","F value"]
		Tb$df1_Sex     [i] <- anv["Sex","NumDF"]
		Tb$df2_Sex     [i] <- anv["Sex","DenDF"]
		Tb$p_Sex       [i] <- anv["Sex","Pr(>F)"]
		Tb$eta2_Sex    [i] <- if (doRob) { NA } else { eta2 %>% dplyr::filter(Parameter=="Sex") %>% {.$Eta2_partial} }
		Tb$spR2_Sex    [i] <- if (doRob) { NA } else { spR2 %>% dplyr::filter(Effect=="Sex") %>% {.$Rsq} }
		Tb$gvif_Sex    [i] <- if (doRob) { NA } else { gvif["Sex","GVIF"] }
		Tb$emm_HC      [i] <- emm %>% dplyr::filter(Grp=="HC") %>% {.$emmean}
		Tb$emm_HC_LCI  [i] <- emm %>% dplyr::filter(Grp=="HC") %>% {.$lower.CL}
		Tb$emm_HC_HCI  [i] <- emm %>% dplyr::filter(Grp=="HC") %>% {.$upper.CL}
		Tb$emm_OUD     [i] <- emm %>% dplyr::filter(Grp=="OUD") %>% {.$emmean}
		Tb$emm_OUD_LCI [i] <- emm %>% dplyr::filter(Grp=="OUD") %>% {.$lower.CL}
		Tb$emm_OUD_HCI [i] <- emm %>% dplyr::filter(Grp=="OUD") %>% {.$upper.CL}
		Tb$slp_Age     [i] <- slp[1]
		Tb$slp_Age_LCI [i] <- slp[2]
		Tb$slp_Age_UCI [i] <- slp[3]
		Tb$emm_M       [i] <- emm2 %>% dplyr::filter(Sex=="Male") %>% {.$emmean}
		Tb$emm_M_LCI   [i] <- emm2 %>% dplyr::filter(Sex=="Male") %>% {.$lower.CL}
		Tb$emm_M_HCI   [i] <- emm2 %>% dplyr::filter(Sex=="Male") %>% {.$upper.CL}
		Tb$emm_F       [i] <- emm2 %>% dplyr::filter(Sex=="Female") %>% {.$emmean}
		Tb$emm_F_LCI   [i] <- emm2 %>% dplyr::filter(Sex=="Female") %>% {.$lower.CL}
		Tb$emm_F_HCI   [i] <- emm2 %>% dplyr::filter(Sex=="Female") %>% {.$upper.CL}
		
	}
	Tb$pFDR_Grp <- func_FdrBy(p = Tb$p_Grp, pname = Tb$Var, pgrp = VarClust)
	Tb$pprint_Grp <- shiRFormatPval(Tb$p_Grp)
	Tb$pFDRprint_Grp <- shiRFormatPval(Tb$pFDR_Grp)
	Tb$pFDR_Age <- func_FdrBy(p = Tb$p_Age, pname = Tb$Var, pgrp = VarClust)
	Tb$pprint_Age <- shiRFormatPval(Tb$p_Age)
	Tb$pFDRprint_Age <- shiRFormatPval(Tb$pFDR_Age)
	Tb$pFDR_Sex <- func_FdrBy(p = Tb$p_Sex, pname = Tb$Var, pgrp = VarClust)
	Tb$pprint_Sex <- shiRFormatPval(Tb$p_Sex)
	Tb$pFDRprint_Sex <- shiRFormatPval(Tb$pFDR_Sex)
	Tb %<>% dplyr::mutate(STRING=gsub(', pFDRNA','',gsub('-','–',sprintf(
		"F(%d,%.2f)=%.2f, p%s, pFDR%s, partial η²=%.2f, R²=%.2f%%; %.2f [%.2f,%.2f]; %.2f [%.2f,%.2f]; F(%d,%.2f)=%.2f, p%s, pFDR%s, partial η²=%.2f, R²=%.2f%%; %.2f [%.2f,%.2f] ×10⁻³; F(%d,%.2f)=%.2f, p%s, pFDR%s, partial η²=%.2f, R²=%.2f%%; %.2f [%.2f,%.2f]; %.2f [%.2f,%.2f]",
		df1_Grp, df2_Grp, F_Grp, pprint_Grp, pFDRprint_Grp, eta2_Grp, spR2_Grp*100, emm_HC, emm_HC_LCI, emm_HC_HCI, emm_OUD, emm_OUD_LCI, emm_OUD_HCI, df1_Age, df2_Age, F_Age, pprint_Age, pFDRprint_Age, eta2_Age, spR2_Age*100, slp_Age*1000, slp_Age_LCI*1000, slp_Age_UCI*1000, df1_Sex, df2_Sex, F_Sex, pprint_Sex, pFDRprint_Sex, eta2_Sex, spR2_Sex*100, emm_M, emm_M_LCI, emm_M_HCI, emm_F, emm_F_LCI, emm_F_HCI
	))))
	
	return(list(
		mdl = mdl,
		Tb = Tb
	))
	
}

#### HxOD
func_Stat_mdl.HxOD = function(dat, Var, VarClust, C, RFX = "(1|Study)", OutlierCtr = NULL, Weight = NULL) {
	
	doRob <- identical(OutlierCtr %>% tolower() %>% substr(1,3), "rob")
	mdl <- list()
	Tb = data.frame(
		Var            = length(Var) %>% character(),
		Excl           = length(Var) %>% numeric(),
		F_HxOD         = length(Var) %>% numeric(),
		df1_HxOD       = length(Var) %>% numeric(),
		df2_HxOD       = length(Var) %>% numeric(),
		p_HxOD         = length(Var) %>% numeric(),
		pprint_HxOD    = length(Var) %>% character(),
		pFDR_HxOD      = length(Var) %>% numeric(),
		pFDRprint_HxOD = length(Var) %>% character(),
		eta2_HxOD      = length(Var) %>% numeric(),
		spR2_HxOD      = length(Var) %>% numeric(),
		gvif_HxOD      = length(Var) %>% numeric(),
		emm_nOD        = length(Var) %>% numeric(),
		emm_nOD_LCI    = length(Var) %>% numeric(),
		emm_nOD_HCI    = length(Var) %>% numeric(),
		emm_OD         = length(Var) %>% numeric(),
		emm_OD_LCI     = length(Var) %>% numeric(),
		emm_OD_HCI     = length(Var) %>% numeric(),
		STRING = length(Var) %>% character()
	)
	
	dat_od <- dat %>% dplyr::filter(HxOD==0 | HxOD==1) %>% dplyr::filter(Grp=="OUD")
	dat_od$HxOD %<>% droplevels()
	
	for (i in 1:length(Var)) {
		
		paste0(" ", as.character(match.call()[[1]]), " --- ", Var[i], " \n") %>% crayon::black() %>% crayon::bgYellow() %>% cat()
		
		mdl <- func_Stat_Model(
			shiRFormula(Var[i],c("HxOD",C,RFX)),
			Dat = dat_od,
			OutlierCtr = OutlierCtr,
			Weight = Weight
		)
		mdl[[Var[i]]] <- mdl$mdl
		excl <- mdl$excl
		
		anv <- mdl[[Var[i]]] %>% anova(ddf=shiRIf(doRob,"satterthwaite","Satterthwaite"))
		if (!doRob) { eta2 <- anv %>% effectsize::eta_squared(partial=T) }
		if (!doRob) { spR2 <- mdl[[Var[i]]] %>% r2glmm::r2beta(method="nsj") }
		if (!doRob) { gvif <- mdl[[Var[i]]] %>% car::vif() }
		emm <- mdl[[Var[i]]] %>% emmeans::emmeans(~HxOD) %>% summary(infer=c(T,T))
		
		Tb$Var         [i] <- Var[i]
		Tb$Excl        [i] <- excl
		Tb$F_HxOD      [i] <- anv["HxOD","F value"]
		Tb$df1_HxOD    [i] <- anv["HxOD","NumDF"]
		Tb$df2_HxOD    [i] <- anv["HxOD","DenDF"]
		Tb$p_HxOD      [i] <- anv["HxOD","Pr(>F)"]
		Tb$eta2_HxOD   [i] <- if (doRob) { NA } else { eta2 %>% dplyr::filter(Parameter=="HxOD") %>% {.$Eta2_partial} }
		Tb$spR2_HxOD   [i] <- if (doRob) { NA } else { spR2 %>% dplyr::filter(Effect=="HxOD") %>% {.$Rsq} }
		Tb$gvif_HxOD   [i] <- if (doRob) { NA } else { gvif["HxOD","GVIF"] }
		Tb$emm_nOD     [i] <- emm %>% dplyr::filter(HxOD==0) %>% {.$emmean}
		Tb$emm_nOD_LCI [i] <- emm %>% dplyr::filter(HxOD==0) %>% {.$lower.CL}
		Tb$emm_nOD_HCI [i] <- emm %>% dplyr::filter(HxOD==0) %>% {.$upper.CL}
		Tb$emm_OD      [i] <- emm %>% dplyr::filter(HxOD==1) %>% {.$emmean}
		Tb$emm_OD_LCI  [i] <- emm %>% dplyr::filter(HxOD==1) %>% {.$lower.CL}
		Tb$emm_OD_HCI  [i] <- emm %>% dplyr::filter(HxOD==1) %>% {.$upper.CL}
		
	}
	Tb$pFDR_HxOD <- func_FdrBy(p = Tb$p_HxOD, pname = Tb$Var, pgrp = VarClust)
	Tb$pprint_HxOD <- shiRFormatPval(Tb$p_HxOD)
	Tb$pFDRprint_HxOD <- shiRFormatPval(Tb$pFDR_HxOD)
	Tb %<>% dplyr::mutate(STRING=gsub(', pFDRNA','',gsub('-','–',sprintf(
		"F(%d,%.2f)=%.2f, p%s, pFDR%s, partial η²=%.2f, R²=%.2f%%; %.2f [%.2f,%.2f]; %.2f [%.2f,%.2f]",
		df1_HxOD, df2_HxOD, F_HxOD, pprint_HxOD, pFDRprint_HxOD, eta2_HxOD, spR2_HxOD*100, emm_nOD, emm_nOD_LCI, emm_nOD_HCI, emm_OD, emm_OD_LCI, emm_OD_HCI
	))))
	
	return(list(
		mdl = mdl,
		Tb = Tb
	))
	
}

#### Med
func_Stat_mdl.Med = function(dat, Var, VarClust, C, RFX = "(1|Study)", OutlierCtr = NULL, Weight = NULL) {
	
	doRob <- identical(OutlierCtr %>% tolower() %>% substr(1,3), "rob")
	mdl <- list()
	Tb = data.frame(
		Var           = length(Var) %>% character(),
		Excl          = length(Var) %>% numeric(),
		F_Med         = length(Var) %>% numeric(),
		df1_Med       = length(Var) %>% numeric(),
		df2_Med       = length(Var) %>% numeric(),
		p_Med         = length(Var) %>% numeric(),
		pprint_Med    = length(Var) %>% character(),
		pFDR_Med      = length(Var) %>% numeric(),
		pFDRprint_Med = length(Var) %>% character(),
		eta2_Med      = length(Var) %>% numeric(),
		spR2_Med      = length(Var) %>% numeric(),
		gvif_Med      = length(Var) %>% numeric(),
		emm_BUP       = length(Var) %>% numeric(),
		emm_BUP_LCI   = length(Var) %>% numeric(),
		emm_BUP_HCI   = length(Var) %>% numeric(),
		emm_MTD       = length(Var) %>% numeric(),
		emm_MTD_LCI   = length(Var) %>% numeric(),
		emm_MTD_HCI   = length(Var) %>% numeric(),
		STRING = length(Var) %>% character()
	)
	
	dat_med <- dat %>% dplyr::filter(Medication=="Methadone" | Medication=="Buprenorphine") %>% dplyr::filter(Grp=="OUD")
	dat_med$Medication %<>% droplevels()
	
	for (i in 1:length(Var)) {
		
		paste0(" ", as.character(match.call()[[1]]), " --- ", Var[i], " \n") %>% crayon::black() %>% crayon::bgYellow() %>% cat()
		
		mdl <- func_Stat_Model(
			shiRFormula(Var[i],c("Medication",C,RFX)),
			Dat = dat_med,
			OutlierCtr = OutlierCtr,
			Weight = Weight
		)
		mdl[[Var[i]]] <- mdl$mdl
		excl <- mdl$excl
		
		anv <- mdl[[Var[i]]] %>% anova(ddf=shiRIf(doRob,"satterthwaite","Satterthwaite"))
		if (!doRob) { eta2 <- anv %>% effectsize::eta_squared(partial=T) }
		if (!doRob) { spR2 <- mdl[[Var[i]]] %>% r2glmm::r2beta(method="nsj") }
		if (!doRob) { gvif <- mdl[[Var[i]]] %>% car::vif() }
		emm <- mdl[[Var[i]]] %>% emmeans::emmeans(~Medication) %>% summary(infer=c(T,T))
		
		Tb$Var         [i] <- Var[i]
		Tb$Excl        [i] <- excl
		Tb$F_Med       [i] <- anv["Medication","F value"]
		Tb$df1_Med     [i] <- anv["Medication","NumDF"]
		Tb$df2_Med     [i] <- anv["Medication","DenDF"]
		Tb$p_Med       [i] <- anv["Medication","Pr(>F)"]
		Tb$eta2_Med    [i] <- if (doRob) { NA } else { eta2 %>% dplyr::filter(Parameter=="Medication") %>% {.$Eta2_partial} }
		Tb$spR2_Med    [i] <- if (doRob) { NA } else { spR2 %>% dplyr::filter(Effect=="Medication") %>% {.$Rsq} }
		Tb$gvif_Med    [i] <- if (doRob) { NA } else { gvif["Medication","GVIF"] }
		Tb$emm_BUP     [i] <- emm %>% dplyr::filter(Medication=="Buprenorphine") %>% {.$emmean}
		Tb$emm_BUP_LCI [i] <- emm %>% dplyr::filter(Medication=="Buprenorphine") %>% {.$lower.CL}
		Tb$emm_BUP_HCI [i] <- emm %>% dplyr::filter(Medication=="Buprenorphine") %>% {.$upper.CL}
		Tb$emm_MTD     [i] <- emm %>% dplyr::filter(Medication=="Methadone") %>% {.$emmean}
		Tb$emm_MTD_LCI [i] <- emm %>% dplyr::filter(Medication=="Methadone") %>% {.$lower.CL}
		Tb$emm_MTD_HCI [i] <- emm %>% dplyr::filter(Medication=="Methadone") %>% {.$upper.CL}
		
	}
	Tb$pFDR_Med <- func_FdrBy(p = Tb$p_Med, pname = Tb$Var, pgrp = VarClust)
	Tb$pprint_Med <- shiRFormatPval(Tb$p_Med)
	Tb$pFDRprint_Med <- shiRFormatPval(Tb$pFDR_Med)
	Tb %<>% dplyr::mutate(STRING=gsub(', pFDRNA','',gsub('-','–',sprintf(
		"F(%d,%.2f)=%.2f, p%s, pFDR%s, partial η²=%.2f, R²=%.2f%%; %.2f [%.2f,%.2f]; %.2f [%.2f,%.2f]",
		df1_Med, df2_Med, F_Med, pprint_Med, pFDRprint_Med, eta2_Med, spR2_Med*100, emm_BUP, emm_BUP_LCI, emm_BUP_HCI, emm_MTD, emm_MTD_LCI, emm_MTD_HCI
	))))
	
	return(list(
		mdl = mdl,
		Tb = Tb
	))
	
}

#### Yrs
func_Stat_mdl.Yrs = function(dat, Var, VarClust, C, RFX = "(1|Study)", OutlierCtr = NULL, Weight = NULL) {
	
	doRob <- identical(OutlierCtr %>% tolower() %>% substr(1,3), "rob")
	mdl <- list()
	Tb = data.frame(
		Var           = length(Var) %>% character(),
		Excl          = length(Var) %>% numeric(),
		F_Yrs         = length(Var) %>% numeric(),
		df1_Yrs       = length(Var) %>% numeric(),
		df2_Yrs       = length(Var) %>% numeric(),
		p_Yrs         = length(Var) %>% numeric(),
		pprint_Yrs    = length(Var) %>% character(),
		pFDR_Yrs      = length(Var) %>% numeric(),
		pFDRprint_Yrs = length(Var) %>% character(),
		eta2_Yrs      = length(Var) %>% numeric(),
		spR2_Yrs      = length(Var) %>% numeric(),
		gvif_Yrs      = length(Var) %>% numeric(),
		slp_Yrs       = length(Var) %>% numeric(),
		slp_Yrs_LCI   = length(Var) %>% numeric(),
		slp_Yrs_UCI   = length(Var) %>% numeric(),
		STRING = length(Var) %>% character()
	)
	
	dat_yrs <- dat %>% dplyr::filter(!is.na(YrsOpUse) & Grp=="OUD")
	
	for (i in 1:length(Var)) {
		
		paste0(" ", as.character(match.call()[[1]]), " --- ", Var[i], " \n") %>% crayon::black() %>% crayon::bgYellow() %>% cat()
		
		mdl <- func_Stat_Model(
			shiRFormula(Var[i],c("YrsOpUse",C,RFX)),
			Dat = dat_yrs,
			OutlierCtr = OutlierCtr,
			Weight = Weight
		)
		mdl[[Var[i]]] <- mdl$mdl
		excl <- mdl$excl
		
		anv <- mdl[[Var[i]]] %>% anova(ddf=shiRIf(doRob,"satterthwaite","Satterthwaite"))
		if (!doRob) { eta2 <- anv %>% effectsize::eta_squared(partial=T) }
		if (!doRob) { spR2 <- mdl[[Var[i]]] %>% r2glmm::r2beta(method="nsj") }
		if (!doRob) { gvif <- mdl[[Var[i]]] %>% car::vif() }
		slp <- mdl[[Var[i]]] %>% {c(fixef(.)["YrsOpUse"],confint(.,parm="YrsOpUse"))}
		
		Tb$Var         [i] <- Var[i]
		Tb$Excl        [i] <- excl
		Tb$F_Yrs       [i] <- anv["YrsOpUse","F value"]
		Tb$df1_Yrs     [i] <- anv["YrsOpUse","NumDF"]
		Tb$df2_Yrs     [i] <- anv["YrsOpUse","DenDF"]
		Tb$p_Yrs       [i] <- anv["YrsOpUse","Pr(>F)"]
		Tb$eta2_Yrs    [i] <- if (doRob) { NA } else { eta2 %>% dplyr::filter(Parameter=="YrsOpUse") %>% {.$Eta2_partial} }
		Tb$spR2_Yrs    [i] <- if (doRob) { NA } else { spR2 %>% dplyr::filter(Effect=="YrsOpUse") %>% {.$Rsq} }
		Tb$gvif_Yrs    [i] <- if (doRob) { NA } else { gvif["YrsOpUse","GVIF"] }
		Tb$slp_Yrs     [i] <- slp[1]
		Tb$slp_Yrs_LCI [i] <- slp[2]
		Tb$slp_Yrs_UCI [i] <- slp[3]
		
	}
	Tb$pFDR_Yrs <- func_FdrBy(p = Tb$p_Yrs, pname = Tb$Var, pgrp = VarClust)
	Tb$pprint_Yrs <- shiRFormatPval(Tb$p_Yrs)
	Tb$pFDRprint_Yrs <- shiRFormatPval(Tb$pFDR_Yrs)
	Tb %<>% dplyr::mutate(STRING=gsub(', pFDRNA','',gsub('-','–',sprintf(
		"F(%d,%.2f)=%.2f, p%s, pFDR%s, partial η²=%.2f, R²=%.2f%%; %.2f [%.2f,%.2f] ×10⁻³",
		df1_Yrs, df2_Yrs, F_Yrs, pprint_Yrs, pFDRprint_Yrs, eta2_Yrs, spR2_Yrs*100, slp_Yrs*1000, slp_Yrs_LCI*1000, slp_Yrs_UCI*1000
	))))
	
	return(list(
		mdl = mdl,
		Tb = Tb
	))
	
}

#### Days
func_Stat_mdl.Days = function(dat, Var, VarClust, C, OutlierCtr = NULL, Weight = NULL) {
	
	doRob <- identical(OutlierCtr %>% tolower() %>% substr(1,3), "rob")
	mdl <- list()
	Tb = data.frame(
		Var            = length(Var) %>% character(),
		Excl           = length(Var) %>% numeric(),
		F_Days         = length(Var) %>% numeric(),
		df1_Days       = length(Var) %>% numeric(),
		df2_Days       = length(Var) %>% numeric(),
		p_Days         = length(Var) %>% numeric(),
		pprint_Days    = length(Var) %>% character(),
		pFDR_Days      = length(Var) %>% numeric(),
		pFDRprint_Days = length(Var) %>% character(),
		eta2_Days      = length(Var) %>% numeric(),
		spR2_Days      = length(Var) %>% numeric(),
		gvif_Days      = length(Var) %>% numeric(),
		slp_Days       = length(Var) %>% numeric(),
		slp_Days_LCI   = length(Var) %>% numeric(),
		slp_Days_UCI   = length(Var) %>% numeric(),
		STRING = length(Var) %>% character()
	)
	
	dat_days <- dat %>% dplyr::filter(!is.na(DaysAbstin) & Grp=="OUD")
	
	for (i in 1:length(Var)) {
		
		paste0(" ", as.character(match.call()[[1]]), " --- ", Var[i], " \n") %>% crayon::black() %>% crayon::bgYellow() %>% cat()
		
		mdl <- func_Stat_Model(
			shiRFormula(Var[i],setdiff(c("DaysAbstin",C),"RestDur")),
			Dat = dat_days,
			OutlierCtr = OutlierCtr,
			Weight = Weight
		)
		mdl[[Var[i]]] <- mdl$mdl
		excl <- mdl$excl
		
		anv <- mdl[[Var[i]]] %>% Anova(type=3)
		if (!doRob) { eta2 <- anv %>% effectsize::eta_squared(partial=T) }
		if (!doRob) { spR2 <- mdl[[Var[i]]] %>% effectsize::r2_semipartial() }
		if (!doRob) { gvif <- mdl[[Var[i]]] %>% car::vif() }
		slp <- mdl[[Var[i]]] %>% {c(coef(.)["DaysAbstin"],confint(.,parm="DaysAbstin"))}
		
		Tb$Var          [i] <- Var[i]
		Tb$Excl         [i] <- excl
		Tb$F_Days       [i] <- anv["DaysAbstin",shiRIf(doRob,"F","F value")]
		Tb$df1_Days     [i] <- anv["DaysAbstin","Df"]
		Tb$df2_Days     [i] <- anv["Residuals","Df"]
		Tb$p_Days       [i] <- anv["DaysAbstin","Pr(>F)"]
		Tb$eta2_Days    [i] <- if (doRob) { NA } else { eta2 %>% dplyr::filter(Parameter=="DaysAbstin") %>% {.$Eta2_partial} }
		Tb$spR2_Days    [i] <- if (doRob) { NA } else { spR2 %>% dplyr::filter(Term=="DaysAbstin") %>% {.$r2_semipartial} }
		Tb$gvif_Days    [i] <- if (doRob) { NA } else { gvif["DaysAbstin","GVIF"] }
		Tb$slp_Days     [i] <- slp[1]
		Tb$slp_Days_LCI [i] <- slp[2]
		Tb$slp_Days_UCI [i] <- slp[3]
		
	}
	Tb$pFDR_Days <- func_FdrBy(p = Tb$p_Days, pname = Tb$Var, pgrp = VarClust)
	Tb$pprint_Days <- shiRFormatPval(Tb$p_Days)
	Tb$pFDRprint_Days <- shiRFormatPval(Tb$pFDR_Days)
	Tb %<>% dplyr::mutate(STRING=gsub(', pFDRNA','',gsub('-','–',sprintf(
		"F(%d,%.2f)=%.2f, p%s, pFDR%s, partial η²=%.2f, R²=%.2f%%; %.2f [%.2f,%.2f] ×10⁻³",
		df1_Days, df2_Days, F_Days, pprint_Days, pFDRprint_Days, eta2_Days, spR2_Days*100, slp_Days*1000, slp_Days_LCI*1000, slp_Days_UCI*1000
	))))
	
	return(list(
		mdl = mdl,
		Tb = Tb
	))
	
}

#### Tsnr
func_Stat_Tsnr <- function(Dat) {
	
	dat_long <- Dat %>%
		dplyr::select(-dplyr::contains("Overall")) %>% 
		tidyr::pivot_longer(
			cols = tidyr::matches("^(SegIndivid_|TsnrIndivid_)"),
			names_to = c(".value", "Net"),
			names_pattern = "^(Seg|Tsnr)Individ_(.*)"
		)
	dat_longc <- dat_long %>% dplyr::mutate(
		Tsnr = Tsnr %>% {.-mean(.,na.rm=T)}
	)
	
	#### Power264, Tsnr ~ Net
	mdl <- lmer(Tsnr ~ Net + (1|Subj), data=dat_long)
	anv.Net.Tsnr <- mdl %>% anova()
	emm.Net.Tsnr <- mdl %>% emmeans::emmeans(~Net) %>% summary(infer=c(T,T))
	
	#### Power264, SegNet ~ Tsnr*Net
	mdl.NetTsnr.SegNet <- lmer(Seg ~ Tsnr*Net + (1|Subj), data=dat_longc)
	anv.NetTsnr.SegNet <- mdl.NetTsnr.SegNet %>% anova()
	emtr.NetTsnr.SegNet <- mdl.NetTsnr.SegNet %>% emmeans::emtrends(~Net,"Tsnr") %>% summary(infer=c(T,T))
	
	####
	return(list(
		Tb_anv = rbind(anv.Net.Tsnr,anv.NetTsnr.SegNet) %>% 
			remove_rownames() %>% 
			mutate(Term = c("Net.Tsnr(Net)","NetTsnr.SegNet(Tsnr)","NetTsnr.SegNet(Net)","NetTsnr.SegNet(Tsnr:Net)"),.before=1),
		Tb_em = rbind(
			emm.Net.Tsnr %>% as.data.frame() %>% dplyr::mutate(Net=stringr::str_c("Net.Tsnr.",Net)) %>% dplyr::rename(em=emmean),
			emtr.NetTsnr.SegNet %>% as.data.frame() %>% dplyr::mutate(Net=stringr::str_c("NetTsnr.SegNet.",Net)) %>% dplyr::rename(em=Tsnr.trend)
		)
	))
	
}

#### Grp x Tsnr
func_Stat_mdl.GrpTsnrInt = function(dat, Var, VarClust, C, RFX = "(1|Study)", OutlierCtr = NULL, Weight = NULL) {
	
	Tsnrv <- gsub("Seg","Tsnr",Var,fixed=F)
	
	doRob <- identical(OutlierCtr %>% tolower() %>% substr(1,3), "rob")
	mdl <- list()
	Tb = data.frame(
		Var                = length(Var) %>% character(),
		Excl               = length(Var) %>% numeric(),
		F_Grp              = length(Var) %>% numeric(),
		df1_Grp            = length(Var) %>% numeric(),
		df2_Grp            = length(Var) %>% numeric(),
		p_Grp              = length(Var) %>% numeric(),
		pprint_Grp         = length(Var) %>% character(),
		pFDR_Grp           = length(Var) %>% numeric(),
		pFDRprint_Grp      = length(Var) %>% character(),
		eta2_Grp           = length(Var) %>% numeric(),
		spR2_Grp           = length(Var) %>% numeric(),
		gvif_Grp           = length(Var) %>% numeric(),
		F_TsnrInt          = length(Var) %>% numeric(),
		df1_TsnrInt        = length(Var) %>% numeric(),
		df2_TsnrInt        = length(Var) %>% numeric(),
		p_TsnrInt          = length(Var) %>% numeric(),
		pprint_TsnrInt     = length(Var) %>% character(),
		pFDR_TsnrInt       = length(Var) %>% numeric(),
		pFDRprint_TsnrInt  = length(Var) %>% character(),
		eta2_TsnrInt       = length(Var) %>% numeric(),
		spR2_TsnrInt       = length(Var) %>% numeric(),
		gvif_TsnrInt       = length(Var) %>% numeric(),
		emm_Grp_HiTsnr     = length(Var) %>% numeric(),
		emm_Grp_HiTsnr_LCI = length(Var) %>% numeric(),
		emm_Grp_HiTsnr_HCI = length(Var) %>% numeric(),
		emm_Grp_LoTsnr     = length(Var) %>% numeric(),
		emm_Grp_LoTsnr_LCI = length(Var) %>% numeric(),
		emm_Grp_LoTsnr_HCI = length(Var) %>% numeric(),
		emtr_Tsnr_HC       = length(Var) %>% numeric(),
		emtr_Tsnr_HC_LCI   = length(Var) %>% numeric(),
		emtr_Tsnr_HC_UCI   = length(Var) %>% numeric(),
		emtr_Tsnr_OUD      = length(Var) %>% numeric(),
		emtr_Tsnr_OUD_LCI  = length(Var) %>% numeric(),
		emtr_Tsnr_OUD_UCI  = length(Var) %>% numeric()
		)
	
	dat_tsnrc <- dat %>% dplyr::mutate(dplyr::across(dplyr::starts_with("Tsnr"), ~.x-mean(.x,na.rm=T)))
	
	for (i in 1:length(Var)) {
		
		paste0(" ", as.character(match.call()[[1]]), " --- ", Var[i], " \n") %>% crayon::black() %>% crayon::bgYellow() %>% cat()
		
		mdl <- func_Stat_Model(
			shiRFormula(Var[i],c(paste0("Grp*",Tsnrv[i]),C,RFX)),
			Dat = dat_tsnrc,
			OutlierCtr = OutlierCtr,
			Weight = Weight
		)
		
		mdl[[Var[i]]] <- mdl$mdl
		excl <- mdl$excl
		
		anv <- mdl[[Var[i]]] %>% anova(ddf=shiRIf(doRob,"satterthwaite","Satterthwaite"))
		if (!doRob) { eta2 <- anv %>% effectsize::eta_squared(partial=T) }
		if (!doRob) { spR2 <- mdl[[Var[i]]] %>% r2glmm::r2beta(method="nsj") }
		if (!doRob) { gvif <- mdl[[Var[i]]] %>% car::vif() }
		emm <- mdl[[Var[i]]] %>% emmeans::emmeans(shiRFormula("",paste("Grp |",Tsnrv[i])) %>% as.formula(), at=setNames(list(dat_tsnrc[[Tsnrv[i]]] %>% {mean(.)+sd(.)*c(1,-1)}), Tsnrv[i]))
		ctr <- emm %>% emmeans::contrast(list(HC_v_OUD=c(1,-1)), by=Tsnrv[i]) %>% summary(infer=c(T,T))
		emtr <- mdl[[Var[i]]] %>% emmeans::emtrends(~Grp,Tsnrv[i]) %>% summary(infer=c(T,T))
		
		Tb$Var           [i] <- Var[i]
		Tb$Excl      [i] <- excl
		
		Tb$F_Grp              [i] <- anv["Grp","F value"]
		Tb$df1_Grp            [i] <- anv["Grp","NumDF"]
		Tb$df2_Grp            [i] <- anv["Grp","DenDF"]
		Tb$p_Grp              [i] <- anv["Grp","Pr(>F)"]
		Tb$eta2_Grp           [i] <- if (doRob) { NA } else { eta2 %>% dplyr::filter(Parameter=="Grp") %>% {.$Eta2_partial} }
		Tb$spR2_Grp           [i] <- if (doRob) { NA } else { spR2 %>% dplyr::filter(Effect=="Grp") %>% {.$Rsq} }
		Tb$gvif_Grp           [i] <- if (doRob) { NA } else { gvif["Grp","GVIF"] }
		Tb$F_TsnrInt          [i] <- anv[paste0("Grp:",Tsnrv[i]),"F value"]
		Tb$df1_TsnrInt        [i] <- anv[paste0("Grp:",Tsnrv[i]),"NumDF"]
		Tb$df2_TsnrInt        [i] <- anv[paste0("Grp:",Tsnrv[i]),"DenDF"]
		Tb$p_TsnrInt          [i] <- anv[paste0("Grp:",Tsnrv[i]),"Pr(>F)"]
		Tb$eta2_TsnrInt       [i] <- if (doRob) { NA } else { eta2 %>% dplyr::filter(Parameter==paste0("Grp:",Tsnrv[i])) %>% {.$Eta2_partial} }
		Tb$spR2_TsnrInt       [i] <- if (doRob) { NA } else { spR2 %>% dplyr::filter(Effect==paste0("Grp:",Tsnrv[i])) %>% {.$Rsq} }
		Tb$gvif_TsnrInt       [i] <- if (doRob) { NA } else { gvif[paste0("Grp:",Tsnrv[i]),"GVIF"] }
		Tb$emm_Grp_HiTsnr     [i] <- ctr %>% dplyr::filter(!!sym(Tsnrv[i])>0) %>% {.$estimate}
		Tb$emm_Grp_HiTsnr_LCI [i] <- ctr %>% dplyr::filter(!!sym(Tsnrv[i])>0) %>% {.$lower.CL}
		Tb$emm_Grp_HiTsnr_HCI [i] <- ctr %>% dplyr::filter(!!sym(Tsnrv[i])>0) %>% {.$upper.CL}
		Tb$emm_Grp_LoTsnr     [i] <- ctr %>% dplyr::filter(!!sym(Tsnrv[i])<0) %>% {.$estimate}
		Tb$emm_Grp_LoTsnr_LCI [i] <- ctr %>% dplyr::filter(!!sym(Tsnrv[i])<0) %>% {.$lower.CL}
		Tb$emm_Grp_LoTsnr_HCI [i] <- ctr %>% dplyr::filter(!!sym(Tsnrv[i])<0) %>% {.$upper.CL}
		Tb$emtr_Tsnr_HC       [i] <- emtr %>% dplyr::filter(Grp=="HC") %>% {.[[paste0(Tsnrv[i],".trend")]]}
		Tb$emtr_Tsnr_HC_LCI   [i] <- emtr %>% dplyr::filter(Grp=="HC") %>% {.$lower.CL}
		Tb$emtr_Tsnr_HC_UCI   [i] <- emtr %>% dplyr::filter(Grp=="HC") %>% {.$upper.CL}
		Tb$emtr_Tsnr_OUD      [i] <- emtr %>% dplyr::filter(Grp=="OUD") %>% {.[[paste0(Tsnrv[i],".trend")]]}
		Tb$emtr_Tsnr_OUD_LCI  [i] <- emtr %>% dplyr::filter(Grp=="OUD") %>% {.$lower.CL}
		Tb$emtr_Tsnr_OUD_UCI  [i] <- emtr %>% dplyr::filter(Grp=="OUD") %>% {.$upper.CL}
		
	}
	Tb$pFDR_Grp <- func_FdrBy(p = Tb$p_Grp, pname = Tb$Var, pgrp = VarClust)
	Tb$pprint_Grp <- shiRFormatPval(Tb$p_Grp)
	Tb$pFDRprint_Grp <- shiRFormatPval(Tb$pFDR_Grp)
	Tb$pFDR_TsnrInt <- func_FdrBy(p = Tb$p_TsnrInt, pname = Tb$Var, pgrp = VarClust)
	Tb$pprint_TsnrInt <- shiRFormatPval(Tb$p_TsnrInt)
	Tb$pFDRprint_TsnrInt <- shiRFormatPval(Tb$pFDR_TsnrInt)
	
	return(list(
		mdl = mdl,
		Tb = Tb
	))
	
}

#### Yrs x Tsnr
func_Stat_mdl.YrsTsnrInt = function(dat, Var, VarClust, C, RFX = "(1|Study)", OutlierCtr = NULL, Weight = NULL) {
	
	Tsnrv <- gsub("Seg","Tsnr",Var,fixed=F)
	
	doRob <- identical(OutlierCtr %>% tolower() %>% substr(1,3), "rob")
	mdl <- list()
	Tb = data.frame(
		Var                 = length(Var) %>% character(),
		Excl                = length(Var) %>% numeric(),
		F_Yrs               = length(Var) %>% numeric(),
		df1_Yrs             = length(Var) %>% numeric(),
		df2_Yrs             = length(Var) %>% numeric(),
		p_Yrs               = length(Var) %>% numeric(),
		pprint_Yrs          = length(Var) %>% character(),
		pFDR_Yrs            = length(Var) %>% numeric(),
		pFDRprint_Yrs       = length(Var) %>% character(),
		eta2_Yrs            = length(Var) %>% numeric(),
		spR2_Yrs            = length(Var) %>% numeric(),
		gvif_Yrs            = length(Var) %>% numeric(),
		F_TsnrInt           = length(Var) %>% numeric(),
		df1_TsnrInt         = length(Var) %>% numeric(),
		df2_TsnrInt         = length(Var) %>% numeric(),
		p_TsnrInt           = length(Var) %>% numeric(),
		eta2_TsnrInt        = length(Var) %>% numeric(),
		spR2_TsnrInt        = length(Var) %>% numeric(),
		gvif_TsnrInt        = length(Var) %>% numeric(),
		emtr_Yrs_HiTsnr     = length(Var) %>% numeric(),
		emtr_Yrs_HiTsnr_LCI = length(Var) %>% numeric(),
		emtr_Yrs_HiTsnr_HCI = length(Var) %>% numeric(),
		emtr_Yrs_LoTsnr     = length(Var) %>% numeric(),
		emtr_Yrs_LoTsnr_LCI = length(Var) %>% numeric(),
		emtr_Yrs_LoTsnr_HCI = length(Var) %>% numeric()
		)
	
	dat_tsnrcyrsc <- dat %>% 
		dplyr::filter(!is.na(YrsOpUse) & Grp=="OUD") %>%
		dplyr::mutate(dplyr::across(dplyr::starts_with("Tsnr"), ~.x-mean(.x,na.rm=T))) %>%
		dplyr::mutate(YrsOpUse = YrsOpUse %>% {.-mean(.,na.rm=T)})
	
	for (i in 1:length(Var)) {
		
		paste0(" ", as.character(match.call()[[1]]), " --- ", Var[i], " \n") %>% crayon::black() %>% crayon::bgYellow() %>% cat()
		
		mdl <- func_Stat_Model(
			shiRFormula(Var[i],c(paste0("YrsOpUse*",Tsnrv[i]),C,RFX)),
			Dat = dat_tsnrcyrsc,
			OutlierCtr = OutlierCtr,
			Weight = Weight
		)
		mdl[[Var[i]]] <- mdl$mdl
		excl <- mdl$excl
		
		anv <- mdl[[Var[i]]] %>% anova(ddf=shiRIf(doRob,"satterthwaite","Satterthwaite"))
		if (!doRob) { eta2 <- anv %>% effectsize::eta_squared(partial=T) }
		if (!doRob) { spR2 <- mdl[[Var[i]]] %>% r2glmm::r2beta(method="nsj") }
		if (!doRob) { gvif <- mdl[[Var[i]]] %>% car::vif() }
		emtr <- mdl[[Var[i]]] %>% emmeans::emtrends(shiRFormula("",paste0(Tsnrv[i])) %>% as.formula(), "YrsOpUse", at=setNames(list(dat_tsnrcyrsc[[Tsnrv[i]]] %>% {mean(.)+sd(.)*c(1,-1)}), Tsnrv[i])) %>% summary(infer=c(T,T))
		
		Tb$Var                 [i] <- Var[i]
		Tb$Excl                [i] <- excl
		Tb$F_Yrs               [i] <- anv["YrsOpUse","F value"]
		Tb$df1_Yrs             [i] <- anv["YrsOpUse","NumDF"]
		Tb$df2_Yrs             [i] <- anv["YrsOpUse","DenDF"]
		Tb$p_Yrs               [i] <- anv["YrsOpUse","Pr(>F)"]
		Tb$eta2_Yrs            [i] <- if (doRob) { NA } else { eta2 %>% dplyr::filter(Parameter=="YrsOpUse") %>% {.$Eta2_partial} }
		Tb$spR2_Yrs            [i] <- if (doRob) { NA } else { spR2 %>% dplyr::filter(Effect=="YrsOpUse") %>% {.$Rsq} }
		Tb$gvif_Yrs            [i] <- if (doRob) { NA } else { gvif["YrsOpUse","GVIF"] }
		Tb$F_TsnrInt           [i] <- anv[paste0("YrsOpUse:",Tsnrv[i]),"F value"]
		Tb$df1_TsnrInt         [i] <- anv[paste0("YrsOpUse:",Tsnrv[i]),"NumDF"]
		Tb$df2_TsnrInt         [i] <- anv[paste0("YrsOpUse:",Tsnrv[i]),"DenDF"]
		Tb$p_TsnrInt           [i] <- anv[paste0("YrsOpUse:",Tsnrv[i]),"Pr(>F)"]
		Tb$eta2_TsnrInt        [i] <- if (doRob) { NA } else { eta2 %>% dplyr::filter(Parameter==paste0("YrsOpUse:",Tsnrv[i])) %>% {.$Eta2_partial} }
		Tb$spR2_TsnrInt        [i] <- if (doRob) { NA } else { spR2 %>% dplyr::filter(Effect==paste0("YrsOpUse:",Tsnrv[i])) %>% {.$Rsq} }
		Tb$gvif_TsnrInt        [i] <- if (doRob) { NA } else { gvif[paste0("YrsOpUse:",Tsnrv[i]),"GVIF"] }
		Tb$emtr_Yrs_HiTsnr     [i] <- emtr %>% dplyr::filter(!!sym(Tsnrv[i])>0) %>% {.$YrsOpUse.trend}
		Tb$emtr_Yrs_HiTsnr_LCI [i] <- emtr %>% dplyr::filter(!!sym(Tsnrv[i])>0) %>% {.$lower.CL}
		Tb$emtr_Yrs_HiTsnr_HCI [i] <- emtr %>% dplyr::filter(!!sym(Tsnrv[i])>0) %>% {.$upper.CL}
		Tb$emtr_Yrs_LoTsnr     [i] <- emtr %>% dplyr::filter(!!sym(Tsnrv[i])<0) %>% {.$YrsOpUse.trend}
		Tb$emtr_Yrs_LoTsnr_LCI [i] <- emtr %>% dplyr::filter(!!sym(Tsnrv[i])<0) %>% {.$lower.CL}
		Tb$emtr_Yrs_LoTsnr_HCI [i] <- emtr %>% dplyr::filter(!!sym(Tsnrv[i])<0) %>% {.$upper.CL}
		
	}
	Tb$pFDR_Yrs <- func_FdrBy(p = Tb$p_Yrs, pname = Tb$Var, pgrp = VarClust)
	Tb$pprint_Yrs <- shiRFormatPval(Tb$p_Yrs)
	Tb$pFDRprint_Yrs <- shiRFormatPval(Tb$pFDR_Yrs)
	Tb$pFDR_TsnrInt <- func_FdrBy(p = Tb$p_TsnrInt, pname = Tb$Var, pgrp = VarClust)
	Tb$pprint_TsnrInt <- shiRFormatPval(Tb$p_TsnrInt)
	Tb$pFDRprint_TsnrInt <- shiRFormatPval(Tb$pFDR_TsnrInt)
	
	
	return(list(
		mdl = mdl,
		Tb = Tb
	))
	
}
