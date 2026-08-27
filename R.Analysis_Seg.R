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

setwd('/Users/zhshi/Documents/2026/OUD rest/R')
source('R.Analysis_Prep.R')

dat_Edge <- read.csv("R.Source_Edge.csv", na.strings=c("NA","NaN","nan","#N/A","")) %>%
	dplyr::mutate(across(c(Subj), as.factor)) %>%
	dplyr::select(!contains("UnKn"))


#### mat prep ----

EdgeCol <- setdiff(colnames(dat_Edge), "Subj")

NodeAll <- EdgeCol %>%
	stringr::str_remove("^Edge_") %>%
	stringr::str_split("_") %>%
	unlist() %>%
	unique() %>%
	sort()

stopifnot(length(NodeAll) == 236) # 264 minus 28 uncertain nodes

Part0 <- substr(NodeAll, 1, 4)
names(Part0) <- NodeAll

NodeIdx <- data.frame(
	node = NodeAll,
	idx = 1:length(NodeAll)
)


#### mat ----

plan(multisession)

AdjLst <- future_lapply(
	seq_len(nrow(dat_Edge)),
	function(i) {
		
		nNode <- nrow(NodeIdx)
		
		mat <- matrix(0, nNode, nNode)
		rownames(mat) <- NodeIdx$node
		colnames(mat) <- NodeIdx$node
		
		EdgeVec <- as.numeric(dat_Edge[i, EdgeCol])
		
		for (j in 1:length(EdgeCol)) {
			
			nodes <- stringr::str_split(str_remove(EdgeCol[j], "^Edge_"), "_")[[1]]
			
			idx1 <- NodeIdx$idx[match(nodes[1], NodeIdx$node)]
			idx2 <- NodeIdx$idx[match(nodes[2], NodeIdx$node)]
			
			mat[idx1, idx2] <- EdgeVec[j]
			mat[idx2, idx1] <- EdgeVec[j]
			
		}
		
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


#### main ----

meanPos <- function(x, ...) { mean(x[is.finite(x) & x>0], ...) }

SegNode <- vector("list", length(AdjLst))
SegNet <- vector("list", length(AdjLst))

for (i in seq_along(AdjLst)) {
	
	mat <- AdjLst[[i]]$mat
	rowNet <- substr(rownames(mat), 1, 4)
	colNet <- substr(colnames(mat), 1, 4)
	
	wthNode <- sapply(seq_len(nrow(mat)), function(i) { meanPos(mat[i, which(colNet==rowNet[i])], na.rm=T) })
	btwNode <- sapply(seq_len(nrow(mat)), function(i) { meanPos(mat[i, which(colNet!=rowNet[i])], na.rm=T) })
	
	SegNode[[i]] <- (wthNode - btwNode) / wthNode
	names(SegNode[[i]]) <- paste0("SegNodal_", rownames(mat))
	
	NetName <- unique(rowNet)
	
	wthNet <- numeric(length(NetName))
	btwNet <- numeric(length(NetName))
	xSegNet <- numeric(length(NetName))
	
	for (j in seq_along(NetName)) {
		net <- NetName[j]
		wthNet[j] <- mat[rowNet==net, colNet==net, drop=F] %>% meanPos(na.rm=T)
		btwNet[j] <- mat[rowNet==net, colNet!=net, drop=F] %>% meanPos(na.rm=T)
		xSegNet[j] <- (wthNet[j] - btwNet[j]) / wthNet[j]
	}
	
	SegNet[[i]] <- xSegNet
	names(SegNet[[i]]) <- paste0("SegIndivid_",NetName)
	
}

stopifnot(all(sapply(SegNode, function(x) { identical(names(x), names(SegNode[[1]])) })))
stopifnot(all(sapply(SegNet, function(x) { identical(names(x), names(SegNet[[1]])) })))


#### output ----

DAT_SegNodal <- do.call(rbind, SegNode) %>% as.data.frame()
DAT_Seg <- do.call(rbind, SegNet) %>% as.data.frame()

xSeg_As <- rowMeans(DAT_Seg %>% dplyr::select(starts_with("SegIndivid_As")))
xSeg_Sm <- rowMeans(DAT_Seg %>% dplyr::select(starts_with("SegIndivid_Sm")))
xSeg_All <- rowMeans(DAT_Seg %>% dplyr::select(starts_with("SegIndivid_")))

DAT_SegNodal <- cbind(
	data.frame(Subj = dat_Edge$Subj),
	DAT_SegNodal
)

DAT_Seg <- cbind(
	data.frame(Subj = dat_Edge$Subj),
	data.frame(
		SegWholeBrain_All = xSeg_All,
		SegOverall_As = xSeg_As,
		SegOverall_Sm = xSeg_Sm
	),
	DAT_Seg
)

write.csv(DAT_SegNodal,	"R.Data_SegNodal.csv", row.names=F)
write.csv(DAT_Seg, "R.Data_SegNet.csv", row.names=F)
