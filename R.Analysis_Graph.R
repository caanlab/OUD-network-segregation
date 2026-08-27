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
	dplyr::mutate(across(c(Subj), as.factor))


#### mat prep ----

EdgeCol <- setdiff(colnames(dat_Edge), "Subj")

NodeAll <- EdgeCol %>%
	stringr::str_remove("^Edge_") %>%
	stringr::str_split("_") %>%
	unlist() %>%
	unique() %>%
	sort()

stopifnot(length(NodeAll) == 264)

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
			
			out$Subj <- dat_Edge$Subj[i]
			
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
			
			out$nNode <- matAvail %>% 
				igraph::graph_from_adjacency_matrix(mode="undirected", diag=F, weighted=F) %>% 
				igraph::components() %>% 
				{.$csize} %>% max()
			
			lv <- func_LouvainRep(matAvail, weighted=F, nRun=100)
			out$Qmean <- lv$QMean
			out$Qbest <- lv$QBest
			
			out$NMI <- aricode::NMI(unlist(lv$PartBest) %>% as.character(), p0Avail %>% as.character())
			
			out$EffGlo <- func_Efficiency(matAvail, weighted=F)
			
			return(out)
			
		}
	)
	
}


#### output ----

SparList <- data.frame(
	MinLargestCompPerc = sapply(GraphOut, function(m) min(unlist(lapply(m$Out, "[[", "nNode")))) / length(NodeName),
	Sparsity = sapply(GraphOut, "[[", "Sparsity")
) 
SparMin <- SparList %>% 
	dplyr::filter(MinLargestCompPerc>0.5) %>% 
	dplyr::filter(MinLargestCompPerc==min(MinLargestCompPerc)) %>% 
	{.$Sparsity}
SparIdx <- which(SparList$Sparsity == SparMin)

DAT_Graph <- data.frame(Subj = dat_Edge$Subj)

DAT_Graph[["Graph_Qmean" ]] <- sapply(seq_along(GraphOut[[1]]$Out), function(y) { mean(sapply(GraphOut[SparIdx:length(GraphOut)], function(x) { x$Out[[y]]$Qmean  }), na.rm=T) })
DAT_Graph[["Graph_Qbest" ]] <- sapply(seq_along(GraphOut[[1]]$Out), function(y) { mean(sapply(GraphOut[SparIdx:length(GraphOut)], function(x) { x$Out[[y]]$Qbest  }), na.rm=T) })
DAT_Graph[["Graph_NMI"   ]] <- sapply(seq_along(GraphOut[[1]]$Out), function(y) { mean(sapply(GraphOut[SparIdx:length(GraphOut)], function(x) { x$Out[[y]]$NMI    }), na.rm=T) })
DAT_Graph[["Graph_EffGlo"]] <- sapply(seq_along(GraphOut[[1]]$Out), function(y) { mean(sapply(GraphOut[SparIdx:length(GraphOut)], function(x) { x$Out[[y]]$EffGlo }), na.rm=T) })

write.csv(DAT_Graph, "R.Data_Graph.csv", row.names=F)
sprintf("%.2f", SparList$Sparsity[SparIdx:length(GraphOut)]) %>%
	paste(collapse=" ") %>% 
	cat(file="R.Data_Graph_Threshold.txt")
