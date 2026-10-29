source('oudrestR.Init.R')

#### data import ----

load("oudrestR.Data.DemClinCov.RData")

load("oudrestR.Data.Edge_Power264.RData")
func_Prep_Combat(datEdge, datBehav, "oudrestR.Data.Edge_Power264Combat.RData")
func_Prep_Graph(datEdge, "oudrestR.Data.Graph_Power264.RData")
func_Prep_Seg(datEdge, list(As="As",Sm="Sm"), "oudrestR.Data.Seg_Power264.RData")

rm(datEdge)

load("oudrestR.Data.Edge_Power264Combat.RData")
func_Prep_Graph(datEdge, "oudrestR.Data.Graph_Power264Combat.RData")
func_Prep_Seg(datEdge, list(As="As",Sm="Sm"), "oudrestR.Data.Seg_Power264Combat.RData")

rm(datEdge)

load("oudrestR.Data.Edge_Schaefer1000.RData")
func_Prep_Graph(datEdge, "oudrestR.Data.Graph_Schaefer1000.RData")
func_Prep_Seg(datEdge, list(All=""), "oudrestR.Data.Seg_Schaefer1000.RData")
