rm(list = ls())
library(factoextra)
library(stats)

#Preparation
#____________________________________________________________
DATA <- read("...")
# column 1: Plot ID + Aggregation code, #colum 2 - 627: Bands, #column 628 - xxx: further information


rownames(DATA) = make.names(DATA[,1], unique=TRUE)
DATA[,1] <- NULL

#PCA
#____________________________________________________________
pca <- prcomp(DATA[,-627], scale = TRUE)  #substract columns which are not to be used for the PCA
fviz_eig(pca) # Scree plot
eig.val <- get_eigenvalue(pca) # excat numbers for eigenvalues

#Visualizing:

fviz_pca_ind(pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             title = "PCA: All crops", # with quality of representation # Avoid text overlapping
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_ind(pca,
             col.ind = DATA$Crop, # color by groups - specify
             palette = "Dark2",
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Varieties",
             repel = TRUE,
             title= "Grouping of different crops"
)

# Results for individuals
res.ind <- get_pca_ind(pca)
res.ind
Coor <- res.ind$coord # this is what we want (for dim 1 and 2, depending on eigenvalues -
# 90% of the variance should be explained by the first dimensions)

write... (Coor)
#---------------------------------------------------------
#not so relevant information:

# Results for variables
res.var <- get_pca_var(pca)
Contr <- res.var$contrib        #Contribtuion to PCs
Coord <- res.var$coord          # Coordinates
Cos2 <- res.var$cos2           # Quality of representation 


