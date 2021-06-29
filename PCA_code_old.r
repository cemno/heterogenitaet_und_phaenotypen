rm(list = ls())
library(factoextra)
library(openxlsx)
library(stats)

#Preparation
#____________________________________________________________
DATA <- read("...")
# column 1: Plot ID + Aggregation code, #column 2 - 627: Bands


rownames(DATA) = make.names(DATA[,1], unique=TRUE)
DATA[,1] <- NULL

#PCA
#____________________________________________________________
pca <- prcomp(DATA, scale = TRUE)
fviz_eig(pca) # Scree plot

#Visualizing:

fviz_pca_ind(pca,
             col.ind = "cos2", # Colour by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             title = "PCA: All crops", # with quality of representation # Avoid text overlapping
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_ind(pca,
             col.ind = Varieties, # colour by groups
             palette = "Dark2",
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Varieties",
             repel = TRUE,
             title= "Grouping of different crops"
)


# Results for variables
res.var <- get_pca_var(pca)
#--------------------------
#Check contribution
Contr <- res.var$contrib        
Contr <- as.data.frame(Contr)
Dim1 <- Contr[order(Contr$Dim.1, decreasing = TRUE),][,1:2]
Dim2 <- Contr[order(Contr$Dim.2, decreasing = TRUE),][,1:2]
Dim3 <- Contr[order(Contr$Dim.3, decreasing = TRUE),][,1:3]

Final <- cbind(Dim1,Dim2, Dim3)

write(Final,"...")


# Eigenvalues
eig.val <- get_eigenvalue(pca)
eig.val

# Coordiates
res.var$coord          # Coordinates
res.var$cos2           # Quality of representation 


