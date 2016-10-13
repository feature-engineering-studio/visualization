# R code for HUDK5053 Deliverable 4 (Visualization)

# Set the working directory, read dataset, view dataframe
setwd("~/R")
df <- read.csv("hudk5053_v1.csv")
View(df)

# Visualization 1: Histogram 
plot(df$F1D29A) # This will plot automatically before you decide 
hist(df$F1D29A) # This will plot histogram for F1D29A variable 
hist(df$F1D29B) # Repeat this until F1D29U
hist(df$F1D29C)
hist(df$F1D29D)
hist(df$F1D29E)
hist(df$F1D29F)
hist(df$F1D29G)
hist(df$F1D29H)
hist(df$F1D29I)
hist(df$F1D29J)
hist(df$F1D29K)
hist(df$F1D29L)
hist(df$F1D29M)
hist(df$F1D29N)
hist(df$F1D29O)
hist(df$F1D29P)
hist(df$F1D29Q)
hist(df$F1D29R)
hist(df$F1D29S)
hist(df$F1D29T)
hist(df$F1D29U)

# Recode the missing data in Excel ("-9 ~ -1" into "999"): Because too many missing
# Read and view the new dataset "hudk5053_v2.csv"
df1 <- read.csv("hudk5053_v2.csv")
View(df1)

# Recoding values to missing
df1$F1D29A[df1$F1D29A==999] <- NA # Recoding "999" into "NA" for variable F1D29A
hist(df1$F1D29A) # Check if it is coded well (should only have "0" and "1")
df1$F1D29B[df1$F1D29B==999] <- NA # Repeat this until F1D29U
hist(df1$F1D29B)
df1$F1D29C[df1$F1D29C==999] <- NA
hist(df1$F1D29C)
df1$F1D29D[df1$F1D29D==999] <- NA
hist(df1$F1D29D)
df1$F1D29E[df1$F1D29E==999] <- NA
hist(df1$F1D29E)
df1$F1D29F[df1$F1D29F==999] <- NA
hist(df1$F1D29F)
df1$F1D29G[df1$F1D29G==999] <- NA
hist(df1$F1D29G)
df1$F1D29H[df1$F1D29H==999] <- NA
hist(df1$F1D29H)
df1$F1D29I[df1$F1D29I==999] <- NA
hist(df1$F1D29I)
df1$F1D29J[df1$F1D29J==999] <- NA
hist(df1$F1D29J)
df1$F1D29K[df1$F1D29K==999] <- NA
hist(df1$F1D29K)
df1$F1D29L[df1$F1D29L==999] <- NA
hist(df1$F1D29L)
df1$F1D29M[df1$F1D29M==999] <- NA
hist(df1$F1D29M)
df1$F1D29N[df1$F1D29N==999] <- NA
hist(df1$F1D29N)
df1$F1D29O[df1$F1D29O==999] <- NA
hist(df1$F1D29O)
df1$F1D29P[df1$F1D29P==999] <- NA
hist(df1$F1D29P)
df1$F1D29Q[df1$F1D29Q==999] <- NA
hist(df1$F1D29Q)
df1$F1D29R[df1$F1D29R==999] <- NA
hist(df1$F1D29R)
df1$F1D29S[df1$F1D29S==999] <- NA
hist(df1$F1D29S)
df1$F1D29T[df1$F1D29T==999] <- NA
hist(df1$F1D29T)
df1$F1D29U[df1$F1D29U==999] <- NA
hist(df1$F1D29U)

# Omit N/A data with new data frame
df2 <- na.omit(df1)
View(df2)

# Visualization 2: Cluster Dendrogram
d <- dist(as.matrix(df2))
hc <- hclust(d)
plot(hc, hang = -1)

# Run LCA to identify four classes
# Visualization 3: LCA
library(poLCA)
lca = poLCA(cbind(F1D29A=F1D29A+1, F1D29B=F1D29B+1, F1D29C=F1D29C+1, F1D29D=F1D29D+1, F1D29E=F1D29E+1, F1D29F=F1D29F+1, F1D29G=F1D29G+1, F1D29H=F1D29H+1, F1D29I=F1D29I+1, F1D29J=F1D29J+1, F1D29K=F1D29K+1, F1D29L=F1D29L+1, F1D29M=F1D29M+1, F1D29N=F1D29N+1, F1D29O=F1D29O+1, F1D29P=F1D29P+1, F1D29Q=F1D29Q+1, F1D29R=F1D29R+1, F1D29S=F1D29S+1, F1D29T=F1D29T+1, F1D29U=F1D29U+1) ~ 1, maxiter=50000, nclass=4, nrep=10, data=df2)
plot(lca)


# Possible analysis: We can aggregate variables by classes and generate heatmap. 