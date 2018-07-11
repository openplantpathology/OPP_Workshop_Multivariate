# Principal Component Biplot analysis in R
# Examples are analyses of data in Table 6 in McRoberts & Lennard 1996
# Plant Pathology and some unpublished data from the same experiments

# Ruben Gabriel's original 1971 biplot method is used in the R
# biplot command, which uses the output from either of R's Principal
# Components Analysis commands. All of these are included in the 
# base stats package so you don't need to load a new package
# to run use these techniques

# We are going to use the biplot as a way of inspecting how
# the variance in a table of means from a two-factor ANOVA
# is distributed among the levels of the two factors. This gives
# us a nice visual tool for looking directly at interactions
# and seeing the difference between situations where the 
# interaction is signficant and those where it's not.
# The particualr case we'll look at comes from work on trying to 
# establish where in the infection process host-specificity is
# decided for different Alternaria anamorphs during attempted
# infection on known hosts and non-hosts.

# The first set of data is the percentage of germ-tubes
# of different Alternaria anamorphs which did NOT attempt
# penetration on different plant species.

# The data are in file Alternaria_nopen.csv

Alt_nopen_data <-read.csv(file.choose(), head=FALSE)

# make some labels for the rows and columns of the
# data matrix.

Altspp <-c("alternata","brassicae","brassicicola","infectoria","raphani","solani")
host <- c("Bnapus","Papaver","Lycopersicon","Triticum")
rownames(Alt_nopen_data) <-Altspp
colnames(Alt_nopen_data) <-host

#Calculate attempted penetartion events from raw data

Alt_pen_data <-100-Alt_nopen_data

# Now perform PCA

Alt_pen_PCA <-princomp(Alt_pen_data, cor=FALSE)

summary(Alt_pen_PCA)

# Produce Gabriel bioplot of PCA
Alt_pen_bplt <-biplot(Alt_pen_PCA, pc.biplot=TRUE)


# Now repeat the analysis with the data from Table 6 in M&L 1996.
# These data show the percentage of attempted penetration events
# that were successful.

# The data are in Alternaria_pen.csv

Alt_infect_data <-read.csv(file.choose(), head=FALSE)

# make some labels for the rows and columns of the
# data matrix.
rownames(Alt_infect_data) <-Altspp
colnames(Alt_infect_data) <-host

# Now perform Principal Components analysis and biplot

Alt_infect_PCA <-princomp(Alt_infect_data, cor=FALSE)

summary(Alt_infect_PCA)

# Produce Gabriel bioplot of PCA
Alt_infect_bplt <-biplot(Alt_infect_PCA, pc.biplot=TRUE)

# To see the contributions of the hosts to the PCA solution
# We want the "loadings" (or eigenvectors or latent vectors) from
# each PCA:

Alt_pen_PCA$loadings
Alt_infect_PCA$loadings