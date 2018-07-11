# Non-hierarchical Cluster Analysis
# For this analysis we'll need the data set from the PCA that we carried out
# at the start of the workshop.  Since we know the rust data were all 0's we
# can work with the reduced data set that we generated with the rust variables
# dropped.  If you ran the R code without editing any of the object names and
# if you haven't re-started R since you ran the PCA you should have a data frame
# called PCA1_cordata in your environment. To check if it is there just type
# the name at the console.  If the data frame is in your environment R will 
# print it to the screen.  If it's not there R will tell you it does not exist.
# If you don't have the data frame in your environment, the following code will
# let you re-load the data and re-create the data frame we want.

# The data are in file APS2015_PCA1_Survey.csv

PCA1_data <-read.csv(file.choose(), head=TRUE)
PCA1_cordata <-PCA1_data[c(-7,-8,-9)]
head(PCA1_cordata)

# R provides the funciton K-means in the base stats package to do non-hierachical
# cluster analysis.  In a K-means analysis the clustering algorithm moves objects
# between clusters until it finds the minimum value of the within-group sums of
# of squares, given the number of clusters specified.

# As a first exercise, let's assume that there are 3 clusters of fields and look
# at the output that K-means produces.

# Because we already know that the data are on different measurement scales and the
# clustering algorithm is working on sums of squares we might suspect that the solution
# will be dominated by the data variables with the largest absolute values. To guard
# against that effect we can use the scale() function to mean-center the data divide 
# each variable by its standard deviation before the analysis. Note that scale() only 
# works on numeric data so we split the data into two sections and apply scale() only 
# to columns 3 to 14.

NHCA_data1 <-data.frame(PCA1_cordata[,1:2],scale(PCA1_cordata[,3:14]))
head(NHCA_data1)

# Cluster the fields into 3 groups
NHCA_fields1 <-kmeans(NHCA_data1[,3:14],3)
NHCA_fields1

# Asking R to print the kmeans object produces a summary of the analysis in the following order:
# The number of objects in each cluster
# The value of the data variables at the cluster centroids
# The vector of cluster membership for the objects in the order they appear in the data set
# The within-cluster sums of squares and the relative size of within- and between-cluster SS

# Since the number of data objects is relatively small we can use a simple barplot to look at
# how the fields from each region have been allocated to the new clusters.

NHCA_3clus_plt <-barplot(NHCA_fields1$cluster, names.arg=PCA1_cordata$region, cex.names=0.5,
                         xlab="region", ylab="cluster number")

# Group 1 is dominated by fields in the North region, with one from the West and five from the
# East.  Group 3 is predominantly composed of sites from the West and to a lesser extent the East
# and contains no northern fields. Group 2, which has only five members comprises three fields from 
# the North and two from the East.  The within cluster sums of squares is 70% of the total. Given
# that the clustering is an attempt to minimize within cluster S.S. we might well ask whether 3
# clusters is the optimum number.

# Diagnosing the optimum number of clusters
wSS_fields1 <-(nrow(NHCA_data1)-1)*sum(apply(NHCA_data1[,3:14],2,var))
for(i in 2:20) wSS_fields1[i] <-sum(kmeans(NHCA_data1[,3:14], centers=i)$withinss)
clus_plot <-barplot(wSS_fields1, xlab="number of clusters",ylab="within cluster S.S.",
                    names.arg=seq(1,20,1))
wSS_fields1_pct <-(wSS_fields1/wSS_fields1[1])*100
clus_plot_pct <-barplot(wSS_fields1_pct, xlab="number of clusters",
                    ylab="within cluster %S.S.",names.arg=seq(1,20,1))

# The for() loop cycles through the clustering procedure. extracts the within-cluster sum of 
# squares and stores it in rows 2 to 20 of the data structure wSS_fields1 which was declared
# explicitly outside the loop.  Note that we get the total sum of squares in the calculation 
# that declares wSS_fields1 using the sum() and apply() functions.  The result is stored by
# implication in the first line of the new data structure. Note that this little piece of 
# coding shows a feature of loop writing that is often needed: we do the first calculation
# outside the loop and then write the loop so that it cycles from 2 to our maximum.  In this
# case that let's us use the loop counter, i, to be the number of clusters we want to extract
# from the data.

# The barplot shows how the within-cluster sum of squares drops as the number of clusters grows.
# we can use this (if we are lucky), in the same way we use a scree plot in ordination to select
# the number of dimensions, to work out how many clusters are present.  In a clear case, we would
# choose the number just after a dramatic decrease in the wSS value.  In this case we are not
# so lucky. The Within group S.S. shows a fairly steady decline as the number of clusters increases.
# The first value of wSS_fields1 contains the total S.S. for the data so by calculating the values 
# for the rest of the bars as a percentage of the first one, we can see how the within-cluster S.S.
# decreases as a proportion of the total as the number of clusers increases.
# The values for 6 and 7 clusters are 48.8% and 40.7% respectively. so with 7 clusters the between 
# cluster S.S. is >50% of the total and the within cluster S.S. has dropped by a relatively large
# increment in moving from 6 to 7 clusters.  These features together suggest that a model with
# 7 clusters might be of interest.

# Cluster the fields into 7 groups
NHCA_fields2 <-kmeans(NHCA_data1[,3:14],7)
NHCA_fields2
NHCA_7clus_plt <-barplot(NHCA_fields2$cluster, names.arg=PCA1_cordata$region, cex.names=0.5,
                         xlab="region", ylab="cluster number")

# Although the solution with 7 clusters has reduced the within-cluster S.S. to <50%, it has a
# couple of clusters that have only 1 or 2 members. This may reflect fields that are truly 
# different from the rest of the sample in some important respect, but it might also be an 
# artefact of the clustering process.  It certainly suggests that we should look at the solution
# with only 6 clusters since we now have the hang of running the analysis.

# Cluster the fields into 7 groups
NHCA_fields3 <-kmeans(NHCA_data1[,3:14],6)
NHCA_fields3
NHCA_6clus_plt <-barplot(NHCA_fields3$cluster, names.arg=PCA1_cordata$region, cex.names=0.5,
                         xlab="region", ylab="cluster number")

# Sacrifcing some of the within-cluster S.S. by moving from 7 to 6 clusters didn't really gain us
# anything useful; we still have a couple of fields that apparently do not belong to any of the 
# larger clusters. Some general results from the analysis are apparent, however.  The majority of 
# fields from the West remain clustered together, while the majority of fields from the North form
# a second relatively stable cluster.  The remaining fields from these regions and most of those in
# the East tend to change cluster identity as the number of clusters changes. suggesting that they
# do not have as uniform a pattern of diseases as those from the other regions.

# A graphical representation of how coherent the different clusters are, would be helpful in interperting
# the results of the cluster analysis.  We will look at how ordination can be combined with cluster
# analysis to achieve this, but before doing that, we will look at hierarchical clustering.

#Hierachical cluster analysis: Euclidean distance matrix, furthest neighbor clustering
field_Eucdis <-dist(NHCA_data1[,3:14], method="euclidean")
HCA_field1 <-hclust(field_Eucdis, method="complete")
dend_field1 <-plot(HCA_field1)
field_hgroups1 <-cutree(HCA_field1, k=7)
rect.hclust(HCA_field1, k=7, border="blue")

#Euclidean distance matrix, nearest neighbor clustering
HCA_field2 <-hclust(field_Eucdis, method="single")
dend_field2 <-plot(HCA_field2)
field_hgroups2 <-cutree(HCA_field2, k=7)
rect.hclust(HCA_field2, k=7, border="red")

# How do the results look if we use binary data?
# Binary distance matrix, furthest neighbor clustering
HCA_data2 <-data.frame(PCA1_cordata[,1:2],(PCA1_cordata[,3:14]>0)*1)
field_bindis <-dist(HCA_data2[,3:14], method="binary") 
HCA_field3 <-hclust(field_bindis, method="complete")
dend_field3 <-plot(HCA_field3)
field_hgroups3 <-cutree(HCA_field3, k=7)
rect.hclust(HCA_field3, k=7, border="blue")

#Binary distance matrix, nearest neighbor clustering
HCA_field4 <-hclust(field_bindis, method="single")
dend_field4 <-plot(HCA_field4)
field_hgroups4 <-cutree(HCA_field4, k=7)
rect.hclust(HCA_field4, k=7, border="red")
