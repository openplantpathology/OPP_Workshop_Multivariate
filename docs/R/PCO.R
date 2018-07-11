# This program file contains the code needed to reproduce the PCO examples covered in the 2015 Multivariate
# statistics workshop at the 2015 APS meeting in Pasadena CA.

# Example 1.  Similarity among fields on the basis of margin characteristics

# The data are in file APS2015_PCO1_survey.csv

margin_data <-read.csv(file.choose(), head=TRUE)
head(margin_data)

# Construct a distance matrix from the binary field characteristic data

field_dist <-dist(margin_data[3:22], method="binary")


# Now give the distance matrix to the PCO function to do the dimension reduction.
# We make a vector of labels for the eigenvalues and produce a scree plot of the eigenvalues
# so that we can interpret the PCO solution.

field_pco <-cmdscale(field_dist, eig=TRUE, k=15)
eig_num <-seq(1,length(field_pco$eig),1)
barplot(field_pco$eig, names.arg=eig_num, main="PCO eigenvalue Scree plot")

# In common with PCA, a PCO will result in the largest possible fraction of the variance being captured
# in the first dimension, followed by the second and so on.  In the case of binary data sets where there
# is a relatively large number of unique sequences of binary values among the data objects, one will see
# that the variance is quite evenly distributed over a relatively large number of dimensions.  This
# is one feature apparent in the current example.  Another is that there are negative eigenvalues.
# The presence of negative eigenvalues tells us that the PCO solution is only approximately Euclidean.
# The negative eigenvalues can be thought of as representing imaginary dimensions that absorb some of the 
# distance among the objects in the projected Euclidean space and allow the solution to approximate
# Euclidean distance even though it is not.

fieldPCO_dim1 <-vector(length=54)
fieldPCO_dim2 <-vector(length=54)
fieldPCO_dim3 <-vector(length=54)
fieldPCO_dim1 <-field_pco$points[,1]
fieldPCO_dim2 <-field_pco$points[,2]
fieldPCO_dim3 <-field_pco$points[,3]
fieldpco_plot <-plot(fieldPCO_dim1, fieldPCO_dim2, asp=1, cex=1.5,
                     xlab="PCO dimension 1", ylab="PCO dimension 2")
  text(fieldPCO_dim1, fieldPCO_dim2, cex=1.5, margin_data$region)

fieldpco_plot2 <-plot(fieldPCO_dim1, fieldPCO_dim3, asp=1, cex=1.5,
                     xlab="PCO dimension 1", ylab="PCO dimension 3")
text(fieldPCO_dim1, fieldPCO_dim3, cex=1.5, margin_data$region)

fieldpco_plot3 <-plot(fieldPCO_dim2, fieldPCO_dim3, asp=1, cex=1.5,
                     xlab="PCO dimension 2", ylab="PCO dimension 3")
text(fieldPCO_dim2, fieldPCO_dim3, cex=1.5, margin_data$region)

# If we swap the order of plotting the variables and reverse their signs, we can see that the 
# projection looks something like the geographic map of the regions to which the fields belong.

fieldpco_plot <-plot(-fieldPCO_dim2, -fieldPCO_dim1, asp=1, cex=1.5,
                     xlab="PCO dimension 1", ylab="PCO dimension 2")
text(-fieldPCO_dim2, -fieldPCO_dim1, cex=1.5, margin_data$region)

# An approximate test of the significance of original variables to the PCO solution
# In classical PCO, because the original variables to do directly enter the analysis it
# takes a bit more work than with a PCA to see if they are connected with the PCO solution.
# The following procedure was suggested by the late Pete Digby and others.  If there is evidence
# of grouping along one the PCO dimensions, we can do an approximate variance ratio test using each of 
# the binary variables in turn as a grouping factor and testing the variance ratio of the scores on
# the PCO dimension for objects with and without the variable. Those variables which have large
# F statistics (variance ratios) are indicated as being associated with the grouping.  It is not
# axiomatically true that the PCO scores have a Normal distribution so the test should be taken
# as approximate only.

# Inspectionn of the PCO plots suggests that there are two clusters of fields on the third PCO
# dimension.  These do not appear to have a regional basis so we will use the approximate F test
# to see which variables might be associated with the clustering.  To do this we're going to use a
# small for loop.

select_data <-margin_data[3:22]
Fvals <-vector(length=ncol(select_data))
maxitem <-length(Fvals)

for (i in 1:maxitem){
  lm1<-lm(fieldPCO_dim3~select_data[,i])
  av1 <-anova(lm1)
  Fvals[i]<-av1$F[1]
}
Fvalplot <-barplot(log(Fvals,10), names.arg=colnames(select_data), cex.names=0.5, 
                   main="Approximate v.r. values on PCO dim 3")

# Plotting the barplot of F values on a log scale immediately lets you differentiate those with v.r.>1
# (above the line) from those with v.r. <1 (below the line).  We are interested only in those above the
# line and in this case it is clear that one variable (grass as aa neighboring crop) dominates the others.
# The cluster of fields with positive values on PCO dimension 3 either have grass crops as neghbors or
# are close to farm buildings with uncultivated ground, rough tracks or shelterbelts nearby. These
# features may or may not be of any significance in the ecology of the plant pathogens associated with
# the crops.
