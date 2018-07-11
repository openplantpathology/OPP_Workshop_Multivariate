# Principal Components Analysis Example 1
# Analysis of a survey of winter wheat crops from different regions in Scotland
# The data set is for 16 disease variables measured on 41 crops from 3 different
# regions

# Get the data set.  The data are in file APS2015_PCA1_Survey.csv

PCA1data <-read.csv(file.choose(), head=TRUE)

# Look at the top of the dataset just to check it looks OK. Note that
# the head=TRUE option in read.csv tells R to expect to find labels
# in the first row of the data file.  If your data file just contains
# unlabeled data use head=FALSE and note that the TRUE and FALSE have
# to be in upper case, while head is in lower case.

head(PCA1data)

# The first two columns are an individual field identifier and a region identifier.
# we don't want those for the analysis.

survey_pca <-princomp(PCA1data[3:17])

# A PCA analysis gives three sorts of infomation:
# 1. A breakdown of how the variance in the data is captured in the new variables
# that are formed.
# 2. The loadings (or contributions) of the original variables to the principal components
# 3. The scores (or coordinates) for the objects being analysed on the principal components

# Look at the breakdown of variance first - do this using summary()

summary_pca1 <-summary(survey_pca)
summary_pca1

# We can look at this graphically in the form of a scree plot. R has a special version
# of its general "plot" command that will accept a PCA structure as its input and 
# produce a scree plot.

pca1_scree <-plot(survey_pca)

# Next we want to look at how the original variables contribute to the new principal components

loadings_pca1 <-survey_pca$loadings
loadings_pca1

# We can print out the scores for the fields in a set of lists, but often the best way
# to see what the PCA has achieved is to plot the positions for the objects in the 
# "space" of the principal components.  To do that we'll need the scores from the new
# pca structure.
pca1_plot2_1 <-plot(survey_pca$scores[,1],survey_pca$scores[,2], cex=1.5,
                    xlab="first principal component 54% variance",
                    ylab="second principal component 32% variance")

pca1_plot3_2 <-plot(survey_pca$scores[,2],survey_pca$scores[,3], cex=1.5,
                    xlab="second principal component 32% variance",
                    ylab="third principal component 9% variance")

pca1_plot2_1 <-plot(survey_pca$scores[,1],survey_pca$scores[,3], cex=1.5,
                    xlab="first principal component 54% variance",
                    ylab="third principal component 9% variance")

# To help us see what's going on we'll use different colors to identify
# the fields from the different regions.

pca1_plot2_1N <-plot(survey_pca$scores[1:17,1],survey_pca$scores[1:17,2], cex=1.5, pch=19,
                     xlab="first principal component 54% variance",
                     ylab="second principal component 32% variance",
                     xlim=c(-25,10), ylim=c(-15,15))
  points(survey_pca$scores[18:31,1],survey_pca$scores[18:31,2], cex=1.5, pch=15, col='red')
  points(survey_pca$scores[32:41,1], survey_pca$scores[32:41,2], cex=1.5, pch=17, col='blue')

pca1_plot2_1N <-plot(survey_pca$scores[1:17,2],survey_pca$scores[1:17,3], cex=1.5, pch=19,
                     xlab="second principal component 32% variance",
                     ylab="second principal component 9% variance",
                     xlim=c(-25,10), ylim=c(-15,15))
points(survey_pca$scores[18:31,2],survey_pca$scores[18:31,3], cex=1.5, pch=15, col='red')
points(survey_pca$scores[32:41,2], survey_pca$scores[32:41,3], cex=1.5, pch=17, col='blue')

pca1_plot2_1N <-plot(survey_pca$scores[1:17,1],survey_pca$scores[1:17,3], cex=1.5, pch=19,
                     xlab="first principal component 54% variance",
                     ylab="third principal component 9% variance",
                     xlim=c(-25,10), ylim=c(-15,15))
points(survey_pca$scores[18:31,1],survey_pca$scores[18:31,3], cex=1.5, pch=15, col='red')
points(survey_pca$scores[32:41,1], survey_pca$scores[32:41,3], cex=1.5, pch=17, col='blue')

# Since we can see from the loadings that the first 3 components a really only looking at
# diferences based on the soil-borne diseases, and since the 4th component is >0 let's
# look at what we get by plotting 4 against 3.  Note that we reduce the scale of the 
# axes because these components have less variance so need a smaller absolute scale.

pca1_plot2_1N <-plot(survey_pca$scores[1:17,3],survey_pca$scores[1:17,4], cex=1.5, pch=19,
                     xlab="third principal component 9% variance",
                     ylab="fourth principal component 3% variance",
                     xlim=c(-5,10), ylim=c(-5,10))
points(survey_pca$scores[18:31,3],survey_pca$scores[18:31,4], cex=1.5, pch=15, col='red')
points(survey_pca$scores[32:41,3], survey_pca$scores[32:41,4], cex=1.5, pch=17, col='blue')

# The plot shows that the fourth component is an east-west split (probably related to rainfall
# and cropping density in the rotation, the west being associated with wet weather diseases such
# as Septoria and ear blights and soil-borne diseases (eyespot and sharp eyespot being associated
# with the east where second wheat crops are not unusual and wheat is more frequent generally in the
# rotation.

# With the results of the initial PCA in hand we can see it might be worth looking at an analysis
# of the correlation matrix of the data rather than the data matrix directly.  This is an 
# approach that can help in seeing the fine structure in a data set that might otherwise be
# obscured by differences in absolute magnitude among the variables.  In this case the root
# diseases were scored on a pseudo-percentage scale from 0 to 100 while the foliar and head
# diseases were scored on a 0-9 categorical scale.  There is an option in princomp to switch
# to the correlation matrix, but we first have to dump the rust variables from the data because
# they are all 0 and the correlation method does not work with variables that have 0 variance.

# The 3 rust variables are in columns 7,8 and 9 of the data frame. The following line of cade
# makes a new data frame by dropping those columns from the original one.

PCA1_cordata <-PCA1data[c(-7,-8,-9)]
head(PCA1_cordata)

survey_corpca1 <-princomp(PCA1_cordata[3:14], cor=TRUE)
summary_corpca1 <-summary(survey_corpca1)
summary_corpca1

corpca1_scree <-plot(survey_corpca1)

# This time we can see that the variance is much more evenly spread over the components

loadings_corpca1 <-survey_corpca1$loadings
loadings_corpca1

# The loadings show that the original variables contribute more equally to the components

corpca1_plot2_1 <-plot(survey_corpca1$scores[,1],survey_corpca1$scores[,2], cex=1.5,
                    xlab="first principal component 26% variance",
                    ylab="second principal component 17% variance")

corpca1_plot3_2 <-plot(survey_corpca1$scores[,2],survey_corpca1$scores[,3], cex=1.5,
                    xlab="second principal component 17% variance",
                    ylab="third principal component 14% variance")


pca1_plot2_1N <-plot(survey_corpca1$scores[1:17,1],survey_corpca1$scores[1:17,2], cex=1.5, pch=19,
                     xlab="first principal component 26% variance",
                     ylab="second principal component 17% variance",
                     xlim=c(-4,6), ylim=c(-3,4))
    points(survey_corpca1$scores[18:31,1],survey_corpca1$scores[18:31,2], cex=1.5, pch=15, col='red')
    points(survey_corpca1$scores[32:41,1], survey_corpca1$scores[32:41,2], cex=1.5, pch=17, col='blue')

pca1_plot2_1N <-plot(survey_pca$scores[1:17,2],survey_pca$scores[1:17,3], cex=1.5, pch=19,
                     xlab="second principal component 17% variance",
                     ylab="third principal component 14% variance",
                     xlim=c(-3,4), ylim=c(-4,4))
    points(survey_corpca1$scores[18:31,2], survey_corpca1$scores[18:31,3], cex=1.5, pch=15, col='red')
    points(survey_corpca1$scores[32:41,2], survey_corpca1$scores[32:41,3], cex=1.5, pch=17, col='blue')

# Make the PCA output match the geographic orientation of the regions

pca1_plot2_1N <-plot(survey_corpca1$scores[1:17,2],survey_corpca1$scores[1:17,1], cex=1.5, pch=19,
                     xlab="second principal component 17% variance",
                     ylab="first principal component 26% variance",
                     xlim=c(-3,4), ylim=c(-4,6))
points(survey_corpca1$scores[18:31,2],survey_corpca1$scores[18:31,1], cex=1.5, pch=15, col='red')
points(survey_corpca1$scores[32:41,2], survey_corpca1$scores[32:41,1], cex=1.5, pch=17, col='blue')

# Final piece for this section. Both the scores for the objects and the loadings for the variables
# are related to the principal components.  In 1971 Rueben Gabriel worked out an elegant way to 
# show both the scores and the loadings on the same graph called a biplot.  R includes a plot
# method that accepts a PCA structure as its input and produces a biplot automatically.

corpca1_bplot <-biplot(survey_corpca1, col=c('black','blue'))

# Vectors which point in similar directions are positively correlated.  Points which lie in a zone
# of the graph in the direction of a vector tend to have high values of the variable associated
# with the vector.