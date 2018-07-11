# Correspondence analysis in R
# Examples are analyses of data in Table 6 in Savary et al 1994:
# Agricultural Systems, 46: 385-408

# There are several different CA routines in R. We will use the CA
# package by Nenadic and Greenacre.  Greenacre is one of *the*
# leaders in the development of CA.

# You will need to install the package ca.
# Load the package using the usual library() command

library(ca)

# The data are two contingency tables which show the
# joint distribution of rice fields over categories
# of yield and (a) cropping practices, and (b) pest
# profiles.

Y_PR_data <-read.csv(file.choose(), head=FALSE)

# make some labels for the rows and columns of the
# data matrix.
PR <-c("PR1","PR2","PR3","PR4","PR5","PR6","PR7")
Y <- c("Y1","Y2","Y3","Y4","Y5","Y6")
rownames(Y_PR_data) <-PR
colnames(Y_PR_data) <-Y

# Now perform Correspondence analysis

Y_PR_ca_mod <-ca(Y_PR_data)
print(Y_PR_ca_mod)
summary(Y_PR_ca_mod)
plot(Y_PR_ca_mod)
plot(Y_PR_ca_mod, mass=TRUE, contrib="absolute", map="rowgreen",
     arrows=c(FALSE,TRUE))

#Now repeat the analysis with the Yield by pest profile data

Y_PE_data <-read.csv(file.choose(), head=FALSE)

# make some labels for the rows and columns of the
# data matrix.
PE <-c("PE1","PE2","PE3","PE4","PE5","PE6","PE7")
Y <- c("Y1","Y2","Y3","Y4","Y5","Y6")
rownames(Y_PE_data) <-PE
colnames(Y_PE_data) <-Y

# Now perform Correspondence analysis

Y_PE_ca_mod <-ca(Y_PE_data)
print(Y_PE_ca_mod)
summary(Y_PE_ca_mod)
plot(Y_PE_ca_mod)
plot(Y_PE_ca_mod, mass=TRUE, contrib="absolute", map="rowgreen",
     arrows=c(FALSE,TRUE))

