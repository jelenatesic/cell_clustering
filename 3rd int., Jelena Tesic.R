### 3rd INTERVIEW FOR THE JUNIOR POSITION IN BGI
### JELENA TESIC

                                          #### TEST: 
# Data in a CSV form was presented, which contained coordinates of cells (x,y), and their 
# type (sim anno)
# The purpose of this test is to:
# - cluster given data into communities, 
# - calculate number of different cells and their percentages in these mixtures,
# - chose the best model and change parameters in it, and describe how they effect clustering
# - determine homogeneity of every cell community (cluster)

## Importing data
data <- read.csv("E16.5_E1S3_cell_bin_whole_brain_noborderct.csv", header = TRUE)
View(data)
str(data)

# Visualizing  data
library(ggplot2)
library(tidyr)
library(dplyr)

ggplot(data, aes(x = x, y = y)) +
  geom_jitter(position = "jitter", alpha = 0.6, aes(color = sim.anno))

plot(data$x, data$y, col = data$sim.anno)
legend("topright", legend = unique(data$sim.anno))

## Preprocessing data

any(data[data[2:3] == c('NA','N/A', '<NA>')]) # checking for NAs

# **Scaling coordinates
#* We are given a coordinates of each cell and its type
#* Scaling their values, we are making sure there wont be biased clustering and that the data 
#* will be better interpreted 

data_scal <- data.frame(scale(data[2:3]))
head(data_scal)

# Visualizing 

ggplot(data_scal, aes(x = x, y = y)) +
  geom_jitter(position = "jitter", alpha = 0.6, aes(color = data$sim.anno), shape = 20)

# **Encoding type of cells
#* We know that the spatial communities differ based on the percentage of cells
# that means that we should perhaps take into consideration the type of cells that are in 
# the area

paste('Number of different cell types: ', 
      length(unique(data$sim.anno))) # number of cell types

# _________ Frequency encoding
data_freq <- data.frame(data_scal, sim.anno = data$sim.anno)
View(data_freq)

# Calculating frequency of every cell type
freq <- table(data$sim.anno)
data_freq$sim.anno <- match(data_freq$sim.anno, names(freq)) # matching freq. with cell names

head(data_freq)


# _________ One hot
# Making separate columns for every cell type

install.packages('mltools')
library(mltools)
library(data.table)

data_one_hot <- one_hot(as.data.table(as.factor(data$sim.anno)))
head(data_one_hot, 3L)

new_data <- data.frame(data_scal, data_one_hot )
View(new_data)


## ***1. CLUSTERING
# Clustering is an unsupervised method for grouping our data
# Depending on a algorithm used, there are various types of methods

#                 ===== K-means =====
# K means is a clustering method which groups points into k clusters based on a closness 
# to a mean of clusters

# Elbow plot- to determine the optimal number of clusters 
set.seed(123)

wss <- 0
for (i in 1:20){
  k_means <- kmeans(new_data, 
                    centers = i,
                    nstart = 25)
  wss[i] <- k_means$tot.withinss # within sum of squares 
}

plot(1:20, wss, type = 'b') # from this plot we see that the 3 might be 
                            # optimal number of clusters


# But from the example of this test, we can see that there are 16 clusters
# That makes us think kmeans wont be the best method for clustering this data.

gc() # garbage collection 

# *Coordinates as input
set.seed(123)
k_means_simple <- kmeans(data_scal, # taking only coordinates
                     centers = 16,
                     iter.max = 20,
                     nstart = 4)

# Doing only clustering with coordinates gave Quick-Transfer warnings (36 of them)
# It could be due to the lack of memory to R process  
# A recommended in R documentation, decreasing nstart gave less warnings- with  
# nstart = 4 was highest fr which value we didn't get any warnings
# Additional warning was that the algorithm did not converge in 10 iterations,  
# so increase of the iter.max parameter was needed

# *One hot encoding 
set.seed(123)
k_means_one_hot <- kmeans(new_data, # coordinates with one hot encoding
                         centers = 16,
                         # iter.max = 20,
                         nstart = 25)
ggplot()+
  geom_point(data = new_data, 
             mapping = aes(x = x, y = y, colour = factor(k_means_one_hot$cluster)), shape = 20) 

# *Frequency encoding
set.seed(123)
k_means_freq <- kmeans(data_freq, # coordinates with freq. encoding
                     centers = 16,
                    # iter.max = 10,
                     nstart = 100)

cluster = k_means_freq$cluster
ggplot()+
  geom_point(data = data_freq, 
             mapping = aes(x = x, y = y, colour = factor(cluster)), shape = 20) 


#                      ==== DBSCAN ====
# *** Density-based Spatial Clustering of Applications with Noise (DBSCAN)
# 1. Estimates the density around each data point by counting the number of points in a 
# user-specified eps-neighborhood and applies a used-specified minPts thresholds to 
# identify core, border and noise points.
# 2. Core points are joined into a cluster if they are density-reachable (i.e., there is a 
# chain of core points where one falls inside the eps-neighborhood of the next).
# 3. Border points are assigned to clusters. The algorithm needs parameters eps (the radius of 
# the epsilon neighborhood) and minPts (the density threshold).

## Cluster with the chosen parameters
# If \(\epsilon\) is too small, sparser clusters will be defined as noise. If \(\epsilon\) is 
# too large, denser clusters may be merged together. This implies that, if there are clusters 
# with different local densities, then a single \(\epsilon\) value may not suffice.

library(dbscan)
# minPts = ncol + 1 as from the documentation 


# * Coordinates as input

# Method for determining eps 
kNNdistplot(data_scal, minPts = 3) # for 2 input columns 
abline( h = 0.02, col = 'red') # it doesn't look promising
# FOR START: choosing eps = 0.1 and minPts = 3

res_simple <- dbscan(data_scal, eps = 0.1, minPts = 2)
res_simple

ggplot(new_data, aes(x = x, y = y))+ 
  geom_point(aes(color = factor(res_simple$cluster)), shape = 20)

# * One hot encoding as input

kNNdistplot(new_data, minPts = 33) # for 32 input columns 
abline( h = 0.3, col = 'red') 
# FOR START: choosing eps = 0.3 and minPts = 33

res_one_hot <- dbscan(new_data, eps = 1.2, minPts = 33) #changed parameters
res_one_hot
ggplot(new_data, aes(x = x, y = y))+ 
  geom_point(aes(color = factor(res_one_hot$cluster)), shape = 20)

# * Frequency encoding as input
kNNdistplot(data_freq, minPts = 4) # for 32 input columns 
abline( h = 0.15, col = 'red') 
# FOR START: choosing eps = 0.15 and minPts = 4

res_freq <- dbscan(data_freq, eps = 1.0, minPts = 10)
res_freq

ggplot(new_data, aes(x = x, y = y))+ 
  geom_point(aes(color = factor(res_freq$cluster)), shape = 20)



#                   ==== Shared Nearest Neighbor (SNN) ====
# *** Shared Nearest Neighbor (SNN) is a solution to clustering high-dimensional data with the ability 
# to find clusters of varying density. SNN assigns objects to a cluster, which share a large
# number of their nearest neighbors.

# 1.Constructs a shared nearest neighbor graph for a given k. The edge weights are the number of shared 
# k nearest neighbors (in the range of [0,k]).
# 2. Find each points SNN density, i.e., the number of points which have a similarity of eps or greater.
# 3. Find the core points, i.e., all points that have an SNN density greater than MinPts.
# 4. Form clusters from the core points and assign border points (i.e., non-core points which share at 
# least eps neighbors with a core point).

#library(dbscan)

# *Coordinates as input

set.seed(123)
snn_simple <- sNNclust(data_scal, 
               k = 50,      # neighborhood size
               eps = 40,     # num. of neighbors they share
               minPts = 22)  # minimum num. of points share at least eps nearest neighbors 
                             # for a point to be considered a core points
# k = 50, eps = 40, minPts = 22--- 17 clusters
paste('Number of clusters:',length(unique(snn_simple$cluster)))


ggplot(data_scal, aes(x = x, y = y)) +
  geom_jitter(position = "jitter", alpha = 0.5, aes(color = factor(snn_simple$cluster)), shape = 20)


# *One hot encodings as input

set.seed(123)
snn_one_hot <- sNNclust(new_data, 
               k = 130,      
               eps = 15,     
               minPts = 10)
paste('Number of clusters:',length(unique(snn_one_hot$cluster)))
# Decrease in eps and minPts will lead to decrease in number of clusters
# Increase in k will also lead to decrease of clusters

ggplot(new_data, aes(x = x, y = y)) +
  geom_jitter(position = "jitter", alpha = 0.5, aes(color = factor(snn_one_hot$cluster)), shape = 20)


# *Frequency encodings as input

set.seed(123)
snn_freq <- sNNclust(data_freq, 
                        k = 145,      
                        eps = 8,     
                        minPts = 8)
paste('Number of clusters:',length(unique(snn_freq$cluster)))

ggplot(data_freq, aes(x = x, y = y)) +
  geom_jitter(position = "jitter", alpha = 0.5, aes(color = factor(snn_freq$cluster)), shape = 20)

# plot(data_scal, col = snn_simple$cluster+2L, cex = 0.5, pch = 20)
# legend(x = "topright", legend = unique(snn_simple$cluster), 
#        fill = 1:length(unique(snn_simple$cluster)))



## ======= Deciding on one clustering model ========
# from previous work and visualization looks like it should be *k-means with frequency encoding* 
# Reasons: - kmeans gave the least amount of variation when making a model
#          - when using frequency encoding as input, best results were obtained 

new_data_cluster <- data
new_data_cluster$cluster <- as.character(k_means_freq$cluster)

View(new_data_cluster)

## ***2. Calculate for each community the mixture (count and percentage)  
## of cell types that are present in it

# Number of data frames we need to create
num_data_frames <- length(unique(new_data_cluster$cluster))

# List to store the data frames
data_frame_list <- list()

# Create data frames in a loop and store them in the list
for (i in 1:num_data_frames) {
  
  # Extracting data for every cluster i
  d <- new_data_cluster %>%
    filter(cluster == as.character (i) )
  
  # Calculating number of every cell type in the extracted community d
  df <- as.data.frame(table(d$sim.anno)) 
  
  # Calculating percentages for each cell type in the extracted community 
  df$percentage <- df$Freq/sum(df$Freq) *100
  
  
  # Creating unique data frame names
  df_name <- paste("df_", i, sep="")
  
  # Assigning the data frame to the global environment using assign() function
  assign(df_name, df, envir = .GlobalEnv)
  
  # Store the data frame in the list
  data_frame_list[[i]] <- get(df_name)
}

# Viewing a list containing data frames with counts and percentages of the cell types
View(data_frame_list)

# Viewing specific cluster
View(data_frame_list[[10]]) 

# Based on the different dimensions of data frames, it can be concluded that the obtained 
# communities are varying in their 

## ***3. Changing parameters in clustering algorithm, to see how they affect its performance 
# Parameters that could be changed are, aside from the input data, number of clusters- centers, 
# number of iterations- iter.max and number of possible solutions from which algorithm chooses 
# the best one- nstart


# *Changing nuber of clusters- centers

par(mfrow = c(2,2))
set.seed(123)
for (i in c(5, 12, 16, 20)){
  k_means_freq_centers <- kmeans(data_freq, 
                                 centers = i, # number of clusters
                                 iter.max = 120,
                                 nstart = 50 )
  plot(data_freq$x, data_freq$y, col = factor(k_means_freq_centers$cluster), pch = 20,
       main = paste('center', i), xlab = 'x', ylab = 'y' )

}
# Comment: We can see that the single cells are being visible even in less clustered cases
# This is probably due to the information on cell types that we gave to the algorithm
# With the number of clusters over 16, we are approaching a state where our algorithm can detect   
# single type of cells communities


# *Changing number of iterations- iter.max
# We will now leave 16 clusters as was done before and change the number of iterations 
# in the process of clustering data

par(mfrow = c(2,2))
set.seed(123)
for (i in c(10, 30, 80, 120)){
  k_means_iter_max <- kmeans(data_freq, 
                                 centers = 16,
                                 iter.max = i, # number of iterations
                                 nstart = 50 )
  plot(data_freq$x, data_freq$y, col = factor(k_means_iter_max$cluster), pch = 20,
       main = paste('iter.max =', i), xlab = 'x', ylab = 'y' )
  
}

# Comment: with increase of iterations, borders are becoming more clear, as it can be seen from
# the pictures. Also impact of the single cells is also decreasing with the rise of iterations, 
# which is probably connected with the increase in precision when it comes to having more tries 
# in grouping our cells

# *Changing nstart
# Increasing number of possible solutions algorithm can make so it chooses the best one

par(mfrow = c(2,2))
set.seed(123)
for (i in c(5, 25, 50, 100)){
  k_means_iter_max <- kmeans(data_freq, 
                             centers = 16,
                             iter.max = ,
                             nstart = i ) # possible polutions
  plot(data_freq$x, data_freq$y, col = factor(k_means_iter_max$cluster), pch = 20,
       main = paste('nstart =', i), xlab = 'x', ylab = 'y' )
  
}

# Comment: with increase of number of possible solutions we cannot really see any particular 
# difference but it is good to know that this one presented is the best out of 100 other 
# solutions. 

## ***4. Homogeneity
# The most popular and simple method for determining homogeneity of a certain group is
# **coefficient of variation** *(Cv), which is a statistical measure of the dispersion
# It is defined as ratio of standard deviation and the mean value of the group. 
# It is usually described in percentages, so this ratio is multiplied with 100. 
# It is talking how our data in a group is dispersed around a mean value
# If the Cv < 30%, we can say that our group is homogeneous, if its higher, than the 
# community can be described as group with high variability 

# This coefficient is can help us determine homogeneity, since the Standard Deviation, 
# by itself is telling us if the distribution i homogeneous
# The lower the standard deviation is the more homogeneous our group is


# Defining empty spaces for our data
data_number <- 0
homogenity_type <- 0
variance_coef <- 0

# Making a loop that will enter every community and check for its variation
for (i in 1:num_data_frames){
  
  data_number[i] <- i
  
  variance_coef[i] <- sd(data_frame_list[[i]]$Freq)/mean(data_frame_list[[i]]$Freq)*100
  
  if (variance_coef[i]< 30){
    homogenity_type[i] <- 'homogeneous'
    
  } else homogenity_type[i] <- 'heterogeneous'
  
  
}

homogenity_data <- data.frame(data_number, variance_coef, homogenity_type)
View(homogenity_data)

# From the obtained table, we can conclude that our communities are very heterogeneous
# which could have been concluded only by observing plot
# This means that in every extracted community, we don't have similar amount of the present 
# cell types 

# In case if we wanted to check if two groups were comming from the same population
# Homogeneity test between the groups would be uses as bertlett.test()

# As presented in study [Altschuler, S. J., & Wu, L. F. (2010). Cellular Heterogeneity: 
# Do Differences Make a Difference? Cell, 141(4), 559–563. doi:10.1016/j.cell.2010.04.033], 
# heterogeneity of subpopulations can be a challenge when it comes to 
# creating clinical models for diagnosis and treatment, where they propose the decomposition
# into smaller, more homogeneous subgroups is the best way for overcoming this problem.
# That idea was exactly the purpose of this test, which was unfortunately not achieved.
# Further improvements are being proposed, as trying different models such as Gaussian 
# Mixed Models (GMM) and/or improving input data with additional encoding types.  

