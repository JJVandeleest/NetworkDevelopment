setwd("C:/Users/Jessica/Box/McCowan Lab/Projects_Funded/SSD/SSD_Analyses_Current/Pooled_Infant_Networks/Edgelists")

ch1_0.5yr = read.csv("CH1_Ego_0.5yr.csv", stringsAsFactors = F)

ch1_0.5yr = ch1_0.5yr[,c(2,5)]

##NOTE function only works with variable names Initiator and Recipient right now
##  The columns in this edgelist DO NOT correspond this way, they refer to focal or other ID
names(ch1_0.5yr) = c("Initiator", "Recipient")

adj_matrix = EL_to_matrix(ch1_0.5yr, weightedEL = F, directed = F)



library(sna)

ch1_0.5yr_net = network(adj_matrix, directed = F)

t1_density = gden(ch1_0.5yr_net, mode = "graph", ignore.eval = T)

t1_deg_centralization = centralization(ch1_0.5yr_net, degree, mode = "graph", ignore.eval = T)

t1_clustering = gtrans(ch1_0.5yr_net, mode = "graph", measure = "strong")

t1_clustering/t1_density


##Now subsample graph to match # of nodes at t2 to see if reduction in sample can explain changes.

#check how many nodes in next time point
time2_EL = read.csv("CH1_Ego_1.0yr.csv", stringsAsFactors = F)
time2_EL  = time2_EL[,c(2,5)]
names(time2_EL) = c("Initiator", "Recipient")

time2_matrix = EL_to_matrix(time2_EL, weightedEL = F, directed = F)

ch1_1.0yr_net = network(time2_matrix, directed = F)

t2_density = gden(ch1_1.0yr_net, mode = "graph", ignore.eval = T)

t2_deg_centralization = centralization(ch1_1.0yr_net, degree, mode = "graph", ignore.eval = T)

t2_clustering = gtrans(ch1_1.0yr_net, mode = "graph", measure = "strong")


#subsample time 1 to size of time 2.
t1_subjects = rownames(adj_matrix)

density_list = vector()
deg_cent_list = vector()
clustering_list = vector()
clust_den_list = vector()

for (i in 1:1000){

  subsample = sort(sample(t1_subjects, 28))

  adj_matrix_subsample = adj_matrix[which(rownames(adj_matrix) %in% subsample), which(colnames(adj_matrix) %in% subsample)]
  
  ch1_0.5yr_net = network(adj_matrix_subsample, directed = F)
  
  density_sub = gden(ch1_0.5yr_net, mode = "graph", ignore.eval = T)
  density_list = c(density_list, density_sub)
  
  deg_centralization_sub = centralization(ch1_0.5yr_net, degree, mode = "graph", ignore.eval = T)
  deg_cent_list = c(deg_cent_list, deg_centralization_sub)
  
  clustering_sub = gtrans(ch1_0.5yr_net, mode = "graph", measure = "strong")
  clustering_list = c(clustering_list, clustering_sub)
  
  clust_den_list = c(clust_den_list, (clustering_sub/density_sub))


}

#drop in clustering greater than would be expected by change in network size from 6 mo to 1 year
t2_clustering/t2_density
mean(clust_den_list) - (2 * sd(clust_den_list))

#increast in density higher than would be expected by change in network size from 6 mo to 1 year
t2_density
mean(density_list) + (2 * sd(density_list))

# drop in deg centralization is similar to what is expected with simply a drop in network size
t2_deg_centralization
mean(deg_cent_list) - (2 * sd(deg_cent_list))

