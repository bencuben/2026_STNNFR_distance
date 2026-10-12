knn_list =list()
time_knn_list=list()
mem_knn_list=list()
COST_vect = rep(NA, 5)
for(i in 1:5){
  load(paste0("Result data application 1//result_datapplication_time_",i,".RData"))
  load(paste0("Result data application 1//result_datapplication_memory_",i,".RData"))
  load(paste0("Result data application 1//result_datapplication_knn_",i,".RData"))
  load(paste0("Result data application 1//result_datapplication_COST_",i,".RData"))
  
  knn_list[[i]] = knn_st_results[[1]]
  time_knn_list[[i]] = time_knn[[1]]
  mem_knn_list[[i]] = mem_knn[[1]]
  COST_vect[i]=COST
  
}


# Elapsed time ------------------------------------------------------------

# Naive 
time_knn_list[[1]][3]
# Simulation k 
time_knn_list[[2]][3]
# Simulation
time_knn_list[[3]][3]
# Spherical
time_knn_list[[4]][3]
# Spherical k
time_knn_list[[5]][3]
#NAive seem the best


# CPU time ----------------------------------------------------------------


# Naive 
time_knn_list[[1]][2]
# Simulation k 
time_knn_list[[2]][2]
# Simulation
time_knn_list[[3]][2]
# Spherical
time_knn_list[[4]][2]
# Spherical k
time_knn_list[[5]][2]



# Memory allocation -------------------------------------------------------

# Naive 
mem_knn_list[[1]]
# Simulation k 
mem_knn_list[[2]]
# Simulation
mem_knn_list[[3]]
# Spherical
mem_knn_list[[4]]
# Spherical k
mem_knn_list[[5]]


# COST --------------------------------------------------------------------

COST_vect

