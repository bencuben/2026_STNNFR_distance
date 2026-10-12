source("Pilot_simulation_1_function.R")


scenario_i = commandArgs(TRUE)
scenario_i = as.numeric(scenario_i)

print(paste0("Scenario Knn = ",scenario_i))
load(file="Simulation Grid.Rdata")


distri=simulation_grid[scenario_i,1]
n=simulation_grid[scenario_i,2]
m = simulation_grid[scenario_i,3]
p=simulation_grid[scenario_i,4]
k= simulation_grid[scenario_i,5]
#g= simulation_grid[scenario_i,6]
name = paste0(getwd(),"/data/data",distri,"_",n,"_",m,"_",p,".RData")
load(file = name)


# Simulation --------------------------------------------------------------
#install.packages("pryr")





COST= c()
counter= 1
# ri_scenarios = list(scenario_1 = list(k=k,z_k_p=NULL,naive= T,naive_mult=1),
#                     scenario_2 = list(k=k,z_k_p=NULL,naive= T,naive_mult=1.25),
#                     scenario_3 = list(k=k,z_k_p=NULL,naive= T,naive_mult=1.5),
#                     scenario_4 = list(k=k,z_k_p=0.1,naive= F,naive_mult=NULL),
#                     scenario_5 = list(k=k,z_k_p=0.3,naive= F,naive_mult=NULL),
#                     scenario_6 = list(k=k,z_k_p=0.5,naive= F,naive_mult=NULL),
#                     scenario_7 = list(k=k,z_k_p=0.7,naive= F,naive_mult=NULL),
#                     scenario_8 = list(k=k,z_k_p=0.9,naive= F,naive_mult=NULL)
#                     
# )
ri_scenarios = list(scenario_1 = list(k=k,type= "naive",naive_mult=1),
                    scenario_2 = list(k=k,type="z_k", z_k_p=0.7),
                    scenario_3 = list(k=k,type="z",z_p=k/n),
                    #scenario_4 = list(k=k,type="normal",z_normal_p=k/n),
                    scenario_4 = list(k=k,type="spherical",z_spherical_p=k/n),
                    scenario_5 = list(k=k,type="spherical_k",z_spherical_k_p=0.7)
                    
)


# Runnning the simulation
knn_st_results = list()
mem_knn = list()
time_knn = list()





for(scenario in ri_scenarios){
  # n= nrow(training_set)
  # p=ncol(training_set)
  # m =nrow(query_set)
  print(counter)
  res_name=paste0(distri,"_",n,"_",m,"_",p,"_",counter)
  #mem_knn_aux = mem_change({ 
  #Rprof("memory.out", memory.profiling = TRUE)
  mem_knn_aux <- profmem({
  
  time_knn_aux = system.time({
    knn_st_results[[res_name]] = knn.st.updated(training_set=data$training_set, 
                                            query_set =data$query_set,
                                            k=scenario$k,
                                            type = scenario$type,
                                            naive_mult=scenario$naive_mult,
                                            z_p= scenario$z_p,
                                            z_k_p= scenario$z_k_p,
                                            z_normal_p =  scenario$z_normal_p,
                                            z_spherical_p = scenario$z_spherical_p,
                                            z_spherical_k_p =scenario$z_spherical_k_p
                                            )
    
    # training_set=data$training_set;
    # query_set =data$query_set;
    # k=scenario$k;
    # type = scenario$type;
    # naive_mult=scenario$naive_mult;
    # z_p= scenario$z_p;
    # z_k_p= scenario$z_k_p;
    # z_normal_p =  scenario$z_normal_p;
    # z_spherical_p = scenario$z_spherical_p;
    # z_spherical_k_p =scenario$z_spherical_k_p

    } ) 
  
  })
  #})
  #Rprof(NULL)
  
  #mem_knn[[res_name]] = sum(summaryRprof("memory.out", memory = "both")$by.total[-1,"mem.total"])
  mem_knn[[res_name]] =  sum(mem_knn_aux$bytes,na.rm = T)/(1024^2)
  time_knn[[res_name]] = time_knn_aux
  
  
  sum_h_ij = lapply(knn_st_results[[res_name]]$results$k_prime,h_function,k=k,alpha=1,beta=2)
  
  # Number of rows of query set
  #m = nrow(query_set)
  # right side is the cost of those who had zero neighbors found
  # COST_naive[counter] = sum(h_di)/m + (n-length(k_prime))* alpha*(k/n) /m
  COST[counter] = sum(unlist(sum_h_ij))/m
  counter = counter+1
  
  Sys.sleep(2)
  #gc()
}


save(knn_st_results,file= paste0(getwd(),"/Results3/result_knn_",scenario_i,".RData") )
save(mem_knn,file= paste0(getwd(),"/Results3/result_memory_",scenario_i,".RData")  )
save(time_knn,file= paste0(getwd(),"/Results3/result_time_",scenario_i,".RData")  )
save(COST,file= paste0(getwd(),"/Results3/result_COST_",scenario_i,".RData")  )


remove(knn_st_results,mem_knn,time_knn,ri_scenarios,scenario,
       sum_h_ij)
# save(result_knn,file= paste0(getwd(),"/results/result_knn_",scenario_i,".RData") )
# save(time_knn,file= paste0(getwd(),"/results/time_knn_",scenario_i,".RData") )
# save(mem_knn,file= paste0(getwd(),"/results/mem_knn_",scenario_i,".RData") )