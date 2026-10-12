source("Pilot_simulation_1_function.R")
library(withr,lib.loc ="/home/bcanourrego/work/paper2/Rpackages")
library(farver,lib.loc ="/home/bcanourrego/work/paper2/Rpackages")
library(RColorBrewer,lib.loc ="/home/bcanourrego/work/paper2/Rpackages")
library(caret,lib.loc ="/home/bcanourrego/work/paper2/Rpackages")

# #Read data ------------------------------------------------------------------------
# data = read.csv("Alzheimer_data.csv")
# # WE have only numeric covariates
# 
# #NA
# #table(apply(data,2, function(x) mean(is.na(x))))
# # 0 NA in the entire data set
# 
# 
# # Normalization of the data
# # Removing ID and Response variables
# data_values = data[,-c(1,452)]
# 
# 
# # Synthetic ---------------------------------------------------------------
# 
# Sigma = lapply(split(data_values,data$class), cov)
# Mu = lapply(split(data_values,data$class),function(x) apply(x,2,mean)) 
# 
# library(MASS)
# nh=50000
# np=50000
# set.seed(123)
# data1 = mvrnorm(n=nh, mu=Mu[["H"]],Sigma = Sigma[["H"]])
# set.seed(123)
# data2 = mvrnorm(n=np, mu=Mu[["P"]],Sigma = Sigma[["P"]])
# class_label = rep(c("H","P"),each=50000 )
# data3 = rbind(data1,data2)
# 
# remove(data1,data2)
# 
# 
# #library(caret)
# set.seed(123)
# inTrain <- createDataPartition(
#   y = class_label,  ## the outcome data are needed
#   p = .9,   ## The percentage of data in the
#   ## training set
#   list = FALSE
# )
# 
# 
# training <- data3[ inTrain,]
# testing  <- data3[-inTrain,]
# 
# preProc = preProcess(training,method=c("center","scale") )
# training <- predict(preProc, training)
# testing     <- predict(preProc, testing)
# 
# 
# rownames(training) = 1:nrow(training)
# rownames(testing) = 1:nrow(testing)
# training = as.data.frame(training)
# testing = as.data.frame(testing)
# #training = training[,1:400]
# #testing = testing[,1:400]
# #summary(training)
# #summary(testing)
# # Application of my method
# #g=1
# 
# 
# save.image("/home/bcanourrego/work/paper2/Alzheimer_augmented.RData")
scenario_i = commandArgs(TRUE)
scenario_i = as.numeric(scenario_i)


load("/home/bcanourrego/work/paper2/Alzheimer_augmented.RData")
kmax= round(0.1*nrow(training))#9000
n = nrow(training)
m =nrow(testing)
options(digits = 16)

remove(data3)

#head(training[,1:20])
#head(testing[,1:20])

# mem_knn = mem_change({ 
#   time1 = system.time({results1 = knn.st(query_set = testing, training_set =training ,g=g, kmax=kmax)})#,id="id")})
# })
# print("Done with STNNfr")
# 
# mem_brute=mem_change({ 
#   time2 = system.time({results2 = archaic_knn(query=testing,data=training,k=kmax)})
# })
# 
# print("Done with Brute-force")
# # Application of Brute force
# time1
# time2
# 
# # Proportion in CPU time
# time2[2]/time1[2]
# # Proportion in Elapsed time
# time2[3]/time1[3]
# # Proportion of memory used
# mem_knn/mem_brute 
# 
# results_data_application = list(time1,time2,mem_knn,mem_brute)
# 
# save(results_data_application,file= paste0(getwd(),"/Data_app_results.RData") )
# 
# 
# load("Data_app_results.RData")
# 
# 
# 
# time1= results_data_application[[1]]
# time2= results_data_application[[2]]
# mem_knn= results_data_application[[3]]
# mem_brute= results_data_application[[4]]
# 
# 
# 
# time2
# mem_brute
# 
# time1
# mem_knn
# # Proportion in CPU time
# time2[2]/time1[2]
# # Proportion in Elapsed time
# time2[3]/time1[3]
# # Proportion of memory used
# 1-mem_knn/mem_brute 
# 
# 
# # get_knn =  function(x) do.call(rbind,(x$knn[as.character(1:(length(x$knn))) ])) 
# # knn1= get_knn(results1)
# # 
# # 
# # get_knn_arch = function(x) t(sapply(x[1:(length(x)) ], function(x) x$knn))
# # knn2= get_knn_arch(results2)
# # 
# # mean(knn1==knn2)
# 
# 
# 
# 



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
ri_scenarios = list(scenario_1 = list(k=kmax,type= "naive",naive_mult=1),
                    scenario_2 = list(k=kmax,type="z_k", z_k_p=0.7),
                    scenario_3 = list(k=kmax,type="z",z_p=kmax/n),
                    #scenario_4 = list(k=k,type="normal",z_normal_p=k/n),
                    scenario_4 = list(k=kmax,type="spherical",z_spherical_p=kmax/n),
                    scenario_5 = list(k=kmax,type="spherical_k",z_spherical_k_p=0.7)
                    
)


# Runnning the simulation
knn_st_results = list()
mem_knn = list()
time_knn = list()




scenario = ri_scenarios[[scenario_i]]
#for(scenario in ri_scenarios){
  # n= nrow(training_set)
  # p=ncol(training_set)
  # m =nrow(query_set)
  print(counter)
  res_name=paste0("data_application_",counter)
  #mem_knn_aux = mem_change({ 
  #Rprof("memory.out", memory.profiling = TRUE)
  mem_knn_aux <- profmem({
    
    time_knn_aux = system.time({
      knn_st_results[[res_name]] = knn.st.updated(training_set=training, 
                                                  query_set =testing,
                                                  k=kmax,
                                                  type = scenario$type,
                                                  naive_mult=scenario$naive_mult,
                                                  z_p= scenario$z_p,
                                                  z_k_p= scenario$z_k_p,
                                                  z_normal_p =  scenario$z_normal_p,
                                                  z_spherical_p = scenario$z_spherical_p,
                                                  z_spherical_k_p =scenario$z_spherical_k_p
      )
      
      # training_set=training; 
      # query_set =testing;
      # k=kmax;
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
  
  
  sum_h_ij = lapply(knn_st_results[[res_name]]$results$k_prime,h_function,k=kmax,alpha=1,beta=2)
  
  # Number of rows of query set
  #m = nrow(query_set)
  # right side is the cost of those who had zero neighbors found
  # COST_naive[counter] = sum(h_di)/m + (n-length(k_prime))* alpha*(k/n) /m
  COST[counter] = sum(unlist(sum_h_ij))/m
  counter = counter+1
  
  #Sys.sleep(2)
  #gc()



save(knn_st_results,file= paste0(getwd(),"/result_datapplication_knn_",scenario_i,".RData") )
save(mem_knn,file= paste0(getwd(),"/result_datapplication_memory_",scenario_i,".RData")  )
save(time_knn,file= paste0(getwd(),"/result_datapplication_time_",scenario_i,".RData")  )
save(COST,file= paste0(getwd(),"/result_datapplication_COST_",scenario_i,".RData")  )



# # Proportion in CPU time
# time2[2]/time1[2]
# # Proportion in Elapsed time
# time2[3]/time1[3]
# # Proportion of memory used
# mem_knn/mem_brute