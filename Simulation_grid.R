
source("Pilot_simulation_1_function.R")
# Creation of the GRID ----------------------------------------------------

# So far I considered multivariate standard normal, unit radius circular uniform data, and
# multivariate centered t distribution 
distribution = c(0,1,2) #0 for normal, 1 for t distribution, 2 for uniform, 
# As starting point, I have considered up 2000 observation
n_sample = c(100,1000,10000,100000)#c(10,20,50,100,500,1000,1500,2000)#5000#,25000,100000)
# As starting point, I have considered up 100 observation
# and always m<=n for the sake of simplicity although there is no real restriction

m_sample = c(50,100,200)#,500,1000,1500,5000,25000,100000)
# Due to the limitation computing volumen of high dimensions I restrained the maximum amout to be 250
# It will be good to check what is the maximum p value before starting to get infinite numbers again
# So far I think p=500 is the max what it can take, due to the beta_inc function
# I can further inspect this function to expand this number
p_dim = c(20,50,100,250)
# Due to some high dimensional data, This will add a lot memory usage when increased a lot
# Note that k<=n
#k_neighbors = c(10,20) 
# Arbitrary number of ringed groups. Minimum considered is 1
#g_rings = c(1)

reps= 25


simulation_grid = matrix(NA,ncol = 5,nrow=0)
colnames(simulation_grid) = c('distribution',
                              "n",
                              "m",
                              "p",
                              "k")#,
                              #"g")#,
#"tau")

# HOW TO CONSIDER SPARCITY? BIG radius in Uniform/ High variances for normal?

#length(distribution)*length(n_sample)*length(m_sample)*5*5*length(g_rings)*reps


# Creation of the simulation grid with all the previously mentioned conditions
#counter = reps
for(distri in distribution){
  for(n in n_sample ){
    for(m in m_sample[m_sample<=n] ){
      
      #p_aux = 100
      #p_aux = round(seq(1,0.5*n,length.out=5) )
      for(p in p_dim){#p_aux[p_aux>=2]){
        #k_aux = k_neighbors
        k_aux = round(c(0.1*n, 0.2*n,0.3*n,0.4*n) )
        for(k in k_aux){
          #for(g in g_rings){
            #for(rep in 1:reps){
            #print(counter)
            simulation_grid = rbind(simulation_grid,
                                    matrix(c(distri,n,m,p,k),nrow=reps,ncol=5,byrow=T)  )#g),nrow=reps,ncol=6,byrow=T)  )
            #counter = counter +reps
            #}
            
            
            
          #}
          
        }
      }
    }
  }
}

#simulation_grid = as.data.frame(simulation_grid)
#simulation_grid = subset(simulation_grid,p>1)

save(simulation_grid,file="Simulation Grid.Rdata")

simulation_grid2= unique(simulation_grid[,1:5])
simulation_grid2 = simulation_grid2[rep(seq_len(nrow(simulation_grid2)), each = reps), ]
save(simulation_grid2,file="Simulation Grid2.Rdata")

#simulation_grid[as.numeric(simulation_grid$k)>as.numeric(simulation_grid$n),]

# # For data creation we do not need column 5: K= Number of neighbors
# # nor column 6: g= Number of rings
simulation_grid_for_data =unique(simulation_grid[,-c(5)])


# Creating all possible data sets, and once that is obtained they can be used in the
# simulation stage


simulation_data_creation= function(x){
  # vector of parameters
  distri=x[[1]]
  n=x[[2]]
  m=x[[3]]
  p=x[[4]]
  
  #Normal case
  if(distri == 0){
    
    # Create two independent multivariate normal samples centered at 0
    set.seed(111)
    mu = rep(0,p)
    # Covariance matrix of the data
    Sigma = diag(1,nrow= p)
    training_set = mvrnorm(n=n, mu=mu,Sigma = Sigma)
    colnames(training_set) = paste0("x",1:p)
    training_set = as.data.frame(training_set)
    
    
    set.seed(222)
    mu = rep(0,p)
    # Covariance matrix of the data
    Sigma = diag(1,nrow= p)
    query_set = mvrnorm(n=m, mu=mu,Sigma = Sigma)
    colnames(query_set) = paste0("x",1:p)
    query_set = as.data.frame(query_set)
    
  }
  # T distribution case
  # I need to use the  rmvt(n, sigma = diag(2), df = 1, delta = rep(0, nrow(sigma)),
  # type = c("shifted", "Kshirsagar"), ...)
  # and figure out what are the degrees of freedom I need
  # rmvt(n, mu=m, sigma=S*(D-2)/D, df=5 to 15) given we want heavy tail but not that much since
  # spherical already fills that need
  else if(distri == 1){
    
    # Create two independent multivariate centered T samples with mean 0 
    set.seed(333)
    mu = rep(0,p)
    # Covariance matrix of the data
    Sigma = diag(1,nrow= p)
    D = 10 # degrees of freedom
    training_set = rmvt(n, sigma=Sigma*(D-2)/D, df=D) + mu#df=10 by now
    colnames(training_set) = paste0("x",1:p)
    training_set = as.data.frame(training_set)
    
    
    set.seed(444)
    mu = rep(0,p)
    # Covariance matrix of the data
    Sigma = diag(1,nrow= p)
    D = 10 # degrees of freedom
    query_set = rmvt(m, sigma=Sigma*(D-2)/D, df=D) + mu#df=10 by now
    colnames(query_set) = paste0("x",1:p)
    query_set = as.data.frame(query_set)
  }
  
  
  #Uniform case
  else if(distri == 2){
    set.seed(555)
    center=rep(0,p)
    radius= 1
    training_set <- GenerateSpherePoints(nrPoints=n,nrDim=p,center=center,r=radius)
    colnames(training_set) = paste0("x",1:p)
    training_set = as.data.frame(training_set)
    
    
    set.seed(666)
    center=rep(0,p)
    radius= 1
    query_set <- GenerateSpherePoints(nrPoints=m,nrDim=p,center=center,r=radius)
    colnames(query_set) = paste0("x",1:p)
    query_set = as.data.frame(query_set)
    
    
  }
  
  
  
  return(list(training_set=training_set,query_set = query_set))
  
  
}



#Creating the data sets required for the simulations
#data_list = list()
temp_total = nrow(simulation_grid_for_data)
for(i in 90:temp_total){
  
  print(paste0(i," out of ",temp_total))
  distri=simulation_grid_for_data[i,1]
  n=simulation_grid_for_data[i,2]
  m = simulation_grid_for_data[i,3]
  p=simulation_grid_for_data[i,4]
  name = paste0(getwd(),"/data/data",distri,"_",n,"_",m,"_",p,".RData")
  data=simulation_data_creation(simulation_grid_for_data[i,])
  save(data,file = name)
  
  #Create two data sets with the ith set of parameters of the simulation grid
  #data_list[[as.character(distri) ]][[as.character(n)]][[as.character(m)]][[as.character(p)]]=
  #  simulation_data_creation(simulation_grid_for_data[i,])
}
