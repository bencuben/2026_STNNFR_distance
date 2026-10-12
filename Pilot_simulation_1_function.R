
# FUNCTION========================================================

library(MASS,lib.loc ="/home/bcanourrego/work/paper2/Rpackages")
library(dbscan,lib.loc ="/home/bcanourrego/work/paper2/Rpackages")
library(zipfR,lib.loc ="/home/bcanourrego/work/paper2/Rpackages")
library(gmp,lib.loc ="/home/bcanourrego/work/paper2/Rpackages")
library(mvtnorm,lib.loc ="/home/bcanourrego/work/paper2/Rpackages")
library(pryr,lib.loc ="/home/bcanourrego/work/paper2/Rpackages")
library(profmem,lib.loc ="/home/bcanourrego/work/paper2/Rpackages")



# library(MASS)
# library(dbscan)
# library(zipfR)
# library(gmp)
# library(mvtnorm)
# library(pryr)
# library(profmem)
#library(EnvStats)
#Defining all the functions needed



# Note that for this function, there was a bug for which, big values of the parameters
# will result into a INF value or an NaN in the optimization
# I am not really sure yet, where are the boundaries.
# Function has been updated to use logarithms of big values  and then exponential
# Function has been update to use bigq instead of bigz, the latter rounded the numbers which was a mistake
# p=250 has worked so far


# ri: radius of the circle ci
# r: radius of the circle c defining the sample space
# tau: desired probability you want to obtain with circle ci
# query_point: xi vector of features
# centroid: Vectors of means of the training set
# max_function2 = function(ri,r,tau,query_point,centroid){
#   d = euclidean(centroid,query_point)
#   
#   
#   #the point is inside
#   #dbi=r-euclidean(centroid,query_point)
#   
#   # n here is the number of dimensions not the number of observations
#   p= length(centroid)
#   
#   
#   
#   if(d>=r+ri | ri==0  | r==0){
#     #There is no overlapping 
#     volumen=0
#     
#   }   else if(d<=abs(r-ri)){
#     # Volume of smaller sphere 
#     
#     # https://www.storyofmathematics.com/hypersphere/#:~:text=The%20volume%20%28or%2C%20more%20accurately%2C%20%E2%80%9Ccontent%E2%80%9D%29%20of%20a,r%20n%20where%20%CE%93%20represents%20the%20gamma%20function.
#     #volumen = (  pi^(n/2)   /(gamma(n/2 +1)) ) *(min(ri,r)^n)
#     volumen_log =   as.bigq((p/2)* log(pi) + p* log(min(ri,r) ) - lgamma(p/2 +1) )
#     
#     volumen = exp(1)^asNumeric(volumen_log)
#     
#   } else{
#     # Case in which there is a partial overlap of spheres
#     c1 = ((d^2)+(r^2)-(ri^2))/(2*d)
#     
#     c2 = ((d^2)-(r^2)+(ri^2))/(2*d)
#     
#     
#     vn_cap= function(rad,a){
#       if(a>=0){
#         #res =1/2 *((pi^(n/2)) /(gamma(n/2+1)) )*(rad^n) * beta_inc((n+1)/2,1/2,1-(a^2 /rad^2))
#         #library(gls)
#         #res =exp(log(1/2) + (p/2)* log(pi) - (lgamma(p/2+1)) +p *log(rad) +log(beta_inc((p+1)/2,1/2,1-(a^2 /rad^2) ) ) )
#         #library(zipfR)
#         res =exp(1)^asNumeric(as.bigq(log(1/2) + (p/2)* log(pi) - (lgamma(p/2+1)) +p *log(rad) +Rbeta(1-(a^2 /rad^2), (p+1)/2, 1/2, log = T) ) ) 
#       } else if(a<0){
#         #res = ((pi^(n/2)) /(gamma(n/2+1)) )*(rad^n) - vn_cap(rad,-a)
#         res = exp(1)^asNumeric(as.bigq( (p/2)* log(pi) -(lgamma(p/2+1)) +p*log(rad) ) ) - vn_cap(rad,-a) 
#       }
#       
#       return(res)
#     }#end of vn_cap
#     
#     volumen = vn_cap(r,c1)+vn_cap(ri,c2)
#   }
#   
#   
#   # Generalize prob of the cap
#   #sphere_prob = volumen/( (pi^(n/2)/(gamma(n/2 +1)) ) *(r^n) )
#   sphere_prob = volumen/exp(1)^asNumeric(as.bigq((p/2)* log(pi) + p* log(r)  - lgamma(p/2 +1) ))
#   
#   sphere_prob= if (sphere_prob < 0) 0 else if (sphere_prob > 1) 1 else sphere_prob
#   #sphere_prob = as.double(sphere_prob)
#   
#   return(sphere_prob-tau)
#   
#   
#   
#   
# }
max_function2 = function(ri,r,tau,query_point,centroid){
  d = euclidean(centroid,query_point)

  # n here is the number of dimensions not the number of observations
  p= length(centroid)
  
  # Helper: cap probability relative to its own ball of radius rad
  
  P_cap_self <- function(rad, a) {
    # x = 1 - a^2 / rad^2, clamped to [0, 1]
    x <- 1 - (a^2 / rad^2)
    x <- max(0, min(1, x))
    
    if (a >= 0) {
      # 0.5 * I_x((p+1)/2, 1/2)
      0.5 * pbeta(x, (p + 1) / 2, 1 / 2)
    } else {
      # Cap with negative a is complement of cap with -a
      1 - P_cap_self(rad, -a)
    }
  }
  
  
  
  # No overlap or degenerate radii
  if(d>=r+ri | ri==0  | r==0){
    sphere_prob=0
    
  }   else if(d<=abs(r-ri)){
    # One sphere fully inside the other: smaller sphere volume / volume of radius r
    # After simplications
    sphere_prob = (min(ri, r) / r)^p
    
  } else{
    # Case in which there is a partial overlap of spheres
    c1 = ((d^2)+(r^2)-(ri^2))/(2*d)
    
    c2 = ((d^2)-(r^2)+(ri^2))/(2*d)
    
    # Cap on sphere of radius r, already relative to ball of radius r
    P1 <- P_cap_self(r, c1)
    
    # Cap on sphere of radius ri, first relative to its own ball,
    # then scaled by (ri/r)^p to make it relative to ball of radius r
    P2_self <- P_cap_self(ri, c2)
    P2 <- (ri / r)^p * P2_self
    
    sphere_prob <- P1 + P2
    
    
    
  #   vn_cap= function(rad,a){
  #     if(a>=0){
  #       #res =1/2 *((pi^(n/2)) /(gamma(n/2+1)) )*(rad^n) * beta_inc((n+1)/2,1/2,1-(a^2 /rad^2))
  #       #library(gls)
  #       #res =exp(log(1/2) + (p/2)* log(pi) - (lgamma(p/2+1)) +p *log(rad) +log(beta_inc((p+1)/2,1/2,1-(a^2 /rad^2) ) ) )
  #       #library(zipfR)
  #       res =exp(1)^asNumeric(as.bigq(log(1/2) + (p/2)* log(pi) - (lgamma(p/2+1)) +p *log(rad) +Rbeta(1-(a^2 /rad^2), (p+1)/2, 1/2, log = T) ) ) 
  #     } else if(a<0){
  #       #res = ((pi^(n/2)) /(gamma(n/2+1)) )*(rad^n) - vn_cap(rad,-a)
  #       res = exp(1)^asNumeric(as.bigq( (p/2)* log(pi) -(lgamma(p/2+1)) +p*log(rad) ) ) - vn_cap(rad,-a) 
  #     }
  #     
  #     return(res)
  #   }#end of vn_cap
  #   
  #   volumen = vn_cap(r,c1)+vn_cap(ri,c2)
  }
  
  
  # Generalize prob of the cap
  #sphere_prob = volumen/exp(1)^asNumeric(as.bigq((p/2)* log(pi) + p* log(r)  - lgamma(p/2 +1) ))
  
  # Clamp to [0, 1]
  sphere_prob <- max(0, min(1, sphere_prob))
  
  #sphere_prob = as.double(sphere_prob)
  
  return(sphere_prob-tau)
  
  
  
  
}

#Euclidean distance for two points a and b
euclidean <- function(a, b){
  return(sqrt(sum((a - b)^2)))
}



# nrPoints : desired amount of observations
# nrDim: Number of dimensions
# center: center of the sphere
# r = radius of the sphere
GenerateSpherePoints <- function(nrPoints,nrDim,center=rep(0,nrDim),r=3.5){
  #generate the polar coordinates!
  x <-  matrix(runif(nrPoints*nrDim,-pi,pi),ncol=nrDim)
  x[,nrDim] <- x[,nrDim]/2
  #recalculate them to cartesians
  sin.x <- sin(x)
  cos.x <- cos(x)
  cos.x[,nrDim] <- 1  # see the formula for n.spheres
  
  y <- sapply(1:nrDim, function(i){
    if(i==1){
      cos.x[,1]
    } else {
      cos.x[,i]*apply(sin.x[,1:(i-1),drop=F],1,prod)
    }
  })*sqrt(runif(nrPoints,0,r^2))
  
  y <-  as.data.frame(
    t(apply(y,1,'+',center))
  )
  
  names(y) <- make.names(seq_len(nrDim))
  y
}



radius_candidate_finder= function(q,tau,r,centroid,max_r=3*r){
  return(stats::uniroot(max_function2,#interval = c(0,3*r),
                        lower = 0, upper = max_r,
                        r=r,tau=tau,query_point=q,centroid=centroid,
  )$root)
}

# Vectorized version for tau
radius_candidate_finder_tau_vect = Vectorize(radius_candidate_finder,vectorize.args="tau")



fixed_radius_knn = function(x,data_ref, ri, kmax,p,r,centroid){
  
  q = x[,1:p]
  
  
  id_query =x[,(p+1)]
  # There was a issue in which the query set names were being overwritten
  # I found that forcing the rownames to be this id query works
  #rownames(q) = id_query
  
  #q_group = x[1,(p+2)]
  
  
  # TASK:--------------------------------------
  # With ri^1 known I should get the probability associated with it tau^1
  # Then I need to create the grid seq(tau^1,0.999, length.out =5)
  # For the other four taus I get their respective ri.
  # and then the expansion is iteratively over those values
  
  # Finding the probability of a hyper sphere center at 0 with radius ri
  query_point_temp = centroid
  query_point_temp[1] = query_point_temp[1]-r
  #NEED CENTROID
  tau_1 =  max_function2(ri=ri,r=r,tau=0,query_point=query_point_temp,centroid=centroid)
  
  tau=seq(tau_1,0.999, length.out=5)
  
  ri_results_candidates = radius_candidate_finder_tau_vect(q=query_point_temp,
                                                  tau=tau,
                                                  r=r,
                                                  centroid= centroid,
                                                  max_r=3*r)
  #=--------------------------------------------------------------
  # old way to get candidates
  #ri_results_candidates = seq(ri,r,length.out=5)
  # List of query set ids for which the kNN haven't been found
  need_neighbors = as.character(id_query)
  # list with the kNN ids from the training set for each point in the query set
  #nn_res1 = list()
  # list with the kNN distances from the training set for each point in the query set
  nn_res2 = list()
  
  
  # nn_res2 <- vector("list", nrow(q))
  # names(nn_res2) <- id_query
  
  # list of k' neighbors used
  nn_res3 = list()
  
  # nn_res3 <- vector("list", nrow(q))
  # names(nn_res3) <- id_query
  
  # list containing how many radius was needed to expand in order to find kNN for each observation
  iteration_res = list()
  
  # iteration_res <- vector("list", nrow(q))
  # names(iteration_res) <- id_query
  
  counter = 1
  
  # This process is repeated until all observations have obtained at least k neighbors
  while(length(need_neighbors)>0){
    
    
    # function to find observations at a distance "eps" from each point
    
    # If one exceded the prespecified radius search, then it will gradually increase ri
    # CHANGE!!! It should be needing more iterations than specified. MAYBE I NEED TO CONSIDER THE
    # MAXIMUM RADIUS BASED ON THE QUERY FARTEST DISTANCE
    
    
    
    rad_aux = ifelse(!is.na(ri_results_candidates[counter]) ,
                     ri_results_candidates[counter] ,
                     (counter+2)*r)
    
    q_aux = q[id_query %in% need_neighbors,]
    
    # In the case that there is only one observation left, q would become a vector instead
    # of a matrix, causing problem, thus I force it to be a matrix again
    
    
    nn <- frNN(data_ref[,-(p+1)], # data ref without the id
               eps = rad_aux,
               query= q_aux)
    
    #res1 = nn$id
    res2 = nn$dist
    res3 = lapply(nn$dist,function(x) length(x))
    
    #res3[[!(names(res3) %in% as.character(1:nrow(q)))]] = 0
    # Observations for which lesser than k nearest neighbor have been found
    need_neighbors= names(which(lengths(res2)<kmax) )
    
    # Removing these from the list, so they will be used in the next iteration
    #res1[which(names(res1) %in%  need_neighbors)] <- NULL
    res2[which(names(res2) %in%  need_neighbors)] <- NULL
    # res3[which(names(res3) %in%  need_neighbors)] <- NULL
    
    #At least 1 observation in the iteration with K neighbors
    if(length(res2)>0 ){
      
      # Extract only the k nearest neighbors
      #res1 = lapply(res1,function(x) x[1:kmax])
      res2 = lapply(res2,function(x) x[kmax]) #x[1:kmax])
      
      #nn_res1= c(res1,nn_res1)
      nn_res2= c(nn_res2,res2)
      #nn_res3= c(nn_res3,res3)
      
      
      
      # Attach the iteration for which kNN where found 
      iteration_res[names(res2)]=counter
    }
    
    keys <- unique(c(names(nn_res3), names(res3)))
    nn_res3 = setNames(mapply(c, nn_res3[keys], res3[keys],SIMPLIFY = F), keys)
    
    counter = counter+ 1
  }
  
  
  #return(list(knn=nn_res1,knn_dist=nn_res2,k_prime=nn_res3,iterations=iteration_res))
  return(list(knn_dist=nn_res2,k_prime=nn_res3,iterations=iteration_res))
  
}



# Defining the individual costs for a observation in the query set
h_function = function(x,alpha=2,beta=1,k){
  j=1:length(x)
  
  d_ij= (x-k)/n
  
  h_ij = j * ifelse(d_ij<0,abs(d_ij)*alpha, abs(d_ij)*beta) 
  return( sum(h_ij) )
  
}




#  training, query,k ,z_k_p, naive?,naive_mult=1, M_simul_Z = 10000, alpha=1, beta=2

#type = naive,z,z_k,normal,spherical,spherical_k
knn.st.updated = function(training_set, query_set, k , alpha=1, beta=2, type="naive",
                          z_k_p=0.9,
                          z_p=0.5,
                          z_normal_p=0.5,
                          z_spherical_p = 0.5,
                          z_spherical_k_p = 0.5,
                          naive_mult=1,
                          M_simul_Z = 1000){
  
  #define radius and centroid thus the distribution-------------
  
  # Number of dimensions
  p = ncol(training_set)
  # Number of rows
  n = nrow(training_set)
  m= nrow(query_set)
  
  # Finding the centroid of training set
  centroid = apply(training_set,2, mean)
  
  
  # Calculating the distance of each point in training set to the centroid
  Distances_centroid = apply(training_set,1,euclidean, centroid)
  
  # Finding the point that is farther point from the centroid
  farthest = which.max(Distances_centroid)
  
  # Radius of the circle C defining the sample space
  r= as.numeric(Distances_centroid[farthest])
  #r
  
  #Find the group assignations
  # Generating the  g ring neighborhoods
  
  # group_aux = function(tau,rad){
  #   return(sqrt(tau*rad^2))
  # }
  # Finding groups from 0 to one, such that, there a total of g rings 
  # in addition to the g+1 out of sample space ring
  #ri_groups =group_aux(seq(0,1,length.out=g+1)[-1],rad = r)
  #ri_groups =sqrt( (seq(0,1,length.out=g+1)[-1] )*r^2)
  #ri_groups =r*(seq(0,1,length.out=g+1)[-1] )^(1/p) 
  # Classifying the query observation into the groups
  
  Distances_centroid_query = apply(query_set,1,euclidean, centroid)
  # Finding the distribution of distances-------------
  
  
  

  # Case for distribution of k-th euclidian distance
  
  if(type=="z_k"){
    
    
    # Number of simulation
    M = M_simul_Z
    
    
    x1= GenerateSpherePoints(nrPoints=M,nrDim=p,center=centroid,r=r)
    x2= GenerateSpherePoints(nrPoints=M,nrDim=p,center=centroid,r=r)
    
    z = apply(x1-x2,1,FUN = function(x ) sqrt(sum(x^2)) )
    
    # hist(z,freq = F,ylim=c(0,1),xlab="z=g(x1,x2)", 
    #   main=paste0("n=",n,"m=",m,"p=",p,"k=",k))
    
    
    
    # Find the distribution of k order statistic-------------
    
    # approach one simulation:
    # M2= 1000
    # z_boostrap = function(x){
    #   return(sort(sample(z,n,replace=T),decreasing = F)[k])
    #   }
    # 
    # 
    # z_k_simul =replicate(M2,  z_boostrap() )
    # 
    # 
    # 
    # hist(z_k_simul,freq = F,xlab="z_k", main=paste0("n=",n,"m=",m,"p=",p,"k=",k))
    
    
    
    
    
    #approach two derivation:
    
    # library(EnvStats)
    
    #z_quantile <- qemp(p = seq(0, 1, len = 100), obs = z)
    # f_z = function(x) demp(x, z, discrete = FALSE, density.arg.list = NULL)
    #f_z(3)
    
    # F_z <- function(x) pemp(x, z )
    # F_z(0)
    
    F_z = ecdf(z)
    #aaa(0)
    # f_z_k_deriv = function(x){
    #   part1 = exp(1)^ asNumeric(  as.bigq( lfactorial(n) -lfactorial(k-1) - lfactorial(n-k) ) ) 
    #   #part2 =   f_z(x) * (F_z(x))^(k-1) * (1-F_z(x))^(n-k)
    #   part2 =  exp( log(f_z(x)) +    (k-1) *log(F_z(x)) + (n-k)*log(1-F_z(x)) )
    #   return(part1*part2)
    # }
    # 
    # NUumerical problem for BIG N and K
    #f_z_k_deriv(2)
    #curve(f_z_k_deriv(x),from = 0,to=2*r,add = T,col="red",n = 100)
    
    
    F_z_k_deriv = function(x){
      
      j=k:n
      #cumulation = sum(choose(n,j) * (F_z(x))^(j) * (1-F_z(x))^(n-j) )
      cumulation = sum( exp(   lchoose(n,j) +j *log(F_z(x))+ (n-j) * log(1-F_z(x)) )    )
      return(cumulation)
    }
    
    #F_z_k_deriv(1.8*r)
    
    
    # Use radius search to find KNN in the query set-------------
    
    
    quantile_from_pdf_z_k_p <- function(z_k_p, lower = 0, upper = r) {
      uniroot(function(x) F_z_k_deriv(x) - z_k_p, lower = lower, upper = upper)$root
    }
    
    
    
    z_quantiles = quantile_from_pdf_z_k_p(z_k_p )

    
    # Probabilities associated with the these radius
    
    #tau_z_quantiles = max_function2(ri=z_quantiles,r=r,tau=0,query_point=centroid,centroid=centroid)
    
    ri_candidate= z_quantiles
    #tau_candidate= tau_z_quantiles
  } 
  if(type=="z") {
    
    
    # Number of simulation
    M = M_simul_Z
    
    
    x1= GenerateSpherePoints(nrPoints=M,nrDim=p,center=centroid,r=r)
    x2= GenerateSpherePoints(nrPoints=M,nrDim=p,center=centroid,r=r)
    
    z = apply(x1-x2,1,FUN = function(x ) sqrt(sum(x^2)) )
    
    # hist(z,freq = F,ylim=c(0,1),xlab="z=g(x1,x2)", 
    #   main=paste0("n=",n,"m=",m,"p=",p,"k=",k))
    
    
    F_z = ecdf(z)
    
    # Use radius search to find KNN in the query set-------------
    
    
    quantile_from_pdf_z_p <- function(z_p, lower = 0, upper = 2*r) {
      uniroot(function(x) F_z(x) - z_p, lower = lower, upper = upper)$root
    }
    
    
    
    z_quantiles = quantile_from_pdf_z_p(z_p )
    
    
    # Probabilities associated with the these radius
    
    #tau_z_quantiles = max_function2(ri=z_quantiles,r=r,tau=0,query_point=centroid,centroid=centroid)
    
    ri_candidate= z_quantiles
    #tau_candidate= tau_z_quantiles
    
  } 
  if(type=="normal"){
    
    
    
    F_z_normal = function(x){
      #1 - exp( lgamma(p/2)+ pgamma((x^2) /4,p/2,log.p = T,lower.tail = T)  -lgamma(p/2)  )
      
      1 - exp(Igamma(a=p/2, (x^2) /4,log = T,lower = F)  -lgamma(p/2)  )
    }
    
    # Use radius search to find KNN in the query set-------------
    
    #F_z_normal(10)
    #curve(F_z_normal,from=0,to=20)
    quantile_from_pdf_z_normal_p <- function(z_normal_p, lower = 0, upper = 2*exp(log(2)+lgamma( (p+1)/2) -lgamma(p/2) ) ) {
      uniroot(function(x) F_z_normal(x) - z_normal_p, lower = lower, upper = upper)$root
    }
    
    
    
    z_quantiles = quantile_from_pdf_z_normal_p(z_normal_p )
    #z_quantiles = exp(log(2)+lgamma( (p+1)/2) -lgamma(p/2) )  #quantile_from_pdf_z_normal_p(z_normal_p )
    
    # Probabilities associated with the these radius
    
    #tau_z_quantiles = max_function2(ri=z_quantiles,r=r,tau=0,query_point=centroid,centroid=centroid)
    
    ri_candidate= z_quantiles
    #tau_candidate= tau_z_quantiles
    
    
  } 
  if(type=="naive"){
    ri_naive = radius_candidate_finder(centroid, tau=naive_mult*k/n,
                                       max_r=3*r,#max(Distances_centroid_query)+r,
                                       r=r, centroid=centroid)
    
    
    ri_candidate= ri_naive
    #tau_candidate = naive_mult*k/n
  }
  if(type=="spherical"){
    
    # F_z_spherical = function(x){
    #   #1 - exp(  lgamma(p/2)+ pgamma(p/2, (x^2) /4,log.p = T)  -lgamma(p/2)  )
    #   
    #   # ((x/r)^p ) *Rbeta(1-(x/(2*r) )^r , p/2 +1/2, 1/2, log = F) +
    #   #   Rbeta( (x/(2*r) )^2 , p/2 +1/2, p/2+1/2, log = F)
    #   
    #    (  exp(p*(log(x)-log(r))  + Rbeta(1-(x/(2*r) )^2 , p/2 +1/2, 1/2, log = T) ) +
    #     exp(Rbeta( (x/(2*r) )^2 , p/2 +1/2, p/2 + 1/2, log = T) )  )
    #   
    # }
    # 
    # # Use radius search to find KNN in the query set-------------
    # 
    # #F_z_spherical(6)
    # #curve(F_z_spherical,from=0,to=12)
    # quantile_from_pdf_z_spherical_p <- function(z_spherical_p, lower = 0, upper = 2*r) {
    #   uniroot(function(x) F_z_spherical(x) - z_spherical_p, lower = lower, upper = upper)$root
    # }
    
    
    
    #z_quantiles = quantile_from_pdf_z_spherical_p(z_spherical_p )
    z_quantiles =  qnorm(z_spherical_p,mean= r*sqrt(2),sd=r/sqrt(2*p))
    
    # Probabilities associated with the these radius
    
    #tau_z_quantiles = max_function2(ri=z_quantiles,r=r,tau=0,query_point=centroid,centroid=centroid)
    
    ri_candidate= z_quantiles
    #tau_candidate= tau_z_quantiles
    
    
     
  }
  if(type=="spherical_k"){
    
    # F_z_spherical = function(x){
    #   
    #   ( exp(p*(log(x)-log(r))  + Rbeta(1-(x/(2*r) )^2 , p/2 +1/2, 1/2, log = T) ) +
    #     exp(Rbeta( (x/(2*r) )^2 , p/2 +1/2, p/2+1/2, log = T) ) )
    #   
    # }
    
    
    #F_z_spherical(8)
    #curve(F_z_spherical,from=4,to=12)
    
    # F_spherical_k_deriv = function(x){
    #   
    #   j=k:n
    #   #cumulation = sum(choose(n,j) * (F_z(x))^(j) * (1-F_z(x))^(n-j) )
    #   cumulation = sum( exp(   lchoose(n,j) +j *log(F_z_spherical(x))+ (n-j) * log(1-F_z_spherical(x)) )    )
    #   return(cumulation)
    # }
    # 
    # #F_spherical_k_deriv_2 = Vectorize(F_spherical_k_deriv)
    # 
    # #F_spherical_k_deriv_2(5)
    # 
    # #curve(F_spherical_k_deriv_2, from=5,to=7)
    # 
    # 
    # 
    # # quantile_from_pdf_z_spherical_k_p <- function(z_spherical_k_p, lower = 0, upper = 1.5*r) {
    # #   uniroot(function(x) F_spherical_k_deriv(x) - z_spherical_k_p, lower = lower, upper = upper)$root
    # # }
    # 
    # 
    
    
    F_spherical_k_deriv = function(x){
      
      j=k:n
      #cumulation = sum(choose(n,j) * (F_z(x))^(j) * (1-F_z(x))^(n-j) )
      cumulation = sum( exp(   lchoose(n,j) +j *log(pnorm(x,mean= r*sqrt(2),sd=r/sqrt(2*p)))+ 
                                 (n-j) * log(1- pnorm(x,mean= r*sqrt(2),sd=r/sqrt(2*p))  ) 
      )
      )
      return(cumulation)
    }
    
    
    quantile_from_pdf_z_spherical_k_p <- function(z_spherical_k_p, lower = 0, upper = 3 * r, step = 0.2*r, min_upper = lower + step) {
      repeat {
        # Try to compute the root
        result <- tryCatch({
          uniroot(function(x) F_spherical_k_deriv(x) - z_spherical_k_p, lower = lower, upper = upper)$root
        }, error = function(e) NULL)

        # If successful, return the result
        if (!is.null(result)) {
          return(result)
        }

        # If not successful, reduce upper bound
        upper <- upper - step

        # Stop if upper bound becomes too small
        if (upper <= min_upper) {
          stop("Failed to find root: upper bound too small.")
        }
      }
    }
    # 
    
    
    
    
    
    z_quantiles = quantile_from_pdf_z_spherical_k_p(z_spherical_k_p )
    
    #abline(v= z_quantiles)
    # Probabilities associated with the these radius
    
    #tau_z_quantiles = max_function2(ri=z_quantiles,r=r,tau=0,query_point=centroid,centroid=centroid)
    
    ri_candidate= z_quantiles
    #tau_candidate= tau_z_quantiles
    
    
    
  }
  # Creating identifiers for both data
  query_set$id= 1:nrow(query_set)
  #query_set$group= groups_query
  training_set$id= 1:nrow(training_set)
  
  kmax = k
  
  knn_st_results = fixed_radius_knn(x=query_set,data_ref =training_set,ri= ri_candidate,
                                    kmax,p,r,centroid=centroid)
  
  
  
  return(list(results=knn_st_results,
              ri_candidate=ri_candidate,
              r=r,
              #tau_candidate=tau_candidate,
              label= type#if(naive) paste0("naive_",tau_candidate) else paste0("z_k_",z_k_p)  
              ))
  
  
}
