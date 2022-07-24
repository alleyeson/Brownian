#########################################################
######## Brownian Bridge 
#########

## mean 0 and variance t,s based model 
brownian_bridge_distribution_f <- function(s,t,y,n){
  ret_v <- (s/t)*y  + sqrt( (t-s)*s/t ) * rnorm(n)
  #print(ret_v)
  return(mean(ret_v))
}

brownian_bridge_path_f <- function(s,t,y,n = 30){
  get_path_v <- c(0) 
  vect_v <- s:t
  for(i in 1:length(vect_v)){
    get_path_v[i] <- brownian_bridge_distribution_f(s = vect_v[i],t,y,n)
  }
  
  return(get_path_v)
}


brownian_bridge_sim_f <- function(s,t,y,n_sims_per_point, num_paths){
  
  collect_paths_m <- matrix(0, nrow = t, ncol = num_paths) ## s = 0 should be W(t0) or just 0 
  if(s == 0){
    collect_paths_m <- matrix(0, nrow = t+1, ncol = num_paths) ## s = 0 should be W(t0) or just 0 
  }
  colnames_v <- NA
  for(i in 1:num_paths){
    check_v <- brownian_bridge_path_f(s,t,y, n = n_sims_per_point)
    #print(check_v)
    collect_paths_m[,i] <- check_v
    colnames_v[i] <- paste0(c('pass',i), collapse = '_', sep= '')
  }
  
  colnames(collect_paths_m) <- colnames_v
  return(collect_paths_m)
}

### 
# specified mean and variance for other stocks 

### check visually 

brownian_bridge_distribution_f(s=1,10,y = 0.5,n = 10)
check_obj <- brownian_bridge_path_f(s=1,t=50,y = 0.2,n = 30)

plot(brownian_bridge_path_f(s=1,t=50,y = 0.2,n = 30), type = 'l')

get_b_bridge_m <- brownian_bridge_sim_f(s = 0, t = 30,y=0.1, n_sims_per_point = 60, num_paths = 10)
S0 <- 100 
s0_possible_paths_m <- S0* (1+ get_b_bridge_m) 

colnames(get_b_bridge_m)

get_plot_data_obj <- ggplot_lines.f(data_input = get_b_bridge_m)
get_plot_data_obj$CumRatePerfPlot

get_plot_data_obj$RegRatePlot

get_plot_data_obj$CumSumPerfPlot       



