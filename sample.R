# creating a longitudinal dataset

# draw covs.  r people per n clusters
r = 10
n = 100

ID = unlist(lapply(1:n, FUN = function(x) rep(x,r)))

# error terms for each cluster's time 0 covs'
dfs = lapply(0:2, FUN = function(t) { 
L_1e = rnorm(n)
L_2e = runif(n,.3, .7)

Lnames = c(paste0("L", t, "_1"), paste0("L", t, "_2"))
L_1 = unlist(lapply(1:n, FUN = function(x) rnorm(r)+L_1e[x]))
L_2 = unlist(lapply(1:n, FUN = function(x) rnorm(r)+L_1e[x]))

# make two different exposures
A_1e = rnorm(n)
A_2e = rnorm(n)

A_1 = unlist(lapply(1:n, FUN = function(x) rnorm(r)+A_1e[x]))
A_2 = unlist(lapply(1:n, FUN = function(x) rnorm(r)+A_1e[x]))
Anames = c(paste0("A", t, "_1"), paste0("A", t, "_2"))

if (t==2) {
  Y = rnorm(r*n)
  df = cbind(L_1, L_2, A_1, A_2, Y)
  colnames(df) = c(Lnames, Anames, "Y")
  } else {
  df = cbind(L_1, L_2, A_1, A_2)
  colnames(df) = c(Lnames, Anames)
}
return(df)
})

df_wide = as.data.frame(cbind(ID, do.call(cbind, dfs)))
head(df_wide)

# Then we add columns to the df based on functions of specified columns
# make summary measures for L based on matrix for the network.  For simplicity here, 
# we will use same fcn for L as for A

f_L0 = function(M) {
  c(median(M[,1]), mean(M[,2]))
}

summary_maker = function(data, cols, fcn) {
  data = df_wide
  cols = 2:3
  fcn = f_L0
 D=ddply(data, "ID", .fun = function(df) {
   meas = lapply(1:nrow(df), FUN = function(x) {
    temp = df[-x,cols]
    return(fcn(temp))
  })
  df_new = as.data.frame(do.call(rbind, meas))
  return(df_new)
})
 return(D[,-1])
}


# specify cols and fcn for L and A summaries for each time point
L_summary_info = list(list(data = df_wide, cols = 2:3, fcn = f_L0),
                      list(data = df_wide, cols = 5:6, fcn = f_L0),
                      list(data = df_wide, cols = 9:10, fcn = f_L0))
A_summary_info = list(list(data = df_wide, cols = 4:5, fcn = f_L0),
                      list(data = df_wide, cols = 7:8, fcn = f_L0),
                      list(data = df_wide, cols = 11:12, fcn = f_L0))

# compute the summary measures for each time point
L0_summaries = lapply(L_summary_info, FUN = function(L) {
  summary_maker(data = L$data, cols = L$cols, fcn = L$fcn)
})

head(L0_summaries[[1]])

A0_summaries = lapply(A_summary_info, FUN = function(L) {
  summary_maker(data = L$data, cols = L$cols, fcn = L$fcn)
})

head(A0_summaries[[1]])

# compile df for each time point, including summaries is right order with names
dfs_per_time = lapply(1:3, FUN = function(t) {
  df_new = cbind(df_wide[,L_summary_info[[t]]$cols], L0_summaries[[t]], 
                 df_wide[,A_summary_info[[t]]$cols], A0_summaries[[t]])
  colnames(df_new)[c(3:4, 7:8)] = c(paste0(c("L0_sum1_", "L0_sum2_"), t), paste0(c("A0_sum1_", "A0_sum2_"), t))
  return(df_new)
  
})

head(dfs_per_time[[1]])

# crunch it all together in wide form to then be sequentially regressed
df_new = cbind(ID, do.call(cbind, dfs_per_time), Y=df_wide$Y)
head(df_new)

# Can now use sl3, where we can run a superlearner and predict, 
# then use as outcomes for the next regression.  That is, just use sl3 
# but specify the ID variable.  

