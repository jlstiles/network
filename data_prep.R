library(plyr)

data_fake = read.csv("fakedata.csv")

# make columns via model matrix
X = model.matrix(formula('fakeoutcome.2015~ oofratiocen.2015*outoflaborcen.2015 + pwhite.2015 + 
                           pmale.2015 + wmpage0_19.2015 + wmpage65p.2015 +ratioage0_19.2015 + 
                           ratioage65.2015+ fakeoutcome.2009 + 
                           oofratiocen.2009*outoflaborcen.2009 + pwhite.2009 + pmale.2009 + 
                           wmpage0_19.2009 + wmpage65p.2009 +ratioage0_19.2009 + ratioage65.2009 + 
                           fakeoutcome.2001 + oofratiocen.2001*outoflaborcen.2001 + pwhite.2001 + 
                           pmale.2001 + wmpage0_19.2001 + wmpage65p.2001 +ratioage0_19.2001 + 
                           ratioage65.2001-1'), data = data_fake)

# stick the state variable in front and order by state to match the ddply output
X = cbind(data_fake$state.2001, X)
colnames(X)[1] = "state.2001"
X = X[order(X$state.2001),]

# This is a function that just does column means of input df
f_mean = function(M) {
  colMeans(M)
}

# does colmeans of all others in network for each individual
summary_maker = function(data, cols, fcn, ID) {
  D=ddply(data, ID, .fun = function(df) {
    meas = lapply(1:nrow(df), FUN = function(x) {
      temp = df[-x,cols]
      return(fcn(temp))
    })
    df_new = as.data.frame(do.call(rbind, meas))
    return(df_new)
  })
  return(D)
}

# perform colmeans summaries for all variables and name accordingly
summaries = summary_maker(X, 2:30, f_mean, "state.2001")
colnames(summaries)[2:30]  = paste0(colnames(summaries)[2:30], "_mean") 

# bind it all together
X_full = cbind(X, summaries[,-1])
head(X_full)
