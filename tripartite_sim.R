a = array(dim = c(5, 5, 10))

# rows are individuals, columns are locations
# third dimension is time
a[,,1] = matrix(c(0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0), byrow = T, nrow = 5)

# simulation
# probability of leaving patch is even
# probability going to another patch, conditioned on leaving, is equal for all other patches
set.seed(4958344)
for (t in 2:10) { 
  for (k in 1:5) {
    if (runif(1) < 0.5) { a[k,,t] = sample(c(0, 0, 0, 0, 1), replace = FALSE)
    } else a[k,,t] = a[k,,t-1]
  }
}

a

##### 

sim.fission.fusion = function(indvs.total, sites.total, times.total, prob.move) {
  
  # indvs.total = 5
  # sites.total = 5
  # times.total = 10
  # prob.move = 0.5
  
  a = array(dim = c(indvs.total, sites.total, times.total))
  
  a[,,1] = t(replicate(indvs.total, sample(c(rep(0,sites.total-1), 1))))
  
  for (t in 2:times.total) { 
    for (k in 1:indvs.total) {
      if (runif(1) < prob.move) { a[k,,t] = sample(c(0, 0, 0, 0, 1), replace = FALSE)
      } else a[k,,t] = a[k,,t-1]
    }
  }
  
  return(a)
  
}

sim.stable.groups = function(indvs.total, sites.total, times.total, prob.follow, groups) {
  
  # indvs.total = 9
  # sites.total = 5
  # times.total = 10
  # prob.move = 0.5
  # prob.follow = 0.9
  # groups = c(rep(1,3), rep(2,3), rep(3,3))
  
  a = array(dim = c(indvs.total, sites.total, times.total))
  
  inits = t(replicate(length(unique(groups)), sample(c(rep(0,sites.total-1), 1))))
  for (g in groups) for (k in which(groups %in% g)) a[k,,1] = inits[g,]
  
  for (t in 2:times.total) { 
    for (g in groups) {
      group.target = sample(1:sites.total, 1)
      for (k in which(groups %in% g)) {
        if (runif(1) < prob.follow) { 
          a[k,,t] = 0 
          a[k,group.target,t] = 1
        } else { 
          a[k,,t] = 0
          a[k,sample((1:sites.total)[-group.target], 1),t] = 1
        }
      }
    }
  }
  
  return(a)
  
}

####

