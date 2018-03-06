library(mlogit)
data(Yogurt)

pan   = Yogurt[,1] #index of indivudual/household
price = Yogurt[,6:9]
feat  = Yogurt[,2:5]
choice= Yogurt[,10]

# One hot encoding the yogurt brands. 
choice = data.frame(sapply(levels(choice), function(x) as.integer(x == choice)))

#number of observations
n_obs = nrow(choice)

#number of alternatives in each choice situation
n_alt = 3

#number of households/people (and each face multiple choice situations)
n_p = length(unique(pan))

#number of explanatory variables: price and feature
n_var = 2


#define the function of log likelihood
rc.llh = function(beta){
  seed2 = seed #reset the seed for each call of rc.llh()
  
  #initialize the probability for n_p people's sequence of choices
  pll = matrix(0, n_p, 1) # init value of 0 in n_p x 1 matrix
  
  m = beta[c(4, 5)] # mean of price and feature, can be extracted from beta
  s = beta[c(6, 7)] # sd of price and feature, can be extracted from beta
  for(i in 1:n_p){ #loop over individuals, calculate the loglikelihood
    set.seed(seed2)
    
    ##############
    # Simulation #
    ##############
    
    #random draw ~ N(0,1) into a matrix of n_var x n_draws
    err = apply(matrix(1, n_var, n_draws, dimnames = list(c('price', 'feat'))), c(1,2), rnorm)
    beta2 = m + s * err #draws of random coefficients
    beta3 = rbind(replicate(n_draws, beta[c(1, 2, 3)]), beta2) #5 x n_draws matrix, row1-3: constants, row4-5: coefficients for price & feature -
             #from beta2

    # get X variables(e.g. constant, price, feature) for person/household i
    # (number of purchases by same person == nrows) x (3 columns) MATRIX -> 2 columns for dannon as baseline
    yoplait_i = cbind(rep.int(1, nrow(price[pan == i, ])), 
      price[pan == i, ]$price.yoplait, feat[pan == i,]$feat.yoplait) 

    hiland_i  = cbind(rep.int(1, nrow(price[pan == i, ])), 
      price[pan == i, ]$price.hiland, feat[pan == i,]$feat.hiland)

    weight_i  = cbind(rep.int(1, nrow(price[pan == i, ])), 
      price[pan == i, ]$price.weight, feat[pan == i,]$feat.weight)

    dannon_i  = cbind(price[pan == i,]$price.dannon, feat[pan == i,]$feat.dannon)
    
    ## e_brand is a n_purchases of consumer i x 1 [choice probability for each brand per purchase occasion]
    e_yoplait= exp(yoplait_i %*% beta3[c(1, 4, 5), ]) # numerator for brand Yoplait for person i
    e_hiland = exp(hiland_i %*% beta3[c(2, 4, 5), ])
    e_weight = exp(weight_i %*% beta3[c(3, 4, 5), ])
    e_dannon = exp(dannon_i %*% beta3[c(4,5), ])
    denominator = e_yoplait + e_hiland + e_weight + e_dannon

    choice_i = choice[pan == i, ] # get actual choices for person i
    
    # one choice for each time period
    cp_yoplait = e_yoplait / denominator
    cp_hiland  = e_hiland / denominator
    cp_weight  = e_weight / denominator
    cp_dannon  = e_dannon / denominator
    
    # matrix of choice probabilities for person i: 
    # num of rows = number of purchases of person i; num of columns = n_draws
    cp = cp_yoplait * choice_i$yoplait + cp_hiland * choice_i$hiland + 
    cp_weight * choice_i$weight + cp_dannon * choice_i$dannon
    
    ## accounts for the j (choice) multiplication of likelihood function. pg28 of Lecture_8
    cp_pan = apply(cp, 2, prod) #2 indicates columns and cp_pan gives probability multiplied
    #across choice situations (all the observations/purchases for person i)
    
    ## account for R: sum and divided by R - pg 10 of lecture9
    pll[i] = log(mean(cp_pan)) #simulated probability: averaged over all n_draws draws, then take log
  }
  llh = sum(pll) #sum over each person
  return(llh)
}

#initialize beta coefficients
beta=c("const_yoplait" = 0.1, "const_hiland" = 0.1, "const_weight" = 0.1, "price" = 0.1, "feat" = 0.1,
       "price.sd" = 0.1, "feat.sd" = 0.1)
n_draws = 50 #number of draws in simulation
seed = 999 #seed to use in making draws

#maximize the llh using maxLik package:
yogurt.llh = maxLik(rc.llh, start = beta, method = "bfgs")
summary(yogurt.llh)

