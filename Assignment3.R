#Assignment 3 coded by Nico Purnomo, Natasya Lucky, and Abraham
# ------------------------------------------------------
# Clear all entries
rm(list=ls())

# -----------------------------------------------------------------
# -----------------------------------------------------------------
library(MASS)
library(AER)
library(xtable)
set.seed(42)

# Parameter definitions
beta0 = 0
beta1 = 0
beta2 = 0
sigma = 1

# Simulation definitions
nreps = 1000  # Number of repetitions

# List each of the cases to go through
A1 = list(n = 100,  rho = 0,   Hetero = FALSE)
A2 = list(n = 100,  rho = 0,   Hetero = TRUE)



# Compile all of the above cases into a single, compact matrix
A = list(A1,A2)

# Storage matrices for p-values
p.OLS = matrix(nrow = nreps, ncol = length(A))
p.HC  = matrix(nrow = nreps, ncol = length(A))

beta.OLS = matrix(nrow = nreps, ncol = length(A))

##### Outer loop
for (i in 1:length(A)){
  # Select the appropriate variables defined by the i-th case
  # (hey, what's with the double square brackets?)
  n      = A[[i]]$n       # Get the sample size
  rho    = A[[i]]$rho     # Get the correlation between regressors
  Hetero = A[[i]]$Hetero  # Specify if there is non-constant variance
  
  # Use the selected rho value to make a covariance matrix
  VarX   = cbind(c(1,rho), c(rho,1))
  
  ##### Inner loop
  for (j in 1:nreps){
    # Generate our values of X
    X = mvrnorm(n, mu = c(0,0), Sigma = VarX)
    X1 = X[,1] # Specify element X1
    X2 = X[,2] # Specify element X2
    
    # Check the heteroskedasticity condition now that X1 is defined
    if (Hetero == "TRUE"){
      sd = exp(X1/2) # Hang on, how come we're dividing by 2?
    } else{
      sd = sigma     # No heteroskedasticity (yay!)
    }
    
    # Generate our values of Y
    Y = rnorm(n, mean = beta0 + beta1*X1 + beta2*X2, (0.1*X1*X2)^2)
              
     # Get regression estimates
     eq = lm(Y ~ X1 + X2)
              
     # Conduct hypothesis tests as per tute sheet
     # then store them in the appropriate matrix
     # (hey, what's coeftest and why are we calling element [2,4]?)
     p.OLS[j,i] = coeftest(eq)[2,4]
     p.HC[j,i]  = coeftest(eq, vcov = vcovHC(eq))[2,4]
     beta.OLS[j,i] = eq$coefficients[2]
  } 
} 
#tab = cbind(p.OLS < 0.05, p.HC < 0.05)
tab = cbind(colMeans(p.OLS < 0.05), colMeans(p.HC < 0.05))
colnames(tab) = c("OLS","HC")

print(tab, digits = 3)
print(xtable(tab, include.rownames=FALSE,digits=4))
