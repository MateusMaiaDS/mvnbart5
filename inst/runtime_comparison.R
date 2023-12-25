# install.packages("devtools")
# devtools::install_github("MateusMaiaDS/mvnbart4")
devtools::load_all()
set.seed(42)
# test for continuous outcomes

# Generating a new simulated dataset from different Friedman scenario
# ====
p <- 10
n <- 250
mvn_dim <- 3
if(mvn_dim==3){
     sigma1 <- 1
     sigma2 <- 2.5
     sigma3 <- 5
     rho12 <- 0.8
     rho13 <- 0.5
     rho23 <- 0.25
     Sigma <- diag(c(sigma1^2,sigma2^2,sigma3^2),nrow = mvn_dim)
     Sigma[1,2] <- Sigma[2,1] <- rho12*sigma1*sigma2
     Sigma[1,3] <- Sigma[3,1] <- rho13*sigma1*sigma3
     Sigma[2,3] <- Sigma[3,2] <- rho23*sigma2*sigma3
     determinant(Sigma)$modulus[1]
     eigen(Sigma)$values
} else {
     sigma1 <- 1
     sigma2 <- 10
     rho12 <- 0.75
     Sigma <- diag(c(sigma1^2,sigma^2),nrow = mvn_dim)
     Sigma[1,2] <- Sigma[2,1] <-sigma1*sigma2*rho12
     determinant(Sigma)$modulus[1]
     eigen(Sigma)$values

}

sim_mvn_friedman1 <- function(n, p, mvn_dim,Sigma,seed = NULL){

     # Setting the seed

     # Generate the x matrix
     x <- matrix(runif(p*n), ncol = p)
     y1 <- 10*sin(x[,1]*x[,2]*pi) + 20*(x[,3]-0.5)^2
     y2 <- 8*x[,4] + 20*sin(x[,1]*pi)

     # Adding the only if p=3
     if(mvn_dim==3){
          y3 <- 15* x[,5]
     }

     y <- matrix(0,nrow = n,ncol = mvn_dim)
     if(mvn_dim==3){
          y_true <- cbind(y1,y2,y3)
          for(i in 1:n){
               y[i,] <- y_true[i,] + mvnfast::rmvn(n = 1,mu = rep(0,mvn_dim),sigma = Sigma)
          }

     } else if(mvn_dim==2){
          y_true <- cbind(y1,y2)
          for(i in 1:n){
               y[i,] <- y_true[i,] + mvnfast::rmvn(n = 1,mu = rep(0,mvn_dim),sigma = Sigma)
          }
     }

     # Return a list with all the quantities
     return(list( x = x ,
                  y = y,
                  y_true = y_true,
                  Sigma = Sigma))
}

sim_data <- sim_mvn_friedman1(n = n,p = 10,mvn_dim = mvn_dim,
                              Sigma = Sigma,seed = 42)
sim_new <- sim_mvn_friedman1(n = n,p = 10,mvn_dim = mvn_dim,
                             Sigma = Sigma,seed = 43)

# Transforming the elements into df
df_x <- as.data.frame(sim_data$x)
df_y <- sim_data$y
df_x_new <- as.data.frame(sim_new$x)


# Getting two differente packages
mod_comparison <- microbenchmark::microbenchmark(mvnbart4_mod <- mvnbart4::mvnbart4(x_train = df_x,y_mat = df_y,x_test = df_x_new),
                                   mvnbart5_mod <- mvnbart5::mvnbart(x_train = df_x,y_mat = df_y,x_test = df_x_new),times = 1)
