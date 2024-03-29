# install.packages("devtools")
# devtools::install_github("MateusMaiaDS/mvnbart4")
rm(list=ls())
devtools::load_all()
set.seed(42)
# test for continuous outcomes

# Generating a new simulated dataset from different Friedman scenario
# ====
p <- 10
n <- 250
mvn_dim <- 2
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
     Sigma <- diag(c(sigma1^2,sigma2^2),nrow = mvn_dim)
     Sigma[1,2] <- Sigma[2,1] <-sigma1*sigma2*rho12
     determinant(Sigma)$modulus[1]
     eigen(Sigma)$values

}

sim_mvn_friedman1 <- function(n, p, mvn_dim,Sigma){


     # Verifying if it is a valid Sigma.
     if(NROW(Sigma)!=mvn_dim | NCOL(Sigma)!=mvn_dim){
             stop(paste0("Insert a valid Sigma matrix for the ",mvn_dim,"-d case."))
     }
     # Verifying if is semi-positive-define
     if(!all(eigen(Sigma)$values>0)){
        stop("Insert a positive-semidefined matrix")
     }

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



mod <- mvnbart(x_train = df_x,
                y_mat = df_y,
                x_test = df_x_new,
                varimportance = TRUE,specify_variables = list(c(1,2,3), c(1,4)),
                df = 10,n_tree = 100)


# Visualzing the variable importance
par(mfrow=c(1,3))
for( y_j_plot in 1:mvn_dim){
     total_count <- apply(mod$var_importance[,,y_j_plot],2,sum)
     norm_count <- total_count/sum(total_count)
     names(norm_count) <- paste0("x.",1:ncol(df_x))
     sort <- sort(norm_count,decreasing = TRUE)
     barplot(sort,main = paste0("Var importance for y.",y_j_plot),las = 2)
}

# Diagonistics of the prediction over the test set
par(mfrow = c(2,3))

for( y_j_plot in 1:mvn_dim){
     plot(sim_data$y_true[,y_j_plot],mod$y_hat_mean[,y_j_plot], pch = 20, main = paste0("y.",y_j_plot, " train pred"),
          xlab = "y.true.train" , ylab = "y.hat.train", col = ggplot2::alpha("black",0.2))
     abline(a = 0,b = 1,col = "blue", lty = 'dashed', lwd = 1.5)
}
for( y_j_plot in 1:mvn_dim){
     plot(sim_new$y_true[,y_j_plot],mod$y_hat_test_mean[,y_j_plot], pch = 20, main = paste0("y.",y_j_plot, " test pred"),
          xlab = "y.true.test" , ylab = "y.hat.test", col = ggplot2::alpha("black",0.2))
     abline(a = 0,b = 1,col = "blue", lty = 'dashed', lwd = 1.5)
}



# For the 2-dvariate case
if(mvn_dim == 2) {
     par(mfrow=c(1,3))
     plot(sqrt(mod$Sigma_post[1,1,]), main = expression(sigma[1]), type = 'l', ylab = expression(sigma[1]),xlab = "MCMC iter")
     abline(h = sqrt(Sigma[1,1]), lty = 'dashed', col = 'blue')
     plot(sqrt(mod$Sigma_post[2,2,]), main = expression(sigma[2]), type = 'l', ylab = expression(sigma[2]),xlab = "MCMC iter")
     abline(h = sqrt(Sigma[2,2]), lty = 'dashed', col = 'blue')
     plot(mod$Sigma_post[1,2,]/(sqrt(mod$Sigma_post[1,1,])*sqrt(mod$Sigma_post[2,2,])), main = expression(rho), type = 'l', ylab = expression(rho),xlab = "MCMC iter")
     abline(h = rho12, lty = 'dashed', col = 'blue')
} else if(mvn_dim ==3 ){
     par(mfrow=c(2,3))
     plot(sqrt(mod$Sigma_post[1,1,]), main = expression(sigma[1]), type = 'l', ylab = expression(sigma[1]),xlab = "MCMC iter")
     abline(h = sqrt(Sigma[1,1]), lty = 'dashed', col = 'blue')
     plot(sqrt(mod$Sigma_post[2,2,]), main = expression(sigma[2]), type = 'l', ylab = expression(sigma[2]),xlab = "MCMC iter")
     abline(h = sqrt(Sigma[2,2]), lty = 'dashed', col = 'blue')
     plot(sqrt(mod$Sigma_post[3,3,]), main = expression(sigma[3]), type = 'l', ylab = expression(sigma[3]),xlab = "MCMC iter")
     abline(h = sqrt(Sigma[3,3]), lty = 'dashed', col = 'blue')

     plot(mod$Sigma_post[1,2,]/(sqrt(mod$Sigma_post[1,1,])*sqrt(mod$Sigma_post[2,2,])), main = expression(rho[12]), type = 'l', ylab = expression(rho[12]),xlab = "MCMC iter")
     abline(h = Sigma[1,2]/(sqrt(Sigma[1,1])*sqrt(Sigma[2,2])), lty = 'dashed', col = 'blue')
     plot(mod$Sigma_post[1,3,]/(sqrt(mod$Sigma_post[1,1,])*sqrt(mod$Sigma_post[3,3,])), main = expression(rho[13]), type = 'l', ylab = expression(rho[13]),xlab = "MCMC iter")
     abline(h = Sigma[1,3]/(sqrt(Sigma[1,1])*sqrt(Sigma[3,3])), lty = 'dashed', col = 'blue')
     plot(mod$Sigma_post[2,3,]/(sqrt(mod$Sigma_post[3,3,])*sqrt(mod$Sigma_post[2,2,])), type = 'l', ylab = expression(rho[23]),xlab = "MCMC iter")
     abline(h = Sigma[2,3]/(sqrt(Sigma[2,2])*sqrt(Sigma[3,3])), lty = 'dashed', col = 'blue')

}

# test for binary outcomes ####
p <- 10
n <- 250
mvn_dim <- 3
if(mvn_dim==3){
     rho12 <- 0.8
     rho13 <- 0.5
     rho23 <- 0.25
     Sigma <- diag(mvn_dim)
     Sigma[1,2] <- Sigma[2,1] <- rho12
     Sigma[1,3] <- Sigma[3,1] <- rho13
     Sigma[2,3] <- Sigma[3,2] <- rho23
     determinant(Sigma)$modulus[1]
     eigen(Sigma)$values
} else {
     Sigma <- diag(nrow = mvn_dim)
     Sigma[1,2] <- Sigma[2,1] <- 0.75
     determinant(Sigma)$modulus[1]
     eigen(Sigma)$values

}

sim_mvn_binary <- function(n, p, mvn_dim,Sigma,seed = NULL){

     # Setting the seed

     # Generate the x matrix
     x <- matrix(runif(p*n), ncol = p)
     z1 <- 2*x[,1]*x[,2]
     z2 <- x[,4] + sin(x[,1]*2*pi)

     # Adding the only if p=3
     if(mvn_dim==3){
          z3 <- cos(2*pi*(x[,5] - x[,3]))
     }

     z <- matrix(0,nrow = n, ncol = mvn_dim)
     y <- matrix(0,nrow = n, ncol = mvn_dim)
     p <- matrix(0,nrow = n, ncol = mvn_dim)
     if(mvn_dim==3){
          z_true <- cbind(z1,z2,z3)
          for(i in 1:n){
               z[i,] <- z_true[i,] + mvnfast::rmvn(n = 1,mu = rep(0,mvn_dim),sigma = Sigma)
               y[i,] <- (z[i,] > 0)
               p[i,] <- pnorm(z_true[i,])
          }

     } else if(mvn_dim==2){
          z_true <- cbind(z1,z2)
          for(i in 1:n){
               z[i,] <- z_true[i,] + mvnfast::rmvn(n = 1,mu = rep(0,mvn_dim),sigma = Sigma)
               y[i,] <- (z[i,] > 0)
               p[i,] <- pnorm(z_true[i,])
          }
     }

     # Return a list with all the quantities
     return(list( x = x ,
                  y = y,
                  z_true = z_true,
                  z = z,
                  p = p,
                  Sigma = Sigma))
}

sim_data <- sim_mvn_binary(n = n,p = 10,mvn_dim = mvn_dim,
                           Sigma = Sigma,seed = 42)
sim_new <- sim_mvn_binary(n = n,p = 10,mvn_dim = mvn_dim,
                          Sigma = Sigma,seed = 43)

# Transforming the elements into df
df_x <- as.data.frame(sim_data$x)
df_y <- sim_data$y
df_x_new <- as.data.frame(sim_new$x)

mod <- mvnbart(x_train = df_x,
                y_mat = df_y,
                x_test = df_x_new,
                varimportance = TRUE,
                df = 10,n_tree = 100)
