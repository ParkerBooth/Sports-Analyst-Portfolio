# Load Libraries
library(shinystan)
library(rstan)
library(bayesplot)
library(bridgesampling)
library(loo)
library(matrixStats)
library(coda)
library(ggmcmc)
options(mc.cores = parallel::detectCores())
color_scheme_set("red")


# Charting to Find Priors
{
  
  
# Normal(0, 5)
hist(diff_stan_df$Attacking_K_Perc, main = "Kill % Differential", xlab = "Kill %", probability = TRUE, ylim = c(0, 0.08))
curve(dnorm(x, mean = 0, sd = 5), col = "blue", lwd = 2, add = TRUE)


# Normal(70, 5)
hist(gameday_catapult$Perc_Good_Jumps, main = "High Jump % in Games", xlab = "High Jump %", probability = TRUE, ylim = c(0, 0.08))
curve(dnorm(x, mean = 70, sd = 5), col = "blue", lwd = 2, add = TRUE)

# Normal(60, 5)
hist(gd1_catapult$Perc_Good_Jumps, main = "High Jump % in Practice", xlab = "High Jump %", probability = TRUE, ylim = c(0, 0.08))
curve(dnorm(x, mean = 60, sd = 5), col = "blue", lwd = 2, add = TRUE)

# Normal(3.5, .5)
hist(gd1_catapult$Player_Load_Per_Min, probability = TRUE)
curve(dnorm(x, mean = 3.5, sd = .5), col = "blue", lwd = 2, add = TRUE)




# Normal(35, 5)
hist(utah_stan_df$Attacking_K_Perc, main = "Kill % Distribution", xlab = "Kill %", probability = TRUE, ylim = c(0, 0.08))
curve(dnorm(x, mean = 35, sd = 5), col = "blue", lwd = 2, add = TRUE)

# Normal(8, 2)
hist(utah_stan_df$Attacking_E_Perc, probability = TRUE)
curve(dnorm(x, mean = 8, sd = 2), col = "blue", lwd = 2, add = TRUE)

# Normal(65, 5)
hist(utah_stan_df$Defense_D_Perc, probability = TRUE)
curve(dnorm(x, mean = 65, sd = 5), col = "blue", lwd = 2, add = TRUE)

# Normal(12, 4)
hist(utah_stan_df$Serving_E_Perc, probability = TRUE)
curve(dnorm(x, mean = 12, sd = 4), col = "blue", lwd = 2, add = TRUE)

# Normal(8, 4)
hist(utah_stan_df$Serving_A_Perc, probability = TRUE)
curve(dnorm(x, mean = 8, sd = 4), col = "blue", lwd = 2, add = TRUE)

}




# Logit Model on Win Dummy     Win ~ Kill % + Error % + Dig % + Serve Error % + Ace %
{
  
plot(utah_stan_df$Attacking_K_Perc, utah_stan_df$Win_Dummy, xlab = "Kill %", ylab = "Wins Game")
  

# Specify Stan Model for Utah Variables
initial_utah_model = "
data {                            
  int<lower=0> N;                       // number of observations
  int<lower=0,upper=1> Win_Dummy[N];    // setting the all variables as binary
  vector[N] Attacking_K_Perc;           // independent variable 1
  vector[N] Attacking_E_Perc;           // independent variable 2
  vector[N] Defense_D_Perc;             // independent variable 3
  vector[N] Serving_E_Perc;             // independent variable 4
  vector[N] Serving_A_Perc;             // independent variable 5
}
parameters {
  real alpha;                 // intercept
  real b_kill_perc;           // beta for Kill %, etc
  real b_att_error_perc;
  real b_dig_perc; 
  real b_serve_error_perc;
  real b_ace_perc; 
}
model {
  b_kill_perc ~ normal(35, 5);
  b_att_error_perc ~ normal(8, 2);
  b_dig_perc ~ normal(65, 5);
  b_serve_error_perc ~ normal(12, 4);
  b_ace_perc ~ normal(8, 4);
  Win_Dummy ~ bernoulli_logit(alpha + b_kill_perc * Attacking_K_Perc + b_att_error_perc * Attacking_E_Perc + b_dig_perc * Defense_D_Perc + b_serve_error_perc * Serving_E_Perc + b_ace_perc * Serving_A_Perc);
}
generated quantities {
  vector[N] Win_Dummy_rep ; 
  for (n in 1:N) {
    Win_Dummy_rep[n] = bernoulli_logit_rng(alpha + b_kill_perc * Attacking_K_Perc[n] + b_att_error_perc * Attacking_E_Perc[n] + b_dig_perc * Defense_D_Perc[n] + b_serve_error_perc * Serving_E_Perc[n] + b_ace_perc * Serving_A_Perc[n]) ;
  } 
}
"


N <- nrow(utah_stan_df)
Win_Dummy <- utah_stan_df$Win_Dummy
Attacking_K_Perc <- utah_stan_df$Attacking_K_Perc
Attacking_E_Perc <- utah_stan_df$Attacking_E_Perc
Defense_D_Perc <- utah_stan_df$Defense_D_Perc
Serving_E_Perc <- utah_stan_df$Serving_E_Perc
Serving_A_Perc <- utah_stan_df$Serving_A_Perc

data_list = list(N = N, Win_Dummy = Win_Dummy, Attacking_K_Perc = Attacking_K_Perc, Attacking_E_Perc = Attacking_E_Perc,
                 Defense_D_Perc = Defense_D_Perc, Serving_E_Perc = Serving_E_Perc, Serving_A_Perc = Serving_A_Perc)

# Run Stan code for Utah match stats
match_stan <- stan(model_code = initial_utah_model, data = data_list, 
                  chains = 3, iter = 10000, warmup = 1000)


print(match_stan)


# Plot Trace
mcmc_trace(match_stan, pars = c("b_kill_perc")) +
  labs(title = "Traceplots for Kill Percentage") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title.y = element_blank()
  )

# Plot Density
mcmc_areas(match_stan, pars = c("b_kill_perc"), prob = 0.95, prob_outer = 0.99, point_est = "mean") +
  labs(title = "Posterior distribution for Kill Percentage",
       subtitle = "Including Mean Point Estimate & 95% probabilty interval") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.y = element_blank() 
  ) +
  scale_x_continuous(breaks = seq(0, 2, by = 0.25))



# Auto Correlation
mcmc_acf_bar(match_stan, pars = c("b_kill_perc")) +
  labs(title = "Autocorrelation over Kill Percentage") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5))




# PPC
samples <- rstan::extract(match_stan)

yRep <- samples$Win_Dummy_rep
y <- Win_Dummy
x <- Win_Dummy

ppc_dens_overlay(y, yRep[1:50,])
ppc_stat(y, yRep, stat = "mean",binwidth = .05)
ppc_stat(y, yRep, stat = "sd",binwidth = .05)


plot(utah_stan_df$Attacking_K_Perc, utah_stan_df$Win_Dummy, xlab = "Kill %", ylab = "Wins Game", xlim = c(0, 100))
samplesAll <- as.data.frame(rstan::extract(match_stan))

b0hat<-mean(samplesAll$alpha)
b1hat<-mean(samplesAll$b_kill_perc)
b2hat<-mean(samplesAll$b_att_error_perc)
b3hat<-mean(samplesAll$b_dig_perc)
b4hat<-mean(samplesAll$b_serve_error_perc)
b5hat<-mean(samplesAll$b_ace_perc)

curve(1/(1+exp(-b0hat-b1hat*x)),lwd=3,add=TRUE,col="red")
curve(1 / (1 + exp(-b0hat - b1hat*x - b2hat*x - b3hat*x - b4hat*x - b5hat*x)),lwd=3,add=TRUE,col="red")

1 / (1 + exp(-b0hat - b1hat * x - b2hat * x - b3hat * x - b4hat * x - b5hat * x))
1/(1+exp(-b0hat-b1hat*40))




}




# Logit Model on Win Dummy     Win ~ Kill Diff
{

  
# Plot the curve afterwards
plot(diff_stan_df$Attacking_K_Perc, diff_stan_df$Win_Dummy, xlab="Kill % Differential", ylab="Wins Game")
 
# b0hat<-mean(samplesAll$beta0)
# b1hat<-mean(samplesAll$beta1)
# 
# curve(1/(1+exp(-b0hat-b1hat*x)),lwd=3,add=TRUE,col="red")
  
  
# Try for Just Kill % on Win Dummy
kill_diff_perc_model = "
data {                            
  int<lower=0> N;         // number of observations
  int<lower=0,upper=1> Win_Dummy[N]; 
  vector[N] dif_Attacking_K_Perc;           
}
parameters {
  real intercept;
  real beta;
}
model {
  beta ~ normal(0, 5);
  Win_Dummy ~ bernoulli_logit(intercept + beta * dif_Attacking_K_Perc);
}
generated quantities {
  vector[N] Win_Dummy_rep ; 
  for (n in 1:N) {
    Win_Dummy_rep[n] = bernoulli_logit_rng(intercept + beta * dif_Attacking_K_Perc[n]) ;
  } 
}
"

N <- nrow(diff_stan_df)
Win_Dummy <- diff_stan_df$Win_Dummy
dif_Attacking_K_Perc <-  diff_stan_df$Attacking_K_Perc

kill_perc_data = list(N = N, Win_Dummy = Win_Dummy, dif_Attacking_K_Perc = dif_Attacking_K_Perc)

# Run Stan code for Kill % stats
kill_perc_stan <- stan(model_code = kill_diff_perc_model, data = kill_perc_data, 
                  chains = 3, iter = 10000, warmup = 1000)

print(kill_perc_stan)


# Plot Trace
mcmc_trace(kill_perc_stan, pars = c("beta")) +
  labs(title = "Traceplots for Kill Percentage Differential") +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title.y = element_blank()
  )

# Plot Density
mcmc_areas(kill_perc_stan, pars = c("beta"), prob = 0.95, prob_outer = 0.99, point_est = "mean") +
  labs(title = "Posterior distribution for Kill Percentage Differential",
       subtitle = "Including Mean Point Estimate & 95% probabilty interval") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.y = element_blank() 
  ) 


# Auto Correlation
mcmc_acf_bar(kill_perc_stan, pars = c("beta")) +
  labs(title = "Autocorrelation over Kill Percentage Differential") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5))


# PPC
samples <- rstan::extract(kill_perc_stan)

yRep <- samples$Win_Dummy_rep
y <- Win_Dummy
x <- Win_Dummy
ppc_dens_overlay(y, yRep[1:150,])
ppc_stat(y, yRep, stat = "mean",binwidth = .05)
ppc_stat(y, yRep, stat = "sd",binwidth = .05)


# Not Working Below

plot(diff_stan_df$Attacking_K_Perc, diff_stan_df$Win_Dummy, xlab = "Kill Differential %", ylab = "Wins Game")

samplesAll <- as.data.frame(rstan::extract(kill_perc_stan))

b0hat<-mean(samplesAll$intercept)
b1hat<-mean(samplesAll$beta)

curve(1/(1+exp(-b0hat-b1hat*x)),lwd=3,add=TRUE,col="red")


1/(1+exp(-b0hat-b1hat*0))



}




# Regression high jumps (combine high and med) on Kill Differential    Kill Diff ~ Jumps
{

jumps_on_kills_model_errors_modeled = "
data {
  int<lower=0> N; 
  int<lower=0> K; 
  matrix[N, K] X; 
  vector[N] y;
}
parameters {
  vector[K] beta; 
  real<lower=0> sigma;
  real<lower=2> nu;
}
model {
  nu ~ gamma(2,.1);
  y ~ student_t(nu, X * beta, sigma);
}
generated quantities {
  vector[N] y_rep ; 
  for (n in 1:N) {
    y_rep[n] = student_t_rng(nu, X[n]*beta, sigma) ;
  } 
}
"


# Create data list for Stan
X = model.matrix(~Perc_Good_Jumps, gameday_catapult)
jumps_on_kills_data <- list(N = nrow(gameday_catapult), K = ncol(gameday_catapult), y = gameday_catapult$Attacking_K_Perc , X = X)

abline2 <- lm(Attacking_K_Perc ~ Perc_Good_Jumps, data = gameday_catapult)
plot(gameday_catapult$Perc_Good_Jumps, gameday_catapult$Attacking_K_Perc)
abline(abline2, col = "blue")

jumps_on_kills_stan_error <- stan(model_code = jumps_on_kills_model_errors_modeled, data = jumps_on_kills_data, 
                                  chains = 3, iter = 10000, warmup = 1000)


print(jumps_on_kills_stan_error)
plot(jumps_on_kills_stan_error, pars = c("beta[2]", "nu"))

# Charts to compare between Robust vs not Robust
mcmc_dens(jumps_on_kills_stan_error, pars = c("beta[2]", "nu"))


mcmc_dens(jumps_on_kills_stan_error, pars = c("nu")) +
  theme_minimal() +
  theme(axis.text.y = element_blank())


# Plot Trace
mcmc_trace(jumps_on_kills_stan_error, pars = c("beta[2]")) +
  labs(title = "Traceplots for Good Jump Percentage") +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title.y = element_blank()
    )

# Plot Density
mcmc_areas(jumps_on_kills_stan_error, pars = c("beta[2]"), prob = 0.95, prob_outer = 0.99, point_est = "mean") +
  labs(title = "Posterior distribution for the Coefficient on Good Jumps",
       subtitle = "Including Mean Point Estimate & 95% probabilty interval") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.y = element_blank() 
  ) +
  scale_x_continuous(breaks = seq(0, 2, by = 0.25))



# Auto Correlation
mcmc_acf_bar(jumps_on_kills_stan_error, pars = c("beta[2]")) +
  labs(title = "Autocorrelation over Good Jumps") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5))




# PPC
samples <- rstan::extract(jumps_on_kills_stan_error)

yRep <- samples$y_rep
y <- gameday_catapult$Attacking_K_Perc

ppc_dens_overlay(y, yRep[1:125,])+ xlim(-30, 30)
ppc_stat(y, yRep, stat = "mean",binwidth = .1)+ xlim(-10, 10)
ppc_stat(y, yRep, stat = "sd",binwidth = .1) + xlim(0, 20)



}




# Regression jumps and player load in GD-1 on Kill Differential    Kill Diff ~ Jumps + Player Load (GD-1)
{
  
practice_jumps_errors_modeled = "
data {
  int<lower=0> N; 
  int<lower=0> K; 
  matrix[N, K] X; 
  vector[N] y;
}
parameters {
  vector[K] beta; 
  real<lower=0> sigma;
  real<lower=2> nu;
}
model {
  nu ~ gamma(2,.1);
  y ~ student_t(nu, X * beta, sigma);
}
generated quantities {
  vector[N] y_rep ; 
  for (n in 1:N) {
    y_rep[n] = student_t_rng(nu, X[n]*beta, sigma) ;
  } 
}
"

# practice_jumps_errors_modeled = "
# data {
#   int<lower=0> N; 
#   int<lower=0> K; 
#   matrix[N, K] X; 
#   vector[N] y;
# }
# parameters {
#   vector[K] beta; 
#   real<lower=0> sigma;
# }
# model {
#   y ~ normal(X*beta, sigma);
# }
# generated quantities {
#   vector[N] y_rep ; 
#   for (n in 1:N) {
#      y_rep[n] = normal_rng(X[n]*beta, sigma);  
#   } 
# }
# "

X = model.matrix(~Perc_Good_Jumps + Player_Load_Per_Min, gd1_catapult)
practice_jumps_data <- list(N = nrow(gd1_catapult), K = ncol(gd1_catapult), y = gd1_catapult$Attacking_K_Perc , X = X)

# abline2 <- lm(Attacking_K_Perc ~ Perc_Good_Jumps + Player_Load_Per_Min, data = gd1_catapult)
# plot(gameday_catapult$Perc_Good_Jumps, gameday_catapult$Attacking_K_Perc)
# abline(abline2, col = "blue")


practice_jumps_on_kills_stan_error <- stan(model_code = practice_jumps_errors_modeled, data = practice_jumps_data, 
                                           chains = 3, iter = 10000, warmup = 1000)

print(practice_jumps_on_kills_stan_error)
print(practice_jumps_on_kills_stan_error, digits_summary=2, probs = c(.025, .975))

plot(practice_jumps_on_kills_stan_error, pars = c("beta[2]", "beta[3]", "nu"))




mcmc_dens(practice_jumps_on_kills_stan_error, pars = c("nu")) +
  theme_minimal() +
  theme(axis.text.y = element_blank())



# Plot Trace
mcmc_trace(practice_jumps_on_kills_stan_error, pars = c("beta[2]", "beta[3]")) +
  labs(title = "Traceplots for Good Jumps and Player Load per Minute",
       subtitle = "beta[2] = Good Jumps, beta[3] = Load per Min") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title.y = element_blank()
  )

# Plot Density
mcmc_areas(practice_jumps_on_kills_stan_error, pars = c("beta[2]"), prob = 0.95, prob_outer = 0.99, point_est = "mean") +
  labs(title = "Posterior distribution for the Coefficient on Good Jumps",
       subtitle = "Including Mean Point Estimate & 95% probabilty interval") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.y = element_blank() 
  )

mcmc_areas(practice_jumps_on_kills_stan_error, pars = c("beta[3]"), prob = 0.95, prob_outer = 0.99, point_est = "mean") +
  labs(title = "Posterior distribution for the Coefficient on Player Load per Minute",
       subtitle = "Including Mean Point Estimate & 95% probabilty interval") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.y = element_blank() 
  )

# Auto Correlation
mcmc_acf_bar(practice_jumps_on_kills_stan_error, pars = c("beta[2]", "beta[3]")) +
  labs(title = "Autocorrelation over Good Jumps and Player Load per Minute",
       subtitle = "beta[2] = Good Jumps, beta[3] = Load per Min") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5))




# PPC
samples <- rstan::extract(practice_jumps_on_kills_stan_error)

yRep <- samples$y_rep
y <- gd1_catapult$Attacking_K_Perc

ppc_dens_overlay(y, yRep[1:31,])
ppc_stat(y, yRep, stat = "mean",binwidth = .1) + xlim(-10, 10)
ppc_stat(y, yRep, stat = "sd",binwidth = .1) + xlim(0, 20)




}


