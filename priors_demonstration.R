library(lazymcmc)
library(tidyverse)
library(coda)

##
set.seed(10)

##########################################
## Quick analysis demonstrating that prior choice can make
## two distributions look different, even if they are actually identical
##########################################


## Simulating values from two distributions with the same mean and sd
## We have more observations of Virus 1 than Virus 2
N1 <- 50
N2 <- 7
## Sampled fromt he same distributions
x_mean <- 7
x_sd <- 1.5

virus1 <- tibble(i=1:N1,x=rnorm(N1, mean=x_mean, sd=x_sd),Data="A",yshift=-0.1)
virus2 <- tibble(i=1:N2,x=rnorm(N2, mean=x_mean, sd=x_sd),Data="B",yshift=-0.05)

virus_dat <- bind_rows(virus1, virus2)


##########################################
## Prior function
##########################################
## Assume the same priors on both distributions, similar to Kissler et al.
prior_func <- function(pars){
  x_mean1_prior <- dnorm(pars["x_mean1"],mean=15,sd=5,log=TRUE)
  x_mean2_prior <- dnorm(pars["x_mean2"],mean=15,sd=5,log=TRUE)
  
  x_sd1_prior <- dexp(pars["x_sd1"],rate=1,log=TRUE)
  x_sd2_prior <- dexp(pars["x_sd2"],rate=1,log=TRUE)
  
  prior_all <- x_mean1_prior + x_mean2_prior + x_sd1_prior + x_sd2_prior
  prior_all
  
}

## Posterior function
## Normal likelihood function
create_posterior_func <- function(parTab,dat, PRIOR_FUNC,...){
  par_names <- parTab$names
  
  virus_1_dat <- dat %>% filter(Data == "A") %>% pull(x)
  virus_2_dat <- dat %>% filter(Data == "B") %>% pull(x)
  
  f <- function(pars){
    names(pars) <- par_names
    
    lik1 <- sum(dnorm(virus_1_dat, mean=pars["x_mean1"], sd=pars["x_sd1"],log=TRUE))
    lik2 <- sum(dnorm(virus_2_dat, mean=pars["x_mean2"], sd=pars["x_sd2"],log=TRUE))
    
    prior <- PRIOR_FUNC(pars)
    
    posterior <- lik1 + lik2 + prior
    posterior
  }
  f
}

## Set up lazymcmc framework
parTab <- data.frame(values=c(x_mean,x_sd,x_mean,x_sd),
                     names=c("x_mean1","x_sd1","x_mean2","x_sd2"),
                     fixed=c(0,0,0,0),
                     lower_bound=c(-100,0,-100,0),
                     upper_bound=c(100,10,100,10),
                     steps=c(0.1,0.1,0.1,0.1),
                     lower_start=c(0,0.5,0,0.5),
                     upper_start=c(10,2.5,10,2.5)
                     )

## Control MCMC length etc
mcmcPars <- c("iterations"=10000,"popt"=0.44,"opt_freq"=1000,
              "thin"=10,"adaptive_period"=5000,"save_block"=1000)

## Run 3 chains
for(chain in 1:3){
  res <- run_MCMC(parTab=parTab,data=virus_dat, mcmcPars=mcmcPars, filename=paste0("chains/virus_compare_",chain),
                CREATE_POSTERIOR_FUNC = create_posterior_func, mvrPars=NULL,PRIOR_FUNC=prior_func)
}
## Read in chains
chains <- load_mcmc_chains("chains",parTab,unfixed=TRUE,burnin = 5000,multi = FALSE)
## Inspect convergence
plot(chains[[1]])

## Plot posteriors for means against prior
chains_comb <- as.data.frame(chains[[2]])
chains_plot_mean <- chains_comb[,c("x_mean1","x_mean2")] %>% mutate(sampno=1:n()) %>% pivot_longer(-sampno)
conv_names <- c("x_mean1"="Virus A mean","x_mean2"="Virus B mean")
chains_plot_mean$name <- conv_names[chains_plot_mean$name]
colnames(chains_plot_mean)[2] <- "virus"

p1 <- ggplot() + 
  geom_density(data=chains_plot_mean,aes(x=value,fill=virus),alpha=0.5) + 
  stat_function(fun=dnorm,n=101,args=list(mean=15,sd=5),aes(linetype="Prior")) +
  geom_point(data=virus_dat,aes(x=x,col=Data,y=yshift)) +
  scale_x_continuous(limits=c(0,30),breaks=seq(0,30,by=5)) +
  scale_linetype_manual(values=c("Prior"="dashed")) +
  ylab("Density") +
  xlab("X value") +
  ggtitle("Posterior estimates for means of two distributions appear different,\neven though both datasets are simulated from the same distribution") +
  theme_classic() +
  theme(legend.position=c(0.75,0.6),
        plot.title=element_text(size=12))
p1

ggsave("posteriors.png",p1,width=10,height=6,dpi=300,units="in")
