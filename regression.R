library(bindata)
library(rstanarm)
options(mc.cores=parallel::detectCores())
library(dplyr)
library(tidyr)
library(ggplot2)

interval_width <- function(y) as.numeric(quantile(y, 0.975) - quantile(y, 0.025))

y_df <- data.frame()
append_result <- function(postp, info) {
  y_df <<- rbind.data.frame(y_df, c(median(postp), interval_width(postp), info))
}
post_pred <- function(model) posterior_predict(model, newdata=data.frame(X1=c(1), X2=c(1)))

# Set parameters
set.seed(42)
coefs <- c(1, 0)
p_x <- c(0.5, 0.05)
r_error <- function(n) rnorm(n, 0, 0.75)

# Construct data generator
rdata <- function(n) {
  X <- rmvbin(n, margprob=p_x)
  y <- X %*% coefs + r_error(n)
  data <- as.data.frame(cbind(X, y))
  colnames(data) <- c(paste0("X", 1:length(coefs)), "y")
  data
}

# Get true distributions
data_much <- rdata(10^6)
append_result(data_much$y, c("y", "true", "none"))
postp <- filter(data_much, X1==1)$y
append_result(postp, c("y_x1", "true", "none"))
append_result(postp, c("y_x1_x2", "true", "none"))

make_model <- function(formula, data, prior) {
  stan_glm(formula, data=data,
           prior=prior,
           prior_intercept=normal(0, 10),
           prior_aux=exponential(1),
           seed=42, refresh=0, iter=10^4)
}

for (i in 1:25) {
  data <- rdata(100)
  
  # Don't condition on any covariates
  append_result(post_pred(make_model(y ~ 1, data, c())),
                c("y", "postp", "wide"))
  
  # Condition on informative feature
  append_result(post_pred(make_model(y ~ X1, data, normal(0, 10))),
                c("y_x1", "postp", "wide"))
  
  # Condition on both features under different priors
  append_result(post_pred(make_model(y ~ ., data, normal(0, 10))),
                c("y_x1_x2", "postp", "wide"))
  append_result(post_pred(make_model(y ~ ., data, normal(c(0, -1), c(10, 0.25)))),
                c("y_x1_x2", "postp", "wrong"))
  append_result(post_pred(make_model(y ~ ., data, normal(c(0, 0), c(10, 0.25)))),
                c("y_x1_x2", "postp", "good"))
}

# Format results dataframe
colnames(y_df) <- c("median", "range", "distr", "type", "prior")
y_df <- y_df %>%
  mutate(median=as.numeric(median), range=as.numeric(range), distr=factor(distr)) %>%
  mutate(distr=recode_factor(distr,
                             `y`="No features modeled",
                             `y_x1`="Informative feature\nmodeled",
                             `y_x1_x2`="Informative and spurious\nfeatures modeled"))

# Plot
ggplot(y_df %>%
         mutate(prior=factor(prior, levels=c("wide", "wrong", "good", "none"))) %>%
         arrange(prior) %>%
         mutate(prior=factor(prior, levels=c("none", "wide", "wrong", "good"))) %>%
         mutate(prior=recode_factor(prior,
                                    `none`="True",
                                    `wide`="Inferred,\nwide priors",
                                    `wrong`="Inferred, biased prior\non spurious feature",
                                    `good`="Inferred, regularizing\nprior on spurious feature"))) +
  geom_point(aes(x=median, y=range, color=prior, shape=prior, size=prior),
             alpha=0.85, fill="darkgrey") +
  facet_grid(~distr) +
  scale_color_manual(values=c("black" , "blue", "red", "darkgreen")) +
  scale_shape_manual(values=c(23,16,4,17)) +
  scale_size_manual(values=c(3,1.5,1.5,1.5)) +
  labs(x="Median of reference distribution", y="Width of reference interval") +
  theme_classic() +
  theme(legend.title=element_blank(), legend.position="bottom",
        strip.background=element_blank(),
        text=element_text(size=11), axis.title=element_text(size=10))
ggsave("figures/regression_intervals.pdf", width=6, height=3)
