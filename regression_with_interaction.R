library(bindata)
library(rstanarm)
library(dplyr)
library(tidyr)
library(ggplot2)

list_to_long <- function(l) {
  df <- data.frame()
  for (name in names(l)) {
    vals <- as.numeric(l[[name]])
    df <- rbind(df, data.frame(name=rep(name, length(vals)), val=vals))
  }
  tibble(df)
}

# Set parameters
set.seed(42)
coefs <- c(1, 1, 1)
p_x <- c(0.5, 0.5)
r_error <- function(n) rnorm(n, 0, 0.75)

# Construct data generator
r_data <- function(n) {
  X <- rmvbin(n, margprob=p_x)
  X <- cbind(X, X[,1] * X[,2])
  y <- X %*% coefs + r_error(n)
  data <- as.data.frame(cbind(X, y))
  colnames(data) <- c(paste0("X", 1:length(coefs)), "y")
  data
}
data <- r_data(100)

# Get true distributions
data_much <- r_data(10^6)
y <- list(y__true=data_much$y,
          y_0_0__true=filter(data_much, X1==0, X2==0)$y,
          y_1_0__true=filter(data_much, X1==1, X2==0)$y,
          y_1_1__true=filter(data_much, X1==1, X2==1)$y)

# Run interaction-less model
model <- stan_glm(y ~ X1 + X2, data=data,
                  prior=normal(0, 10),
                  prior_intercept=normal(0, 10),
                  prior_aux=exponential(1),
                  seed=42, iter=10^4, refresh=0)
y$y_0_0__postp <- posterior_predict(model, newdata=data.frame(X1=c(0), X2=c(0)))
y$y_1_0__postp <- posterior_predict(model, newdata=data.frame(X1=c(1), X2=c(0)))
y$y_1_1__postp <- posterior_predict(model, newdata=data.frame(X1=c(1), X2=c(1)))

# Format results
y_df <- list_to_long(y) %>%
  separate("name", c("distr", "type"), sep="__")

# Compute interval statistics
ci_levels <- c("y true", "y_0_0 true", "y_0_0 postp", "y_1_0 true", "y_1_0 postp",
               "y_1_1 true", "y_1_1 postp")
y_stats_df <- y_df %>%
  group_by(distr, type) %>%
  summarize(q025=quantile(val, 0.025), q500=median(val), q975=quantile(val, 0.975)) %>%
  pivot_longer(cols=c(q025, q500, q975), names_to="quantile") %>%
  mutate(ci=factor(paste(distr, type), levels=rev(ci_levels)))

# Plot reference intervals
ggplot(y_stats_df, aes(x=ci, y=value, group=ci, color=distr)) +
  geom_line(aes(linewidth=ifelse(type=="true", 2, 0.8)), show.legend=F) +
  geom_point(aes(y=value), data=filter(y_stats_df, quantile=="q500"), size=3, show.legend=F) +
  scale_y_continuous(breaks=-2:4) +
  scale_linewidth_identity() +
  scale_color_brewer(palette="Set1") +
  labs(y="Test result", tag="A") +
  coord_flip() +
  theme_bw() +
  theme(panel.grid.major.y=element_blank(), panel.grid.minor.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks=element_blank(),
        plot.margin=unit(c(-0.25, 0.1, 0.1, 0.1), "cm"), panel.border=element_blank(),
        plot.tag.position=c(0.03, 0.94),
        text=element_text(size=15))
ggsave("figures/regression_with_interaction_intervals.pdf", width=4, height=4)

# Plot coefficient statistics
coef_stats_df <- data.frame(param=names(model$coefficients),
                            mean=as.numeric(model$coefficients),
                            se=as.numeric(model$ses))
ggplot(coef_stats_df, aes(x=param, y=mean)) +
  geom_bar(stat="identity", width=1, color="black", fill="grey95") +
  scale_x_discrete(labels=c(expression(beta[0]), expression(beta[1]), expression(beta[2]))) +
  geom_hline(yintercept=1) +
  geom_hline(yintercept=0) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9)) +
  scale_y_continuous(breaks=c(0, 0.5, 1, 1.5),
                     labels=c(expression(beta[0]==0), 0.5,
                              expression({{beta[1]==beta[2]}==beta[3]}==1), 1.5)) +
  labs(x="Coefficient", y="Inferred value", tag="B") +
  theme_classic() +
  theme(axis.ticks.x=element_blank(),
        legend.title=element_blank(), legend.position="top",
        plot.tag.position=c(0, 0.98),
        text=element_text(size=15))
ggsave("figures/regression_with_interaction_coefs.pdf", width=4, height=4)
