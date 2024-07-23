library(tidyr)
library(ggplot2)

post <- function(yh, ynh, ph) (yh*ph)/(yh*ph + ynh*(1-ph))

n <- 10^5
ylim <- c(-10, 10)
dcurve <- function(expr) eval(substitute(curve(expr, ylim[1], ylim[2], n=n)))

make_plot_df <- function(dh_curve, dnh_curve, priors=0.9) {
  plot_df <- data.frame(dh_curve$x, dh_curve$y, dnh_curve$y)
  post_names <- c()
  for (i in 1:length(priors)) {
    plot_df <- cbind(plot_df, post(dh_curve$y, dnh_curve$y, priors[i]))
    post_names <- c(post_names, paste0("post", i))
  }
  names(plot_df) <- c("y", "H", "notH", post_names)
  pivot_longer(plot_df, -y, names_to="distr")
}

# Main example
plot_df <- make_plot_df(dcurve(dnorm(x)),
                        dcurve(0.8 * dnorm(x, -2) + 0.2 * dnorm(x, 1)),
                        c(0.9, 0.5))
line_breaks <- c("H", "notH", "post2", "post1")
line_labels <- c(expression(paste(italic("p"), "(", italic("y"), "|", italic("H"), ")")),
                 expression(paste(italic("p"), "(", italic("y"), "|", italic("H'"), ")")),
                 expression(paste(italic("p"), "(", italic("H"), "|", italic("y"), ") if ", italic("p"), "(", italic("H"), ")=0.5")),
                 expression(paste(italic("p"), "(", italic("H"), "|", italic("y"), ") if ", italic("p"), "(", italic("H"), ")=0.9")))
ggplot(plot_df) +
  geom_line(aes(x=y, y=value, color=distr, linetype=distr)) +
  scale_y_continuous(n.breaks=5, labels=c("0", "1/4", "1/2", "3/4", "1")) +
  scale_color_manual(breaks=line_breaks, labels=line_labels,
                     values=c("blue", "red", "black", "black")) +
  scale_linetype_manual(breaks=line_breaks, labels=line_labels,
                        values=c(1, 1, 3, 2)) +
  labs(x=expression(italic("y")), y=NULL) +
  theme_classic() +
  theme(legend.position=c(0.2, 0.8), legend.title=element_blank(),
        legend.text=element_text(hjust=0), legend.margin=margin(c(0,0,0,0)))
ggsave("figures/bayes_univariate.pdf", width=4, height=3.25)

# More examples
plot_df1 <- make_plot_df(dcurve(dnorm(x, 0, 1)), dcurve(dnorm(x, 0.5, 0.5)))
mode_mean <- 4
plot_df2 <- make_plot_df(dcurve(0.5 * dnorm(x, -mode_mean, 0.5) + 0.5 * dnorm(x, mode_mean, 0.5)),
                         dcurve(0.5 * dnorm(x, -mode_mean, 0.7) + 0.5 * dnorm(x, mode_mean, 0.7)))
plot_df1$facet <- "A"
plot_df2$facet <- "B"
ggplot(rbind(plot_df1, plot_df2)) +
  geom_line(aes(x=y, y=value, color=distr)) +
  facet_grid(~facet) +
  scale_y_continuous(n.breaks=5, labels=c("0", "1/4", "1/2", "3/4", "1")) +
  scale_color_manual(values=c("blue", "red", "black"),
                     labels=c(expression(paste(italic("p"), "(", italic("y"), "|", italic("H"), ")")),
                              expression(paste(italic("p"), "(", italic("y"), "|", italic("H'"), ")")),
                              expression(paste(italic("p"), "(", italic("H"), "|", italic("y"), ")")))) +
  labs(x=expression(italic("y")), y=NULL) +
  theme_classic() +
  theme(strip.background=element_blank(),
        strip.text.x=element_text(face="bold", hjust=0, margin=margin(l=0)),
        legend.position=c(0.08, 0.6), legend.title=element_blank(),
        legend.text=element_text(hjust=0), legend.margin=margin(c(0,0,0,0)),
        text=element_text(size=15))
ggsave("figures/bayes_univariate_more.pdf", width=7, height=3.25)
