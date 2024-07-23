import itertools
import math
import cmath
import scipy
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.patheffects as pe


def posterior(likelihood, prior, likelihood_not):
    return (likelihood * prior) / (prior * likelihood + (1 - prior) * likelihood_not)


def ellipse(s1, s2, r, q=0.95, n_points=10**5):
    p = scipy.stats.chi2.ppf(q, df=2)

    def _ellipse_point(y1, side):
        side_sign = 1 if side == "top" else -1
        point = (
            (1 - r**2)
            * s2**2
            * (
                (2 * r * y1) / ((1 - r**2) * s1 * s2)
                + side_sign
                * (2 * cmath.sqrt(-p * s1**2 + y1**2))
                / cmath.sqrt(-(s1**2) * s2**2 + r**2 * s1**2 * s2**2)
            )
            / 2
        )
        assert point.imag == 0
        return point.real

    # Compute the endpoints and then the points between them
    y1_range = math.sqrt(p) * s1
    y1s = list(np.linspace(-y1_range, y1_range, num=n_points // 2))[1:-1]

    return [
        y1s + list(reversed(y1s)),
        [_ellipse_point(y1, "bottom") for y1 in y1s]
        + [_ellipse_point(y1, "top") for y1 in reversed(y1s)],
    ]


# Set up grid
axis_endpoints = (-5, 5)
xys = np.array(list(itertools.product(np.linspace(*axis_endpoints, 10**2), repeat=2)))


def plot_distrs(
    ax, sigma1_nh, sigma2_nh, rho_h, rho_nh, ph=0.9, sigma1_h=1, sigma2_h=1, label=None
):
    cov_h = rho_h * sigma1_h * sigma2_h
    cov_nh = rho_nh * sigma1_nh * sigma2_nh

    yh = scipy.stats.multivariate_normal.pdf(
        xys, mean=[0, 0], cov=[[sigma1_h**2, cov_h], [cov_h, sigma2_h**2]]
    )
    ynh = scipy.stats.multivariate_normal.pdf(
        xys, mean=[0, 0], cov=[[sigma1_nh**2, cov_nh], [cov_nh, sigma2_nh**2]]
    )
    post = posterior(yh, ph, ynh)

    # Plot 1D likelihood intervals
    yh_q975 = scipy.stats.norm.ppf(0.975, 0, sigma1_h)
    ynh_q975 = scipy.stats.norm.ppf(0.975, 0, sigma2_h)
    for sign in [-1, 1]:
        ax.hlines(sign * yh_q975, *axis_endpoints, linestyle="dotted", color="black")
        ax.vlines(sign * ynh_q975, *axis_endpoints, linestyle="dotted", color="black")

    # Plot 2D likelihood regions
    ax.plot(*ellipse(sigma1_h, sigma2_h, rho_h), color="black")
    ax.plot(*ellipse(sigma1_nh, sigma2_nh, rho_nh), color="red")

    # Plot posterior probabilities
    post_plot = ax.pcolormesh(
        np.reshape(xys[:, 0], (100, 100)),
        np.reshape(xys[:, 1], (100, 100)),
        1 - np.reshape(post, (100, 100)),
        cmap=plt.colormaps["Reds"],
        vmin=0,
        vmax=1,
        linewidth=0,
        rasterized=True,
    )

    if label is not None:
        ax.text(
            -3.5,
            4.5,
            label,
            fontweight="bold",
            color="white",
            va="top",
            ha="right",
            path_effects=[pe.withStroke(linewidth=1, foreground="black")],
        )

    ax.axis("off")
    ax.set_box_aspect(1)

    return post_plot


def format_figure(fig, post_plot):
    cbar = fig.colorbar(post_plot, ax=axes, pad=0.02)
    cbar.ax.get_yaxis().set_ticks([0, 1])
    cbar.ax.get_yaxis().set_ticks_position("none")
    cbar.ax.set_ylabel(r"$p(H'|y_1, y_2)$", rotation=270)
    fig.supxlabel(r"$y_1$")
    fig.supylabel(r"$y_2$")


# Plot main cases
fig, axes = plt.subplots(1, 5, sharex="all", sharey="all", layout="compressed")
plot_distrs(axes[0], sigma1_nh=2, sigma2_nh=1, rho_h=0, rho_nh=0, label="A")
plot_distrs(axes[1], sigma1_nh=2, sigma2_nh=1.2, rho_h=0, rho_nh=0, label="B")
plot_distrs(axes[2], sigma1_nh=2, sigma2_nh=1, rho_h=0.5, rho_nh=0.5, label="C")
plot_distrs(axes[3], sigma1_nh=2, sigma2_nh=2, rho_h=0.5, rho_nh=0.5, label="D")
post_plot = plot_distrs(
    axes[4], sigma1_nh=2, sigma2_nh=2, rho_h=0.5, rho_nh=-0.9, label="E"
)
format_figure(fig, post_plot)
fig.set_size_inches(7, 1.5)
fig.savefig("figures/bayes_multivariate.pdf", dpi=300)

# Plot more cases
fig, axes = plt.subplots(2, 2, sharex="all", sharey="all", layout="compressed")
plot_distrs(axes[0, 0], sigma1_nh=2, sigma2_nh=2, rho_h=0, rho_nh=0.5, label="A")
plot_distrs(axes[0, 1], sigma1_nh=2, sigma2_nh=2, rho_h=0.5, rho_nh=0, label="B")
plot_distrs(axes[1, 0], sigma1_nh=2, sigma2_nh=1, rho_h=0, rho_nh=0.5, label="C")
post_plot = plot_distrs(
    axes[1, 1], sigma1_nh=2, sigma2_nh=1, rho_h=0.5, rho_nh=0, label="D"
)
format_figure(fig, post_plot)
fig.set_size_inches(4.75, 3.75)
fig.savefig("figures/bayes_multivariate_more.pdf", dpi=300)
