x = rnorm(100)
y = rnorm(100, 1)
hpd.x = hpd_uni(x)
hpd.y = hpd_uni(y)

# x and y on their own plots
plot_hpd(x, col1 = "dodgerblue")
plot_hpd(y, col1 = "firebrick1")

# together on one plot
plot_hpd(x, col1 = "dodgerblue", fade = 0.6)
plot_hpd(y, col1 = "firebrick1", fade = 0.6, add = TRUE)
