set.seed(1)
x = rnorm(20)
y = rnorm(20)

# Figure 1
plot(x, type="l", col = "red", lwd = 2)
lines(y, col = "blue", lwd = 2)
make_phantom(text = c("Red", "Blue"), display = 1:2, colors = c("red", "blue"), sep = "  ")

# Figure 2
par(mfrow = c(1, 2))
plot(x, type="l", col = "red", lwd = 2)
make_phantom(text = c("Red", "Blue"), display = 1, colors = c("red", "blue"), sep = "  ")

plot(y, type="l", col = "blue", lwd = 2)
make_phantom(text = c("Red", "Blue"), display = 2, colors = c("red", "blue"), sep = "  ")
par(mfrow = c(1, 1))
# Notice the position of the text in the two titles compared with that of Figure 1
