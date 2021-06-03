x <- rnorm(100)
y <- rnorm(100, sd = 1.5)
z <- rnorm(100, sd = 0.6)
x1 <- x + y + rnorm(100, sd = 0.3)
x2 <- x - 2 * y + z + rnorm(100, sd = 0.2)
x3 <- y + z + rnorm(100, sd = 0.3)
x4 <- x + y + z
A <- cbind(x1, x2, x3, x4)
pca <- prcomp(A, scale. = TRUE)
summary(pca)

n_pca_var_level(pca)

xx <- pca$x[21:25,]

from_pca_to_original(pca, xx[,1:3])

A[21:25, ]

APCA <- apply_rowop(apply_rowop(A, `-`, pca$center), `/`, pca$scale) %*% pca$rotation



head(APCA - pca$x)

(xx %o% pca$rotation[,1:3])

xx %*% t(pca$rotation[,1:3])


pca$center
