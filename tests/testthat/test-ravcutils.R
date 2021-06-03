# Test rm_NA_vars -------------------------------------------------------------
test_that("rm_NA_vars works", {
  df <- data.frame(col1 = c(1, NA, 3, NA, 5, NA),
                   col2 = c(11, 12, NA, 14, 15, 16),
                   col3 = c(21, 22, 23, 24, 25, 26))
  df1 <- rm_NA_vars(df)
  df2 <- rm_NA_vars(df, threshold = 0.1)
  df3 <- rm_NA_vars(df, threshold = 0.7)
  expect_equal(dim(df1)[[1]], dim(df)[[1]])
  expect_equal(dim(df2)[[1]], dim(df)[[1]])
  expect_equal(dim(df3)[[1]], dim(df)[[1]])
  expect_equal(sort(names(df1)), c("col2", "col3"))
  expect_equal(sort(names(df2)), c("col3"))
  expect_equal(sort(names(df3)), c("col1", "col2", "col3"))
  expect_equal(df1[,"col2"], df[,"col2"])
  expect_equal(df1[, "col3"], df[,"col3"])
})

# Test rm_nonvar_vars ---------------------------------------------------------
test_that("rm_nonvar_vars works", {
  df <- data.frame(col1 = c(1.001, 0.9998, 1, 1.00003),
                   col2 = c(10.0, 14.0, 2.0, 20.0),
                   col3 = c("yes", "yes", "yes", "yes"),
                   col4 = c(1.1, 0.8, 0.95, 1.07))
  df1 <- rm_nonvar_vars(df, threshold = 1e-2)
  df2 <- rm_nonvar_vars(df, threshold = 0.2)
  expect_equal(dim(df1)[[1]], dim(df)[[1]])
  expect_equal(dim(df2)[[1]], dim(df)[[1]])
  expect_equal(df1[,"col2"], df[,"col2"])
  expect_equal(df2[,"col2"], df[,"col2"])
  expect_equal(sort(names(df1)), c("col2", "col3", "col4"))
  expect_equal(sort(names(df2)), c("col2", "col3"))
})

# Test n_pca_var_level --------------------------------------------------------
test_that("n_pca_var_level works", {
  # 50 % of var
  x1 <- rnorm(100, sd = sqrt(0.5))
  # 20% of var (70% cum)
  x2 <- rnorm(100, sd = sqrt(0.2))
  # 15% of var (85% cum)
  x3 <- rnorm(100, sd = sqrt(0.15))
  # 11% of var (96% cum)
  x4 <- rnorm(100, sd = sqrt(0.11))
  # 2% of var (98% cum)
  x5 <- rnorm(100, sd = sqrt(0.02))
  # 1.5% of var (99.5% cum)
  x6 <- rnorm(100, sd = sqrt(0.015))
  # 0.5 % of var (100% cum)
  x7 <- rnorm(100, sd = sqrt(0.005))
  X <- cbind(x1, x2, x3, x4, x5, x6, x7)
  # Generate random rotation matrix
  Q <- qr.Q(qr(matrix(runif(49), nrow = 7)))
  # "Data" matrix
  Y <- X %*% Q
  pca <- prcomp(Y)
  n <- n_pca_var_level(pca, var_level = 0.95)
  expect_equal(n, 4)
  n <- n_pca_var_level(pca, var_level = 0.99)
  expect_equal(n, 6)
})


# Test from_pca_to_original ---------------------------------------------------
test_that("from_pca_to_original works", {
  # 50 % of var
  x1 <- rnorm(100, sd = sqrt(0.5))
  # 20% of var (70% cum)
  x2 <- rnorm(100, sd = sqrt(0.2))
  # 15% of var (85% cum)
  x3 <- rnorm(100, sd = sqrt(0.15))
  # 11% of var (96% cum)
  x4 <- rnorm(100, sd = sqrt(0.11))
  # 2% of var (98% cum)
  x5 <- rnorm(100, sd = sqrt(0.02))
  # 1.5% of var (99.5% cum)
  x6 <- rnorm(100, sd = sqrt(0.015))
  # 0.5 % of var (100% cum)
  x7 <- rnorm(100, sd = sqrt(0.005))
  X <- cbind(x1, x2, x3, x4, x5, x6, x7)
  # Generate random transformation matrix
  Q <- matrix(runif(49), nrow = 7)
  mu <- runif(7, min = 1, max = 5)
  # "Data" matrix
  Y <- apply_rowop(X %*% Q, `+`, mu)
  pca <- prcomp(Y, center = TRUE, scale. = TRUE, retx = TRUE)
  istart <- runif(1, min = 1, max = dim(Y)[[1]] - 10)
  obs_pca <- pca$x[istart:istart+10,]
  obs <- Y[istart:istart+10,]
  expect_equal(from_pca_to_original(pca, obs_pca), obs)
})
