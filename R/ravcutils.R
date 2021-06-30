#' ravcutils: A collection of utils for data processing automation.
#'
#' A collection of useful functions to automate data processing and
#' aid ML application for processing industry applications.
#'
#' @docType package
#' @name ravcutils
#' @importFrom stats sd
#' @importFrom utils head tail
NULL

# Remove variables with too many NAs -------------------------------------------
#' Removes variables from dataframe with too many missing values
#'
#' \code{rm_NA_vars} returns a dataframe with only those variables whose portion
#' of missing values is below the specified threshold.
#'
#' @param df A dataframe (or a list).
#' @param threshold Portion of missing values above which a variable will be
#'     excluded from resulting dataframe.
#' @return A dataframe or a list (depending on \code{df} argument).
#'
#' @examples
#' df <- data.frame(a = c(1, NA, NA, 2), b = c(10, NA, 20, 30))
#' rm_NA_vars(df)
#' rm_NA_vars(df, threshold = 0.1)
#' @export
rm_NA_vars <- function(df, threshold = 0.3) {
  vars <- names(df)
  vars_to_keep <- c()
  for (v in vars) {
    if (sum(is.na(df[[v]])) / dim(df)[[1]] < threshold) {
      vars_to_keep <- c(vars_to_keep, v)
    }
  }
  return(df[vars_to_keep])
}

# Remove non-varying variables -------------------------------------------------
#' Removes numeric variables that vary too little
#'
#' \code{rm_nonvar_vars} returns a dataframe with only those variables from the
#' original dataframe that are either non-numeric or have relative standard
#' deviation above the specified threshold.
#'
#' @param df A dataframe (or a list)
#' @param threshold A scalar, standard deviation threshold for excluding variables
#' @param scale A scalar or a vector, the value by which to scale standard
#'     deviation. Use \code{NA} to use mean for scale. If scalar, this scale
#'     value applies to all variables. Otherwise, the vector must be at least
#'     as long as the number of variables in the dataframe.
#' @return A dataframe with non-varying numeric variables removed.
#'
#' @examples
#' df <- data.frame(a = c(1.001, 0.9998, 1, 1.00003), b = c(10., 14., 2., 20.),
#'                  f = c("yes", "yes", "yes", "yes"))
#' rm_nonvar_vars(df, threshold = 1e-2)
#' @export
rm_nonvar_vars <- function(df, threshold = 1e-3, scale = NA) {
  nvar <- dim(df)[[2]]
  vars_to_keep <- c()
  # Actual scale to use
  s <- rep(0, nvar)
  # Make scale a vector if necessary
  if (length(scale) == 1) {
    scale <- rep(scale, nvar)
  }
  for (i in 1:nvar) {
    if (is.numeric(df[,i])) {
      std <- sd(df[,i], na.rm = TRUE)
      if (is.na(scale[[i]])) {
        std <- std / mean(df[,i], na.rm = TRUE)
      } else {
        std <- std / scale[[i]]
      }
      if (std >= threshold) vars_to_keep <- c(vars_to_keep, i)
    } else {
      # keep non-numeric variables
      vars_to_keep <- c(vars_to_keep, i)
    }
  }
  return(df[,vars_to_keep])
}

# Compute No of PCA components -------------------------------------------------
#' Computes number of PCA components needed to explain variance
#'
#' \code{n_pca_var_level} computes number of PCA components needed to explain
#' variance not below the specified level.
#'
#' @param pca Is an object returned by \code{\link{stats}{prcomp}}
#' @param var_level Level of variance.
#' @return Number of components that explain at least \code{var_level} of data
#'     variability
#'
#' @examples
#' x <- rnorm(100)
#' y <- rgamma(100, shape = 0.5)
#' z <- rnorm(100, sd = 2.0)
#' x1 <- x + y
#' x2 <- x - 2 * y + z
#' x3 <- y + z
#' x4 <- x + y + z
#' A <- cbind(x1, x2, x3, x4)
#' pca <- prcomp(A, scale. = TRUE)
#' n_pca_var_level(pca)
#' @export
n_pca_var_level <- function(pca, var_level = 0.95) {
  vars <- cumsum(pca$sdev^2)
  tot_var <- tail(vars, 1)
  rvars <- vars / tot_var
  head(which(rvars >= var_level), 1)
}

# Convert PCA-space point to original variables space --------------------------
#' Converts PCA-space point to original variables space
#'
#' \code{from_pca_to_original} converts observations given in (latent) PCA-space
#' into their original space, taking into account scaling and centering.
#'
#' @param pca is an object returned by \code{\link[stats]{prcomp}}
#' @param x is a vector or a matrix. It can have less components than full PCA
#'     transformation.
#' @return Returns a point in the original space
#' @export
from_pca_to_original <- function(pca, x) {
  ncomp <- if (is.null(dim(x))) length(x) else dim(x)[[2]]
  # Rotate; result is always a matrix
  y <- x %*% t(pca$rotation[,1:ncomp])
  if (is.numeric(pca$scale)) {
    for (i in 1:dim(y)[[2]]) {
      y[,i] <- y[,i] * pca$scale[[i]]
    }
  }
  if (is.numeric(pca$center)) {
    for (i in 1:dim(y)[[2]]) {
      y[,i] <- y[,i] + pca$center[[i]]
    }
  }
  if (dim(y)[[1]] == 1) return(y[1,]) else return(y)
}

# Row operatrion on matrix -----------------------------------------------------
#' Applies row-wise operation to a matrix and a vector
#'
#' \code{apply_rowop} applies operation \code{op} to each row of the matrix as
#' its first argument and to the vector (treated as a row) as its second
#' argument.
#'
#' @param matrix Matrix
#' @param op operation to apply. Use backquotes for operators: \code{`+`}.
#' @param row vector treated as a single row.
#' @export
apply_rowop <- function(matrix, op, row) {
  nrows <- dim(matrix)[[1]]
  m <- matrix
  for (irow in 1:nrows) {
    m[irow, ] <- op(matrix[irow, ], row)
  }
  return(m)
}

# Standardize data matrix ------------------------------------------------------
#' Standardizes numerical matrix
#'
#' @param M Matrix to standardize
#' @param zero_nonvar_col Logical switch to decide what to do with zero varying
#'                        columns. If `TRUE`, these columns are zeroed,
#'                        otherwise, `NaN`'s will be returned.
#' @param full_result Logical switch to determine what kind of result is
#'                    required. If `TRUE` returns the list with standardized
#'                    matrix, column means and standard deviations. Otherwise,
#'                    only matrix is returned.
#'
#' @return Matrix with each column having mean zero and variance one. If
#'         `full_result` is `TRUE` return the list containing this matrix and
#'         original matrix column means and standard deviations. Helps to
#'         convert the data back to original variables.
#' @export
standardize <- function(M, zero_nonvar_col = TRUE, full_result = FALSE) {
  ncol <- dim(M)[[2]]
  means <- colMeans(M)
  s <- sapply(1:ncol, function(i) { sd(M[,i]) })
  # Need a copy in case returning full result
  s2 <- s
  # Avoid division by zero
  if (zero_nonvar_col) { s2[s2 == 0] <- 1 }
  A <- apply_rowop(M, `-`, means)
  A <- apply_rowop(A, `/`, s2)
  if (full_result) {
    return(list(
      matrix = A,
      means = means,
      std.devs = s
    ))
  } else {
    return(A)
  }
}

# Dunn Index for clustering ----------------------------------------------------
#' Dunn index to select cluster numbers
#'
#' Optimal selection is done for the number of clusters that maximize the index
#'
#' @param data matrix of observations
#' @param classification vector of classiffication labels for each observation
#' @param centroids Matrix whose each row is the centroid of a cluster
#' @param stdev vector of standard deviations for each variable
#' @export
dunn_index <- function(data, classification, centroids, stdev) {
  # Calculate distance between centroids
  dc <- Inf
  s <- (1 / stdev) ^ 2
  nclust <- length(centroids[,1])
  for (i in 1:(nclust - 1)) {
    for (j in (i + 1):nclust) {
      dcandidate <- (centroids[i,] - centroids[j,]) %*%
        (s * (centroids[i,] - centroids[j,]))
      dc <- min(c(dc, dcandidate))
    }
  }
  dc <- sqrt(dc)
  # Calculate sizes of the clusters
  dcluster <- 0
  for (icluster in 1:nclust) {
    xcluster <- data[classification == icluster,]
    cluster_size <- length(xcluster[,1])
    for (iobs in 1:(cluster_size - 1)) {
      for (jobs in (iobs + 1):cluster_size) {
        dcandidate <- (xcluster[iobs,] - xcluster[jobs,]) %*%
          (s * (xcluster[iobs,] - xcluster[jobs,]))
        # Here it doesn't matter that we might compare between different clusters - max is transitive
        # if the metric of the cluster size were different we'd have to compute the cluster size first
        # and then compare sizes of the clusters
        dcluster <- max(dcluster, dcandidate)
      }
    }
  }
  dcluster <- sqrt(dcluster)
  dc / dcluster
}

# Volume of distribution -------------------------------------------------------
#' Computes "volume" of distribution
#'
#' \code{vol_dist} gives an idea of the spread of distribution by computing its
#' "volume". Distribution is assumed to be Gaussian with covariance matrix
#' \code{cov.}. Then the volume of the n-dimensional ellipsoid, defined by this
#' covariance matrix is computed. This function may be useful for clustering to
#' assess the size of the cluster.
#'
#' @param cov. Covariance matrix of the distribution.
#' @return "Volume" of the ellipsoid.
#' @export
vol_dist <- function(cov.) {
  n <- dim(cov.)[[1]]
  a <- eigen(cov., TRUE)$values
  coeff <- pi^(n/2) / gamma(n/2+1)
  return(coeff * prod(a))
}


# Compute number of observation in each group ----------------------------------
#' Finds the count of each observation value (assuming discrete values)
#'
#' @param obs vector of discrete observations
#' @return The list of observations counted by group. Each list item is the list
#'     with (at least) fields \code{value} and \code{count}.
#' @export
count_by_group <- function(obs) {
  groups <- list()

  for (x in obs) {
    found <- FALSE
    foundv <- list()
    for (y in groups) {
      if (y[["value"]] == x) {
        foundv <- y
        found <- TRUE
        break
      }
    }
    if (found) {
      foundv[["count"]] <- foundv[["count"]] + 1
      # replace
      groups[[foundv[["index"]]]] <- foundv
    } else {
      i <- length(groups) + 1
      groups[[i]] <- list(value = x, count = 1, index = i)
    }
  }
  # Transpose the data
  n <- length(groups)
  df <- data.frame(value = rep(groups[[1]]$value, n), count = rep(0, n))
  for (item in groups) {
    df[item$index,] <- item[c("value", "count")]
  }
  return(df)
}


# Convert Excel date serial number to date/time --------------------------------
#' Converts Excel date serial number to POSIXct
#'
#' When date/time field is loaded from Excel it is often in numeric format.
#' This function converts it to POSIX date/time.
#' @param x Date serial number (from Excel)
#' @return POSIX date/time
#' @export
from_excel_date_number <- function(x) {
  as.POSIXct(x * (60*60*24),
             origin="1899-12-30",
             tz="GMT")
}


# Get better palette -----------------------------------------------------------
#' Returns a palette with pleasant colors for plots
#'
#' Returns a palette with 6 colors to use for plotting.
#' @return Vector of 6 colors
#' @export
ravc_palette <- function() {
  RColorBrewer::brewer.pal(n = 6, name = 'Dark2')
}
