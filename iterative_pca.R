library(R.utils)
library(doParallel)
library(foreach)
library(data.table)
library(stats)
#### url <- "Data/2022-12-07_MEH_DTM_Binary.csv"
#### rows <- 10000 # setting number of rows for iterations
#### num_rows <- fread(url,
####                   select = c("Filename"))
#### 
#### num_rows <- nrow(num_rows)
#### 
#### ## STEP I: Calculate sum of squares
#### # Set up file connection and read in first chunk of data
#### con <- file(url, "r")
#### data_chunk <- read.csv(con, nrows = rows)
#### nvar <- ncol(data_chunk)
#### # Calculate sum of squares per column
#### sos_column <- colSums(data_chunk^2)
#### 
#### # Loop over remaining chunks of data and update sum of squares
#### while (nrow(data_chunk) > 0) {
####   data_chunk <- read.csv(con, nrows = rows)
####   if (nrow(data_chunk) > 0) {
####     sos_column <- sos_column + colSums(data_chunk^2)
####   }
#### }
#### 
#### # Calculate the mean values per column
#### num_rows <- (nrow(data_chunk) + rows * (length(sos_column) - 1))
#### means_column <- colSums(data_chunk) / num_rows
#### 
#### # Close the file connection
#### close(con)
#### 
#### ### EDIT TO REMOVE FIRST FOUR COLUMNS
#### 
#### means_column <- means_column[5:nvar]
#### sos_column <- sos_column[5:nvar]

## STEP II: Calculate the covariance matrix

# Open file connection
# UNFORTUNATELY, file connections and parallel processing did NOT get along, 
# so I have to use the older version of read.csv, skip and a forloop
# con <- file(url, "r")
# open(con)
# seek(con, 0)

load("num_rows.Rdata")
nvar = 4299
url <- "Data/2022-12-07_MEH_DTM_Binary.csv"
rows <- 1000 

# Set the number of cores to use
num_cores <- detectCores()

# Set counters and empty matrix for covmat
iter <- 0
covmat <- matrix(0, nrow = nvar, ncol = nvar)

# Set up parallel processing
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Run the iterations in parallel
foreach(i = ceiling(num_rows/rows), .packages = c("stats")) %dopar% {
  skip_rows <- (i-1) * rows
  data_chunk <- read.csv(url, 
                         nrows = rows, 
                         skip = skip_rows + 1)[,-c(1:4)]
  crossprod_chunk <- crossprod(scale(data_chunk, 
                                     center = means_column, 
                                     scale = FALSE))
  covmat <- covmat + crossprod_chunk
  iter <- rows*i
  print(iter)
}

# Stop parallel processing
stopCluster(cl)

covmat <- covmat / (num_rows - 1)

# Close the file connection
close(con)

# STEP III Calculate PCA using princomp with the pre-calculated covariance matrix
pca_results <- princomp(covmat)

plot(pca_results, type = "line")
 
save(con, data_chunk, sos_column, 
     means_column, covmat, crossprod_chunk, 
     pca_results, num_rows, file = "cc_pca_parallel.Rdata")

# ## STEP IV DETERMINE NUMBER OF COMPONENTS
# 
# plot(pca_results, type = "line")
# 
# x <- loadings(pca_results)
# 
# x <- matrix(x, nrow = 4303)
# 
# ?prcomp




