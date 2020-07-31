#' Sample a DTM or TCM based on hierarchical multinomial parameters
#' @description
#'   Sample a sparse DTM or TCM based on hierarchical multinomial paramers.
#' @param theta Matrix where each row is a document, each column is a topic and
#'   the i,j entries represent the probability of sampling topic j from doc i.
#' @param phi Matrix where each row is a topic and each column is a token. The
#'   i,j entries represent the probability of sampling token j from topic i.
#' @param doc_lengths Integer vector of document lengths. Must be the same length
#'   as rows of \code{theta}.
#' @param threads Integer number of parallel threads, defaults to 1.
#' @param verbose Boolean; do you want to print a simple progress bar out to the console?
#' @return Returns a sparse matrix whose rows represent documents, columns
#'   represent tokens and i,j entries are the count of token j in document i.
#' @examples 
#' library(tmsamples)
#' 
#' Nk <- 4
#' Nd <- 50
#' Nv <- 1000
#' 
#' alpha <- rgamma(Nk, 0.5)
#' 
#' beta <- generate_zipf(vocab_size = Nv, magnitude = 500, zipf_par = 1.1)
#' 
#' pars <- sample_parameters(alpha, beta, Nd)
#' 
#' doc_lengths <- rpois(Nd, 50)
#' 
#' dtm <- sample_documents(
#'   theta = pars$theta,
#'   phi = pars$phi,
#'   doc_lengths = doc_lengths,
#'   verbose = TRUE, # verbose controls console output of progress
#'   threads = 1 # threads controls parallel computation
#' )
#' @export
sample_documents <- function(
  theta,
  phi,
  doc_lengths,
  verbose = TRUE,
  threads = 1
) {
  
  # check inputs for validity
  if (! inherits(theta, "matrix")) {
    stop("theta must be a matrix")
  }
  
  if (! inherits(phi, "matrix")) {
    stop("phi must be a matrix")
  }
  
  if (ncol(theta) != nrow(phi)) {
    stop("ncol(theta) must equal nrow(phi)")
  }
  
  if (! is.numeric(doc_lengths) || length(doc_lengths) != nrow(theta)) {
    stop("doc_lengths must be a numeric vector whose length is equal to nrow(theta)")
  }
  
  if (any(is.na(doc_lengths) | any(is.infinite(doc_lengths)))) {
    bad_values <- which(is.na(doc_lengths) | is.infinite(doc_lengths))
    
    stop("doc_lengths must not contain NA or Inf values. Check entries ",
         paste(bad_values, collapse = ", "), ".")
  }
  
  if (any(is.na(phi) | is.infinite(phi))) {
    stop("phi contains NA or Inf values")
  }
  
  if (any(is.na(theta) | is.infinite(theta))) {
    stop("theta contains NA or Inf values")
  }
  
  if (! is.numeric(threads)) {
    stop("threads must be numeric")
  }
  
  if (threads[1] <= 0) {
    stop("threads must be 1 or greater")
  }
  
  if (threads[1] > nrow(theta)) {
    message("threads > nrow(theta). This is more threads than objects. Setting threads = nrow(theta)")
    
    threads <- nrow(theta)
  }
  
  if (! is.logical(verbose)) {
    stop("verbose must be TRUE or FALSE")
  }
  
  if (length(verbose) == 0) {
    stop("verbose must be TRUE or FALSE")
  }
  
  if (length(verbose) > 1) {
    verbose <- verbose[1]
  }
  
  # clean up user passed values in case of any weirdness that may arise
  threads <- max(round(threads[1]), 1) # in case someone passes a decimal or vector
  
  doc_lengths <- round(doc_lengths)
  
  # get current state of R's RNG
  seed_pos <- .Random.seed[2]
  
  seed <- .Random.seed[seed_pos + 2]
  
  result <- sample_documents_c(
    theta = theta,
    phi = phi,
    doc_lengths = doc_lengths,
    seed = seed,
    verbose = verbose,
    threads = threads
  )
  
  # sample a throw-away number to move R's RNG state forward by 1
  runif(1)
  
  # format output DTM
  colnames(result) <- colnames(phi)
  rownames(result) <- rownames(theta)
  
  # return result
  result
}

