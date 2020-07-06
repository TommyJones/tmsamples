#' Generate a Vector Following Zipf's Law.
#' @description Generate a vector following Zipf's law. Can be used as a
#'   parameter for a Dirichlet distribution downstream.
#' @param vocab_size Length of the returned vector.
#' @param magnitude Magnitude, sum total of the returned vector.
#' @param zipf_par Zipf's law parameter as described
#'   \href{http://mathworld.wolfram.com/ZipfsLaw.html}{here}.
#' @return Returns a numeric vector of length \code{vocab_size} whose entries
#'   have the shape described by Zipf's law and sum to \code{magnitude}.
#' @examples 
#' z <- generate_zipf(
#'        10000,
#'        100,
#'        1.78
#'      )
#' plot(log10(seq_along(z)), log10(z))
#' @export
generate_zipf <- function(
  vocab_size,
  magnitude,
  zipf_par = 1.78 # approximate empirical zipf distribution http://mathworld.wolfram.com/ZipfsLaw.html
) {

  # make a beta that follows zipf's law

  zipf_fun <- function(v, zipf_par) 1/(1:v * log(zipf_par * v) )

  zipf_law <- zipf_fun(
    v = vocab_size,
    zipf_par = zipf_par
  )

  # normalize to sum to one (rounding error makes this slightly larger than one)
  zipf_law <- zipf_law / sum(zipf_law)

  out <- zipf_law * magnitude

  names(out) <- paste0("w_", seq_along(out))

  out
}

#' Sample LDA Topic Model Parameters
#' @description Sample parameters simulating a topic model from Dirichlet
#'   distributions.
#' @param alpha Numeric vector of prior parameter for topics over documents. 
#' @param beta Numeric vector of prior parameter for words over topics
#' @param num_documents Number of documents to generate parameters for
#' @return Returns a list with theta and phi matrices. Theta represents
#'   probabilities of topics over documents. Phi represents probabilities of 
#'   words over topics.
#' @note Number of topics is inferred from the length of \code{alpha}.
#' @export
sample_parameters <- function(
  alpha,
  beta,
  num_documents
) {
  # check inputs

  # initialize some variables
  Nk <- length(alpha)

  # generate phi
  if (inherits(beta, "matrix")) {
    stop("matrix beta not yet supported. Please pass a numeric vector for beta")
  } else {
    phi <- gtools::rdirichlet(n = Nk, alpha = beta)
  }

  rownames(phi) <- paste("t", 1:Nk, sep="_")

  if (is.null(names(beta))){
    colnames(phi) <- paste("w", seq_len(ncol(phi)), sep = "_")
  } else {
    colnames(phi) <- names(beta)
  }

  # generate theta
  theta <- gtools::rdirichlet(n = num_documents, alpha = alpha)

  # theta <- t(theta)
  rownames(theta) <- paste("d", 1:num_documents, sep="_")
  colnames(theta) <- rownames(phi)

  # return the output
  return(list(theta = theta, phi = phi))

}



