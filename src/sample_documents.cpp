// Export this as a header for use in other packages
// [[Rcpp::interfaces(r, cpp)]]

#include "matrix_conversions.h"
#include "sample.h"

// // [[Rcpp::depends(dqrng, BH, sitmo)]]
#include <xoshiro.h>
#include <dqrng_distribution.h>
// // [[Rcpp::plugins(cpp11)]]

// #include <RcppArmadillo.h>
#define ARMA_64BIT_WORD

#include <RcppThread.h>

using namespace Rcpp;


//' Sample a DTM or TCM based on hierarchical multinomial parameters
//' @keywords internal
//' @description
//'   Sample a sparse DTM or TCM based on hierarchical multinomial paramers.
//' @param theta Matrix where each row is a document, each column is a topic and
//'   the i,j entries represent the probability of sampling topic j from doc i.
//' @param phi Matrix where each row is a topic and each column is a token. The
//'   i,j entries represent the probability of sampling token j from topic i.
//' @param doc_lengths Integer vector of document lengths. Must be the same length
//'   as rows of \code{theta}.
//' @param threads Integer number of parallel threads, defaults to 1.
//' @param verbose Boolean; do you want to print a simple progress bar out to the console?
//' @param seed Integer for random seed setting
//' @return Returns a sparse matrix whose rows represent documents, columns
//'   represent tokens and i,j entries are the count of token j in document i.
// [[Rcpp::export]]
arma::sp_mat sample_documents_c(
  const NumericMatrix theta,
  const NumericMatrix phi,
  const std::vector<std::size_t> doc_lengths,
  const int seed,
  const bool verbose = true,
  std::size_t threads = 1 // not constant in case user provided is wrong
) {

  // check consistency of inputs
  if ( theta.rows() != doc_lengths.size())
    stop("nrow(theta) must equal length(doc_lengths)");

  if (threads > doc_lengths.size())
    threads = doc_lengths.size();

  // generate some variables
  auto Nd = theta.rows();
  auto Nv = phi.cols();

  auto phi_list = mat_to_vec(phi, true);
  auto theta_list = mat_to_vec(theta, true);

  arma::sp_mat out(Nv, Nd);
  
  dqrng::xoshiro256plus rng(seed);

  // parallel loop over documents to fill in counts
  RcppThread::parallelFor(
    0,
    Nd,
    [&](std::size_t d) {

      // get a thread-specific random seed
      dqrng::xoshiro256plus lrng(rng);      // make thread local copy of rng 
      lrng.long_jump(d + 1);  // advance rng by 1 ... ncores jumps
      
      // initialize a couple variables
      int z; // placeholder for topic sampled

      int w; // placeholder for token sampled
      
      double u; // placeholder for uniform random var

      dqrng::uniform_distribution unif(0.0, 1.0); // Uniform distribution [0,1)

      // loop for that document's length
      for (std::size_t n = 0; n < doc_lengths[d]; n++) {

        // sample a topic, z
        u = unif(lrng);
        
        z = sample_one(theta_list[d], u);

        // given z, sample a token w
        u = unif(lrng);
        
        w = sample_one(phi_list[z], u);

        // add the w, d term in the output matrix
        out(w, d) = out(w, d) + 1;

      } // end loop over tokens
      
      RcppThread::checkUserInterrupt();
      
      // progress bar
      if (verbose) {
        RcppThread::Rcout << "=";
      }

    }, // end parallel loop over documents
  threads);

  // Prepare outputs and expel from the function
  out = out.t();

  return out;

}


