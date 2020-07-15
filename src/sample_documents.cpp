// Export this as a header for use in other packages
// [[Rcpp::interfaces(r, cpp)]]

#include "sample_int.h"
#include "matrix_conversions.h"

#include <RcppArmadillo.h>
#define ARMA_64BIT_WORD

#include <RcppThread.h>

using namespace Rcpp;


//' Sample a DTM or TCM based on hierarchical multinomial parameters
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
//' @return Returns a sparse matrix whose rows represent documents, columns
//'   represent tokens and i,j entries are the count of token j in document i.
//' @examples
//' # TODO
//' @export
// [[Rcpp::export]]
arma::sp_mat sample_documents(
  const NumericMatrix theta,
  const NumericMatrix phi,
  const std::vector<std::size_t> doc_lengths,
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
  // out.fill(0);

  // parallel loop over documents to fill in counts
  RcppThread::parallelFor(
    0,
    Nd,
    [&](std::size_t d) {

      // initialize a couple variables
      arma::uword z; // placeholder for topic sampled

      arma::uword w; // placeholder for token sampled


      // loop for that document's length
      for (std::size_t n = 0; n < doc_lengths[d]; n++) {

        // sample a topic, z
        z = samp_one(theta_list[d]);

        // given z, sample a token w
        w = samp_one(phi_list[z]);

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


