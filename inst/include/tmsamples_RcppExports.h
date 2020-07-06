// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#ifndef RCPP_tmsamples_RCPPEXPORTS_H_GEN_
#define RCPP_tmsamples_RCPPEXPORTS_H_GEN_

#include <RcppArmadillo.h>
#include <Rcpp.h>

namespace tmsamples {

    using namespace Rcpp;

    namespace {
        void validateSignature(const char* sig) {
            Rcpp::Function require = Rcpp::Environment::base_env()["require"];
            require("tmsamples", Rcpp::Named("quietly") = true);
            typedef int(*Ptr_validate)(const char*);
            static Ptr_validate p_validate = (Ptr_validate)
                R_GetCCallable("tmsamples", "_tmsamples_RcppExport_validate");
            if (!p_validate(sig)) {
                throw Rcpp::function_not_exported(
                    "C++ function with signature '" + std::string(sig) + "' not found in tmsamples");
            }
        }
    }

    inline arma::sp_mat sample_documents(const NumericMatrix theta, const NumericMatrix phi, const std::vector<std::size_t> doc_lengths, std::size_t threads = 1) {
        typedef SEXP(*Ptr_sample_documents)(SEXP,SEXP,SEXP,SEXP);
        static Ptr_sample_documents p_sample_documents = NULL;
        if (p_sample_documents == NULL) {
            validateSignature("arma::sp_mat(*sample_documents)(const NumericMatrix,const NumericMatrix,const std::vector<std::size_t>,std::size_t)");
            p_sample_documents = (Ptr_sample_documents)R_GetCCallable("tmsamples", "_tmsamples_sample_documents");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_sample_documents(Shield<SEXP>(Rcpp::wrap(theta)), Shield<SEXP>(Rcpp::wrap(phi)), Shield<SEXP>(Rcpp::wrap(doc_lengths)), Shield<SEXP>(Rcpp::wrap(threads)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<arma::sp_mat >(rcpp_result_gen);
    }

}

#endif // RCPP_tmsamples_RCPPEXPORTS_H_GEN_
