#pragma once

#include <algorithm>
#include <numeric>

// make a categorical struct that maps a category to its probability
// function below makes a vector of categoricals
// Need to sort based on probabilities. This is more easily done in a struct
struct categorical
{
  int category;
  double probability;
  
  // declare constructor
  categorical(){};
  
  // define constructor
  categorical(
    int init_cat, 
    double init_prob
  ) {
    category = init_cat;
    probability = init_prob;
  }
};

// vector of categorical variables for sorting
typedef std::vector<categorical> categorical_variable;


// The function below does not use R's random number generator.
// To use it, you need to put it in a wrapper that (a) reads from R's 
// RNG stream and passes it to the seed argument, so that users can use 
// set.seed() and (b) advances the position
// of the RNG stream after the C++ portion is over
int sample_one(
    const std::vector<double> &p, // vector of probabilities inexing categories 0 to k
    double                    &u  // number between 0 and 1 for comparison to cumulative probabilities
) {
  
  const std::size_t Nk = p.size();
  
  // check inputs
  if (std::any_of(p.begin(), p.end(), [](double j){ return j < 0.0;})) {
    throw std::runtime_error("Negative probabilities not allowed"); 
  }
  
  // construct categorical that combines index and probability
  // while we're at it, make sure probability is normalized
  double p_sum = std::accumulate(p.begin(), p.end(), 0.0);
  
  categorical_variable cat_var;
  
  for (auto k = 0; k < Nk; k++) {
    cat_var.push_back(categorical(k, p[k] / p_sum));
  }
  
  // sort our categorical from highest probability to lowest probability
  std::sort(cat_var.begin(), cat_var.end(), 
            [](categorical const &a, categorical const &b) { 
              return a.probability < b.probability; 
            }); 
  
  // generate a q vector of cumulate probabilities
  std::vector<double> q(Nk);
  
  q[0] = cat_var[0].probability;
  
  for (auto k = 1; k < Nk; k++) {
    q[k] = cat_var[k].probability + q[k - 1];
  }
  
  // compare to u and return the selected value
  for (auto k = 0; k < Nk; k++) {
    if (u <= q[k])
      return cat_var[k].category;
  }
  
  throw std::runtime_error("couldn't find index (samp_one)");
}