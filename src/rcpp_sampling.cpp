#include <Rcpp.h>
#include "degen/sampling.h"
#include <algorithm>

// LHS sampling using R's RNG (respects set.seed)
// [[Rcpp::export(.lhs_sample_cpp)]]
Rcpp::NumericMatrix lhs_sample_cpp(int n, int k) {
  Rcpp::NumericMatrix result(n, k);

  for (int j = 0; j < k; ++j) {
    // Create stratified samples for this dimension
    std::vector<double> samples(n);
    double interval_width = 1.0 / n;

    for (int i = 0; i < n; ++i) {
      double lo = i * interval_width;
      double hi = (i + 1) * interval_width;
      samples[i] = R::runif(lo, hi);
    }

    // Fisher-Yates shuffle using R's RNG
    for (int i = n - 1; i > 0; --i) {
      int j_swap = static_cast<int>(R::runif(0, i + 1));
      if (j_swap > i) j_swap = i;  // guard against edge case
      std::swap(samples[i], samples[j_swap]);
    }

    for (int i = 0; i < n; ++i) {
      result(i, j) = samples[i];
    }
  }

  return result;
}

// Random sampling using R's RNG
// [[Rcpp::export(.random_sample_cpp)]]
Rcpp::NumericMatrix random_sample_cpp(int n, int k) {
  Rcpp::NumericMatrix result(n, k);
  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < k; ++j) {
      result(i, j) = R::runif(0.0, 1.0);
    }
  }
  return result;
}

// [[Rcpp::export(.sample_par_space_cpp)]]
Rcpp::NumericMatrix sample_par_space_cpp(Rcpp::NumericVector lower,
                                         Rcpp::NumericVector upper,
                                         int n,
                                         bool lhs) {
  int k = lower.size();

  // Sample in unit hypercube using R's RNG
  Rcpp::NumericMatrix samples = lhs ? lhs_sample_cpp(n, k)
                                    : random_sample_cpp(n, k);

  // Transform to parameter space with effective bounds
  for (int j = 0; j < k; ++j) {
    auto eff = degen::effective_bounds(lower[j], upper[j], 10.0);
    double lo = eff.first;
    double hi = eff.second;
    for (int i = 0; i < n; ++i) {
      samples(i, j) = lo + samples(i, j) * (hi - lo);
    }
  }

  return samples;
}

// [[Rcpp::export(.effective_bounds_cpp)]]
Rcpp::NumericVector effective_bounds_cpp(double lower, double upper,
                                         double scale = 10.0) {
  auto result = degen::effective_bounds(lower, upper, scale);
  return Rcpp::NumericVector::create(result.first, result.second);
}
