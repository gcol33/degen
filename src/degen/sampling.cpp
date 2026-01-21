#include "sampling.h"
#include <limits>
#include <numeric>

namespace degen {

std::vector<double> lhs_sample(int n, int k, std::mt19937& rng) {
  if (n <= 0 || k <= 0) {
    return std::vector<double>();
  }

  std::vector<double> result(n * k);
  std::uniform_real_distribution<double> unif(0.0, 1.0);

  for (int j = 0; j < k; ++j) {
    // Create stratified samples for this dimension
    std::vector<double> samples(n);
    double interval_width = 1.0 / n;

    for (int i = 0; i < n; ++i) {
      double lower = i * interval_width;
      double upper = (i + 1) * interval_width;
      samples[i] = lower + unif(rng) * (upper - lower);
    }

    // Shuffle to break correlation between dimensions
    std::shuffle(samples.begin(), samples.end(), rng);

    // Store in result matrix (row-major)
    for (int i = 0; i < n; ++i) {
      result[i * k + j] = samples[i];
    }
  }

  return result;
}

std::vector<double> random_sample(int n, int k, std::mt19937& rng) {
  if (n <= 0 || k <= 0) {
    return std::vector<double>();
  }

  std::vector<double> result(n * k);
  std::uniform_real_distribution<double> unif(0.0, 1.0);

  for (int i = 0; i < n * k; ++i) {
    result[i] = unif(rng);
  }

  return result;
}

void transform_to_bounds(std::vector<double>& samples, int n, int k,
                         const std::vector<double>& lower,
                         const std::vector<double>& upper) {
  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < k; ++j) {
      double u = samples[i * k + j];
      samples[i * k + j] = lower[j] + u * (upper[j] - lower[j]);
    }
  }
}

std::pair<double, double> effective_bounds(double lower, double upper,
                                           double scale) {
  bool lower_finite = std::isfinite(lower);
  bool upper_finite = std::isfinite(upper);

  if (lower_finite && upper_finite) {
    return {lower, upper};
  }

  if (lower_finite && !upper_finite) {
    return {lower, lower + scale};
  }

  if (!lower_finite && upper_finite) {
    return {upper - scale, upper};
  }

  // Both infinite
  return {-scale, scale};
}

}  // namespace degen
