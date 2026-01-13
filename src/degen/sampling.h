#ifndef DEGEN_SAMPLING_H
#define DEGEN_SAMPLING_H

#include <vector>
#include <random>
#include <algorithm>
#include <cmath>

namespace degen {

// Latin hypercube sampling in unit hypercube [0,1]^k
// Returns n x k matrix (row-major: result[i * k + j] = sample i, dimension j)
std::vector<double> lhs_sample(int n, int k, std::mt19937& rng);

// Random uniform sampling in unit hypercube [0,1]^k
std::vector<double> random_sample(int n, int k, std::mt19937& rng);

// Transform unit hypercube samples to parameter space
// samples: n x k matrix (row-major)
// lower, upper: k-length vectors of bounds
void transform_to_bounds(std::vector<double>& samples, int n, int k,
                         const std::vector<double>& lower,
                         const std::vector<double>& upper);

// Compute effective bounds (handle infinite values)
// Returns pair of (effective_lower, effective_upper)
std::pair<double, double> effective_bounds(double lower, double upper,
                                           double scale = 10.0);

}  // namespace degen

#endif  // DEGEN_SAMPLING_H
