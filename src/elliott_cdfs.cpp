#include <Rcpp.h>

using Rcpp::NumericMatrix;
using Rcpp::NumericVector;

namespace {
struct LcmWorkspace {
  std::vector<double> w;
  std::vector<double> b_values;
  std::vector<int> knots;
};

inline double compute_lcm_sup(const std::vector<double> &b_values,
                              int grid_points,
                              std::vector<int> &knots) {
  const int n = grid_points;
  const int n_vals = n + 1;

  knots.clear();
  knots.reserve(n_vals);

  for (int i = 0; i < n_vals; ++i) {
    knots.push_back(i);
    while (knots.size() >= 3) {
      const size_t m = knots.size();
      const int k1 = knots[m - 3];
      const int k2 = knots[m - 2];
      const int k3 = knots[m - 1];

      const double slope1 = (b_values[k2] - b_values[k1]) / (k2 - k1);
      const double slope2 = (b_values[k3] - b_values[k2]) / (k3 - k2);

      if (slope1 < slope2) {
        knots.erase(knots.end() - 2);
      } else {
        break;
      }
    }
  }

  const double inv_n = 1.0 / static_cast<double>(n);
  double max_abs = 0.0;

  for (size_t s = 1; s < knots.size(); ++s) {
    const int i1 = knots[s - 1];
    const int i2 = knots[s];
    if (i2 <= i1) {
      continue;
    }
    const double y1 = b_values[i1];
    const double y2 = b_values[i2];
    const double slope = (y2 - y1) * static_cast<double>(n) / (i2 - i1);
    const double intercept = y2 - slope * (static_cast<double>(i2) * inv_n);

    for (int r = i1; r <= i2 - 1; ++r) {
      const double x = (static_cast<double>(r) + 1.0) * inv_n;
      const double y_line = intercept + slope * x;
      const double diff = y_line - b_values[r];
      const double absdiff = diff < 0.0 ? -diff : diff;
      if (absdiff > max_abs) {
        max_abs = absdiff;
      }
    }
  }

  return max_abs;
}
} // namespace

// [[Rcpp::export]]
NumericVector simulate_cdfs_cpp(int iterations, int grid_points) {
  if (iterations <= 0 || grid_points <= 0) {
    return NumericVector(0);
  }

  Rcpp::RNGScope scope;

  const int n = grid_points;
  const int n_vals = n + 1;
  const double inv_sqrt_n = 1.0 / std::sqrt(static_cast<double>(n));
  const double inv_n = 1.0 / static_cast<double>(n);

  NumericVector out(iterations);
  LcmWorkspace ws;
  ws.w.assign(n, 0.0);
  ws.b_values.assign(n_vals, 0.0);
  ws.knots.reserve(n_vals);

  for (int m = 0; m < iterations; ++m) {
    double cum = 0.0;
    for (int i = 0; i < n; ++i) {
      cum += R::rnorm(0.0, 1.0) * inv_sqrt_n;
      ws.w[i] = cum;
    }

    const double w_end = ws.w[n - 1];
    ws.b_values[0] = 0.0;
    for (int i = 0; i < n; ++i) {
      ws.b_values[i + 1] = ws.w[i] - (static_cast<double>(i + 1) * inv_n) * w_end;
    }

    out[m] = compute_lcm_sup(ws.b_values, n, ws.knots);
  }

  return out;
}

// [[Rcpp::export]]
NumericVector simulate_cdfs_block_cpp(const NumericMatrix &eps_block) {
  const int n = eps_block.nrow();
  const int k = eps_block.ncol();

  if (n <= 0 || k <= 0) {
    return NumericVector(0);
  }

  NumericVector out(k);
  LcmWorkspace ws;
  ws.w.assign(n, 0.0);
  ws.b_values.assign(n + 1, 0.0);
  ws.knots.reserve(n + 1);

  const double inv_n = 1.0 / static_cast<double>(n);

  for (int col = 0; col < k; ++col) {
    double cum = 0.0;
    for (int i = 0; i < n; ++i) {
      cum += eps_block(i, col);
      ws.w[i] = cum;
    }

    const double w_end = ws.w[n - 1];
    ws.b_values[0] = 0.0;
    for (int i = 0; i < n; ++i) {
      ws.b_values[i + 1] = ws.w[i] - (static_cast<double>(i + 1) * inv_n) * w_end;
    }

    out[col] = compute_lcm_sup(ws.b_values, n, ws.knots);
  }

  return out;
}
