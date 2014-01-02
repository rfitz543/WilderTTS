#include <Rcpp.h>
#include <numeric>      // for accumulate

using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
NumericVector run_sum(NumericVector x, int n) 
{
    int sz = x.size();
    NumericVector res(sz);
    
    res[n-1] = std::accumulate(x.begin(), x.end()-sz+n, 0.0);
    
    for(int i = n; i < sz; i++) 
    {
       res[i] = res[i-1] + x[i] - x[i-n];
    }
    // pad the first n-1 elements with NA
    std::fill(res.begin(), res.end()-sz+n-1, NA_REAL);
    return res;
}

// [[Rcpp::export]]
NumericVector run_mean(NumericVector x, int n) {
    return run_sum(x, n) / (double)n;
}

// [[Rcpp::export]]
NumericVector run_min(NumericVector x, int n)
{
    int sz = x.size();
    NumericVector res(sz);
    
    for(int i = 0; i < (sz-n+1); i++)
    {
        res[i+n-1] = *std::min_element(x.begin() + i, x.end() - sz + n + i);
        cout << i << endl;
    }
    // pad the first n-1 elements with NA
    std::fill(res.begin(), res.end()-sz+n-1, NA_REAL);
    return res;
}

// [[Rcpp::export]]
NumericVector run_max(NumericVector x, int n)
{
    int sz = x.size();
    NumericVector res(sz);
    for(int i = 0; i < (sz-n+1); i++)
    {
        res[i+n-1] = *std::max_element(x.begin() + i, x.end() - sz + n + i);
    }
    // pad the first n-1 elements with NA
    std::fill(res.begin(), res.end()-sz+n-1, NA_REAL);
    return res;
}

// [[Rcpp:export]]
NumericVector true_range(NumericMatrix x)
{
  int sz = x.nrow();
  int hi = 1;
  int lo = 2;
  int cl = 3;
  
  NumericVector vec(sz);
  vec[0] = x(0,hi)-x(0,lo);
  for(int i=1; i<sz; i++)
  {
    vec[i] = std::max(x(i,hi)-x(i,lo),std::max(std::abs(x(i,hi)-x(i-1,cl)),
    std::abs(x(i,lo)-x(i-1,cl))));
  }
  return vec;
}
